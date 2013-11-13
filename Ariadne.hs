{-# LANGUAGE ViewPatterns, OverloadedStrings #-}
module Main where

import Ariadne.GlobalNameIndex
import Ariadne.Index
import qualified Ariadne.SrcMap as SrcMap

import Language.Haskell.Names
import Language.Haskell.Names.Interfaces
import Language.Haskell.Names.SyntaxUtils
import Language.Haskell.Names.Imports
import Language.Haskell.Exts.Annotated hiding (parse)
import Language.Haskell.Exts.Annotated.CPP
import Distribution.HaskellSuite.Packages
import Distribution.Simple.Compiler (PackageDB(..))

import Control.Applicative
import Control.Monad.Trans
import Control.Monad
import Control.Exception
import Control.Monad.State
import Text.Printf
import System.FilePath
import System.Directory
import System.Environment
import qualified Data.Map as Map
import qualified System.Log.Logger as L
import Data.Maybe
import qualified Data.Foldable as F
import Data.Proxy

import Data.BERT
import Network.BERT.Server
import Network.BERT.Transport
import qualified Data.ByteString.Lazy.UTF8 as UTF8

-- these should probably come from the Cabal file
defaultLang = Haskell2010
defaultExts = []

parse :: FilePath -> IO (ParseResult (Module SrcSpanInfo))
parse path =
  fmap fst <$>
  parseFileWithCommentsAndCPP
    defaultCpphsOptions
    defaultParseMode { parseFilename = path, ignoreLinePragmas = False }
    path

-- | Get the module's root path, based on its path and the module name
rootPath :: FilePath -> ModuleName l -> FilePath
rootPath path (ModuleName _ modname) =
  -- the algorithm is simple: count the number of components in the module
  -- name, and go that number of levels up
  let
    numLevels = length $ filter (== '.') modname
    root = (foldr (.) id $ replicate (numLevels+1) takeDirectory) path
  in root

-- FIXME support lhs etc.
modNameToPath
  :: FilePath -- ^ root path
  -> ModuleNameS -- ^ module name
  -> FilePath -- ^ module path
modNameToPath root name = root </> map dotToSlash name <.> "hs"
  where
    dotToSlash '.' = '/'
    dotToSlash c = c

importedModules :: Module a -> [ModuleNameS]
importedModules mod =
  map ((\(ModuleName _ s) -> s) . importModule) $ getImports mod

collectModules
  :: FilePath
  -> ModuleNameS
  -> StateT (Map.Map ModuleNameS (Module SrcSpanInfo)) IO ()
collectModules root modname = do
  alreadyPresent <- gets $ Map.member modname
  if alreadyPresent
    then return ()
    else do
      let path = modNameToPath root modname
      exists <- liftIO $ doesFileExist path
      if not exists
        then do
          {-liftIO . L.debugM "ariadne.parser" $
            printf "%s: not found at %s" modname path-}
          return ()
        else do
          parseResult <- liftIO $ parse path
          case parseResult of
            ParseFailed loc msg -> do
              liftIO . L.warningM "ariadne.parser" $
                printf "Failed to parse %s (%s: %s)" path (prettyPrint loc) msg
              return ()
            ParseOk parsed -> do
              liftIO . L.debugM "ariadne.parser" $
                printf "Parsed %s at %s" modname path
              modify $ Map.insert modname parsed
              mapM_ (collectModules root) (importedModules parsed)

work :: String -> Int -> Int -> IO (Maybe Origin)
work path line col = handleExceptions $ do
  parseResult <- parse path

  case parseResult of
    ParseFailed loc msg ->
      return $ Just $ ResolveError $ printf "%s: %s" (prettyPrint loc) msg

    ParseOk parsed -> do
      let
        modname@(ModuleName _ modnameS) = getModuleName parsed
        root = rootPath path modname

      sources <-
        liftM Map.elems $
        flip execStateT (Map.singleton modnameS parsed) $
          mapM_ (collectModules root) (importedModules parsed)

      pkgs <- F.fold <$> mapM (getInstalledPackages (Proxy :: Proxy NamesDB)) [GlobalPackageDB, UserPackageDB]
      (resolved, impTbls) <-
        flip evalNamesModuleT pkgs $ do
          errs <- computeInterfaces defaultLang defaultExts sources
          impTbls <- forM sources $ \parsed -> do
            let extSet = moduleExtensions defaultLang defaultExts parsed
            fmap snd $ processImports extSet $ getImports parsed
          resolved <- annotateModule defaultLang defaultExts parsed
          return (resolved, impTbls)
      let
        gIndex = Map.unions $
          zipWith
            (\src impTbl -> mkGlobalNameIndex impTbl (getPointLoc <$> src))
            sources impTbls
        srcMap = mkSrcMap gIndex (fmap srcInfoSpan <$> resolved)

      return $ SrcMap.lookup noLoc { srcLine = line, srcColumn = col } srcMap
  where
    handleExceptions a =
      try (a >>= evaluate) >>= either (\e -> return $ Just $ ResolveError $ show (e::SomeException)) return

main = do
  lookupEnv "ARIADNE_DEBUG" >>= \v ->
    when (isJust v) $ do
      logger <- L.getRootLogger
      L.updateGlobalLogger L.rootLoggerName (L.setLevel L.DEBUG)

  t <- tcpServer 39014
  serve t dispatch
  where
    dispatch mod fn args = do
      L.debugM "ariadne.server" $
        printf "request: %s %s %s" mod fn (show args)
      response <- handle mod fn args
      L.debugM "ariadne.server" $
        printf "response: %s" (show response)
      return response
    handle "ariadne" "find" [BinaryTerm file, IntTerm line, IntTerm col] = do
      work (UTF8.toString file) line col >>= \result -> return . Success $
        case result of
          Nothing -> TupleTerm [AtomTerm "no_name"]
          Just (LocKnown (SrcLoc file' line' col')) ->
            TupleTerm
              [ AtomTerm "loc_known"
              , BinaryTerm (UTF8.fromString file')
              , IntTerm line'
              , IntTerm col'
              ]
          Just (LocUnknown modName) ->
            TupleTerm
              [ AtomTerm "loc_unknown"
              , BinaryTerm (UTF8.fromString modName)
              ]
          Just (ResolveError er) ->
            TupleTerm
              [ AtomTerm "error"
              , BinaryTerm (UTF8.fromString er)
              ]
    handle _ _ _ = return NoSuchFunction
