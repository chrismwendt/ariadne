{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
module Ariadne.ModuleDB.ParseResolve where

import Ariadne.GlobalNameIndex
import Ariadne.Index
import Ariadne.Types
import qualified Ariadne.SrcMap as SrcMap
import Ariadne.ModuleDB.Types


import Language.Haskell.Names
import Language.Haskell.Names.Interfaces
import Language.Haskell.Names.SyntaxUtils
import Language.Haskell.Names.Imports
import qualified Language.Haskell.Names.GlobalSymbolTable as Global
import Language.Haskell.Exts.Annotated hiding (parse)
import Language.Haskell.Exts.Annotated.CPP
import Distribution.HaskellSuite.Packages
import Distribution.Simple.Compiler (PackageDB(..))

import Control.Applicative
import Control.Arrow
import Control.Monad.Trans
import Control.Monad
import Control.Monad.State
import Text.Printf
import System.FilePath
import System.Directory
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.Log.Logger as L
import Data.Maybe
import qualified Data.Foldable as F
import Data.Proxy
import Data.Lens

include :: (MonadState Storage m, MonadIO m) => FilePath -> m ()
include path = do
  alreadyPresent <- gets (Set.member path . getL watchedFiles)
  unless alreadyPresent $ do
    liftIO . L.debugM "ariadne.moduledb" $ printf "Including %s in the set of watched files" path
    exists <- liftIO $ doesFileExist path
    watchedFiles %= Set.insert path
    when exists $ update path

update
  :: (MonadState Storage m, MonadIO m)
  => FilePath -- the file to update
  -> m ()
update path = do
  readSources path

  (paths, sources) <- (Map.keys &&& Map.elems) `liftM` access moduleSources

  pkgs <- liftIO $
   ((F.fold <$> mapM (getInstalledPackages (Proxy :: Proxy NamesDB)) [GlobalPackageDB, UserPackageDB])
    :: IO Packages)

  (resolved, impTbls) <- liftIO (liftM unzip $
   (flip evalNamesModuleT pkgs $ do
      errs <- computeInterfaces defaultLang defaultExts sources
      forM sources $ \parsed -> do
        let extSet = moduleExtensions defaultLang defaultExts parsed
        impTbl <- fmap snd $ processImports extSet $ getImports parsed
        resolved <- annotateModule defaultLang defaultExts parsed
        return (resolved, impTbl))
    :: IO ([Module (Scoped SrcSpan)], [Global.Table]))

  let
    gIndex :: GlobalNameIndex
    gIndex = Map.unions $
      zipWith
        (\src impTbl -> mkGlobalNameIndex impTbl (getPointLoc <$> src))
        sources impTbls

    srcMaps :: [SrcMap.SrcMap Origin]
    srcMaps = map (mkSrcMap gIndex) resolved

  moduleSrcMaps ~= Map.fromAscList (zip paths srcMaps)

  return ()

readSources
  :: (MonadState Storage m, MonadIO m)
  => FilePath -- the file to update
  -> m ()
readSources path = do
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
          let
            modname@(ModuleName _ modnameS) = getModuleName parsed
            root = rootPath path modname
          liftIO . L.debugM "ariadne.parser" $
            printf "Parsed %s at %s" modnameS path
          moduleSources %= Map.insert path (srcInfoSpan <$> parsed)
          mapM_ (include . modNameToPath root) (importedModules parsed)

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
