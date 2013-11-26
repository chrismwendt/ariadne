{-# LANGUAGE TemplateHaskell, FlexibleContexts, MultiParamTypeClasses #-}

module Ariadne.ModuleDB
  ( sendRequestAsync
  , sendRequestSync
  , Request(..)
  , answer
  , withModuleDB
  )
  where

import Ariadne.GlobalNameIndex
import Ariadne.Index
import Ariadne.Types
import qualified Ariadne.SrcMap as SrcMap

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
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Exception
import Control.Monad.State
import Text.Printf
import System.FilePath
import System.Directory
import System.Environment
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified System.Log.Logger as L
import Data.Maybe
import qualified Data.Foldable as F
import Data.Proxy
import Data.Monoid

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.Async
import Control.Concurrent.STM

import Data.Lens
import Data.Lens.Template

data Request
  = Include FilePath
  | Update FilePath

type Callback = IO ()

-- ModuleDB's moduleData should only be modified in the ModuleDB's own
-- thread. Outside, it can only be read. This is inforced by the module
-- system.
data ModuleDB = ModuleDB
  { requestChan :: Chan (Request, Callback)
  , storage :: TVar Storage
  }

data Storage = Storage
  { _watchedFiles :: Set.Set FilePath
  , _moduleSources :: Map.Map FilePath (Module SrcSpan)
  , _moduleSrcMaps :: Map.Map FilePath (SrcMap.SrcMap Origin)
  }

emptyStorage = Storage mempty mempty mempty

makeLenses [''Storage]

-- | Send a request and wait for its completion
sendRequestSync :: ModuleDB -> Request -> IO ()
sendRequestSync (ModuleDB { requestChan = chan }) req = do
  mvar <- newEmptyMVar
  let cb = putMVar mvar ()
  writeChan chan (req, cb)
  takeMVar mvar

sendRequestAsync :: ModuleDB -> Request -> IO ()
sendRequestAsync (ModuleDB { requestChan = chan }) req =
  writeChan chan (req, return ())

withModuleDB :: (ModuleDB -> IO a) -> IO a
withModuleDB act = do
  chan <- newChan
  storageV <- atomically $ newTVar emptyStorage
  withAsync (respond chan storageV) $ \asy ->
    act $ ModuleDB { requestChan = chan, storage = storageV }

respond :: Chan (Request, Callback) -> TVar Storage -> IO ()
respond chan storageV = forever $ do
  storage <- atomically $ readTVar storageV
  (req, cb) <- readChan chan
  storage' <- execStateT (actOn req) storage
  atomically $ writeTVar storageV storage'
  cb

actOn :: Request -> StateT Storage IO ()
actOn (Include path) = include path
actOn (Update path) = update path

include :: (MonadState Storage m, MonadIO m) => FilePath -> m ()
include path = do  
  exists <- liftIO $ doesFileExist path
  if not exists
    -- XXX should we watch for non-existent paths?
    -- Find out whether the OSes support that
    then return ()
    else do
      path <- liftIO $ canonicalizePath path
      alreadyPresent <- gets (Set.member path . getL watchedFiles)
      if alreadyPresent
        then
          -- No reason to assume that the file has changed.
          -- Actually, this is a slight abuse of the watch set — we rely
          -- on the invariant that when we include the path we also update
          -- it.
          return ()
        else do
          liftIO . L.debugM "ariadne.moduledb" $ printf "Including %s in the set of watched files" path
          watchedFiles %= Set.insert path
          update path
          return ()

update
  :: (MonadState Storage m, MonadIO m)
  => FilePath -- the file to update
  -> m ()
update path = do
  readSources Nothing path

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
  => Maybe FilePath -- root path, if known
  -> FilePath -- the file to update
  -> m ()
readSources mbRoot path = do
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
            root = fromMaybe (rootPath path modname) mbRoot
          liftIO . L.debugM "ariadne.parser" $
            printf "Parsed %s at %s" modnameS path
          moduleSources %= Map.insert path (srcInfoSpan <$> parsed)
          mapM_ (readSources (Just root) . modNameToPath root) (importedModules parsed)

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

getSrcMap :: ModuleDB -> FilePath -> IO (Maybe (SrcMap.SrcMap Origin))
getSrcMap (ModuleDB { storage = storageV }) path = do
  storage <- atomically $ readTVar storageV
  return $ Map.lookup path $ storage ^. moduleSrcMaps

answer :: ModuleDB -> String -> Int -> Int -> IO (Maybe Origin)
answer moduleDB path line col = do
  mbSrcMap <- getSrcMap moduleDB path
  return $
    maybe
      (Just $ ResolveError "No information about this module")
      (SrcMap.lookup noLoc { srcLine = line, srcColumn = col })
      mbSrcMap
