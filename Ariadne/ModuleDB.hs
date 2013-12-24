{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module Ariadne.ModuleDB
  ( sendRequestAsync
  , sendRequestSync
  , Request(..)
  , answer
  , withModuleDB
  )
  where

import Ariadne.Types
import qualified Ariadne.SrcMap as SrcMap
import Ariadne.ModuleDB.Types
import Ariadne.ModuleDB.ParseResolve

import Language.Haskell.Names
import Language.Haskell.Exts.Annotated hiding (parse)

import Control.Monad.Trans
import Control.Monad
import Control.Exception
import Control.Monad.State
import Text.Printf
import System.Directory
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified System.Log.Logger as L
import Data.Maybe
import Data.Proxy

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM

import Data.Lens

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
