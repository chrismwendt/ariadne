{-# LANGUAGE ViewPatterns, OverloadedStrings #-}
module Main where

import Ariadne.GlobalNameIndex
import Ariadne.Index
import Ariadne.Types
import qualified Ariadne.SrcMap as SrcMap
import Ariadne.ModuleDB

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

main = do
  lookupEnv "ARIADNE_DEBUG" >>= \v ->
    when (isJust v) $ do
      logger <- L.getRootLogger
      L.updateGlobalLogger L.rootLoggerName (L.setLevel L.DEBUG)

  L.debugM "ariadne" "Ariadne started"

  t <- tcpServer 39014

  withModuleDB $ \mdb -> serve t (dispatch mdb)

  where
    dispatch mdb mod fn args = do
      L.debugM "ariadne.server" $
        printf "request: %s %s %s" mod fn (show args)
      response <- handle mdb mod fn args
      L.debugM "ariadne.server" $
        printf "response: %s" (show response)
      return response
    handle mdb "ariadne" "find" [BinaryTerm file, IntTerm line, IntTerm col] = do
      let filestr = UTF8.toString file
      sendRequestSync mdb (Include filestr)
      answer mdb filestr line col >>= \result -> return . Success $
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
    handle _ _ _ _ = return NoSuchFunction
