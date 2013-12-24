{-# LANGUAGE TemplateHaskell #-}
module Ariadne.ModuleDB.Types where

import qualified Ariadne.SrcMap as SrcMap
import Ariadne.Types

import Control.Concurrent
import Control.Concurrent.STM
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Lens.Template
import Language.Haskell.Exts.Annotated hiding (parse)

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
