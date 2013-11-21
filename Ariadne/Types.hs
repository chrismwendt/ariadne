module Ariadne.Types where

import Language.Haskell.Names
import qualified Language.Haskell.Names.GlobalSymbolTable as Global
import Language.Haskell.Exts.Annotated
import Distribution.HaskellSuite.Packages
import qualified Distribution.ModuleName as Cabal
import qualified Data.Map as Map

import qualified Ariadne.SrcMap as SrcMap

data NameLevel = TypeLevel | ValueLevel
  deriving (Ord, Eq, Show, Enum, Bounded)

-- | Global name index records the correspondence between global names and
-- their definition sites. Not every available global name is necessarily
-- in the index.
type GlobalNameIndex = Map.Map (OrigName, NameLevel) SrcLoc

-- | Data about a module for which we have source code
data ModuleData = ModuleData
  { modulePath :: FilePath
  , moduleSource :: Module SrcSpan
  , moduleSymbols :: Symbols
  , moduleImpTbl :: Global.Table
  , moduleResolved :: Module (Scoped SrcSpanInfo)
  , moduleGIndex :: GlobalNameIndex
  , moduleSrcMap :: SrcMap.SrcMap Origin
  }

data Origin
  = LocKnown SrcLoc
  | LocUnknown ModuleNameS
  | ResolveError String
  deriving Show
