{-# LANGUAGE TupleSections, TypeFamilies #-}
-- | Construction of the global name index. See 'GlobalNameIndex' for the
-- description.
module Ariadne.GlobalNameIndex (mkGlobalNameIndex) where

import Language.Haskell.Names
import qualified Language.Haskell.Names.GlobalSymbolTable as Global
import Language.Haskell.Names.SyntaxUtils
import Language.Haskell.Names.GetBound
import Language.Haskell.Exts.Annotated
import Distribution.HaskellSuite.Modules
import qualified Data.Map as Map
import Data.Maybe

import Ariadne.Types

-- | Create the global name index for a given module.
--
-- This function assumes that the module's import table is calculated
-- somewhere outside (probably using 'processImports').

-- Why we don't calculate the import table right here? First, it would
-- bring all the complexity of ModuleT here. Second, what about recursive
-- modules? Third, this avoids double cost of resolution if it's needed
-- somewhere else too.
mkGlobalNameIndex
  :: Global.Table -> Module SrcLoc -> GlobalNameIndex
mkGlobalNameIndex tbl mod =
  let
    Module _ _ _ _ ds = mod
    ModuleName _ modname = getModuleName mod

    names = concatMap (indexDecl tbl) ds

  in
    Map.fromListWith (const id) -- prefer earlier names
      [ ((OrigName Nothing (GName modname (nameToString n)), level), ann n)
      | (n, level) <- names
      ]

indexDecl :: Global.Table -> Decl SrcLoc -> [(Name SrcLoc, NameLevel)]
indexDecl tbl d =
  case d of
    TypeDecl _ dh _ -> [(getDeclHeadName dh, TypeLevel)]
    TypeFamDecl _ dh _ -> [(getDeclHeadName dh, TypeLevel)]

    DataDecl _ _ _ dh qualConDecls _ ->
      ((getDeclHeadName dh, TypeLevel) :) . map (, ValueLevel) $ do -- list monad

      QualConDecl _ _ _ conDecl <- qualConDecls
      case conDecl of
        ConDecl _ n _ -> return n
        InfixConDecl _ _ n _ -> return n
        RecDecl _ n fields ->
          n :
          [f | FieldDecl _ fNames _ <- fields, f <- fNames]

    GDataDecl _ dataOrNew _ dh _ gadtDecls _ ->
      -- As of 1.14.0, HSE doesn't support GADT records.
      -- When it does, this code should be rewritten similarly to the
      -- DataDecl case.
      -- (Also keep in mind that GHC doesn't create selectors for fields
      -- with existential type variables.)
      -- TODO HSE 1.16 now supports GADT records
          (getDeclHeadName dh, TypeLevel) :
        [ (cn, ValueLevel)
        | GadtDecl _ cn names _ <- gadtDecls
        ]

    ClassDecl _ _ dh _ mds ->
      let
        ms = getBound tbl d
        cdecls = fromMaybe [] mds
      in
          (getDeclHeadName dh, TypeLevel) :
        [ (getDeclHeadName dh, TypeLevel) | ClsTyFam   _   dh _ <- cdecls ] ++
        [ (getDeclHeadName dh, TypeLevel) | ClsDataFam _ _ dh _ <- cdecls ] ++
        [ (mn, ValueLevel) | mn <- ms ]

    FunBind _ ms -> map (, ValueLevel) $ getBound tbl ms

    PatBind _ p _ _ _ -> map (, ValueLevel) $ getBound tbl p

    ForImp _ _ _ _ fn _ -> [(fn, ValueLevel)]

    _ -> []
