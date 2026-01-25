module Language.WeakTerm.Holes (holesType) where

import Control.Comonad.Cofree
import Data.Set qualified as S
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Binder
import Language.Common.HoleID
import Language.WeakTerm.WeakTerm qualified as WT

holesType :: WT.WeakType -> S.Set HoleID
holesType ty =
  case ty of
    _ :< WT.Tau ->
      S.empty
    _ :< WT.TVar {} ->
      S.empty
    _ :< WT.TVarGlobal {} ->
      S.empty
    _ :< WT.TyApp t args ->
      S.unions $ holesType t : map holesType args
    _ :< WT.Pi _ impArgs expArgs defaultArgs t ->
      holesBindersType (impArgs ++ expArgs ++ defaultArgs) (holesType t)
    _ :< WT.Data attr _ es -> do
      let xs1 = S.unions $ map holesType es
      let xs2 = holesAttrData attr
      S.union xs1 xs2
    _ :< WT.Box t ->
      holesType t
    _ :< WT.BoxNoema t ->
      holesType t
    _ :< WT.Code t ->
      holesType t
    _ :< WT.PrimType {} ->
      S.empty
    _ :< WT.Void ->
      S.empty
    _ :< WT.Resource _ _ -> do
      S.empty
    _ :< WT.TypeHole h es ->
      S.insert h $ S.unions $ map holesType es

holesBindersType :: [BinderF WT.WeakType] -> S.Set HoleID -> S.Set HoleID
holesBindersType binder zs =
  case binder of
    [] ->
      zs
    ((_, _, _, t) : xts) -> do
      let set1 = holesType t
      let set2 = holesBindersType xts zs
      S.union set1 set2

holesAttrData :: AttrD.Attr name (BinderF WT.WeakType) -> S.Set HoleID
holesAttrData attr = do
  let consNameList = AttrD.consNameList attr
  S.unions $ map (\(_, binders, _) -> S.unions $ map (\(_, _, _, t) -> holesType t) binders) consNameList
