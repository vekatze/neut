module Language.WeakTerm.Eq (eqType) where

import Control.Comonad.Cofree
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Binder (BinderF)
import Language.WeakTerm.WeakTerm qualified as WT
eqType :: WT.WeakType -> WT.WeakType -> Bool
eqType (_ :< ty1) (_ :< ty2)
  | WT.Tau <- ty1,
    WT.Tau <- ty2 =
      True
  | WT.TVar x1 <- ty1,
    WT.TVar x2 <- ty2 =
      x1 == x2
  | WT.TVarGlobal _ dd1 <- ty1,
    WT.TVarGlobal _ dd2 <- ty2 =
      dd1 == dd2
  | WT.TyApp t1 args1 <- ty1,
    WT.TyApp t2 args2 <- ty2 = do
      let b1 = eqType t1 t2
      let b2 = length args1 == length args2
      let b3 = all (uncurry eqType) $ zip args1 args2
      b1 && b2 && b3
  | WT.Pi pk1 impArgs1 expArgs1 defaultArgs1 cod1 <- ty1,
    WT.Pi pk2 impArgs2 expArgs2 defaultArgs2 cod2 <- ty2 = do
      let b1 = pk1 == pk2
      let b2 = eqImpArgs impArgs1 impArgs2
      let b3 = eqBinderType defaultArgs1 defaultArgs2
      let b4 = eqBinderType (impArgs1 ++ expArgs1 ++ defaultArgs1) (impArgs2 ++ expArgs2 ++ defaultArgs2)
      let b5 = eqType cod1 cod2
      b1 && b2 && b3 && b4 && b5
  | WT.Data attr1 name1 es1 <- ty1,
    WT.Data attr2 name2 es2 <- ty2 = do
      let b1 = name1 == name2
      let b2 = all (uncurry eqType) $ zip es1 es2
      let b3 = eqAttrData attr1 attr2
      b1 && b2 && b3
  | WT.Box t1 <- ty1,
    WT.Box t2 <- ty2 =
      eqType t1 t2
  | WT.BoxNoema t1 <- ty1,
    WT.BoxNoema t2 <- ty2 =
      eqType t1 t2
  | WT.Code t1 <- ty1,
    WT.Code t2 <- ty2 =
      eqType t1 t2
  | WT.PrimType pt1 <- ty1,
    WT.PrimType pt2 <- ty2 =
      pt1 == pt2
  | WT.Void <- ty1,
    WT.Void <- ty2 =
      True
  | WT.Resource _ id1 <- ty1,
    WT.Resource _ id2 <- ty2 =
      id1 == id2
  | WT.TypeHole h1 es1 <- ty1,
    WT.TypeHole h2 es2 <- ty2 = do
      let b1 = h1 == h2
      let b2 = all (uncurry eqType) $ zip es1 es2
      b1 && b2
  | otherwise =
      False

eqImpArgs :: [BinderF WT.WeakType] -> [BinderF WT.WeakType] -> Bool
eqImpArgs =
  eqBinderType


eqBinderType :: [BinderF WT.WeakType] -> [BinderF WT.WeakType] -> Bool
eqBinderType xts1 xts2
  | [] <- xts1,
    [] <- xts2 =
      True
  | (_, k1, x1, t1) : rest1 <- xts1,
    (_, k2, x2, t2) : rest2 <- xts2 = do
      let b1 = k1 == k2 && x1 == x2
      let b2 = eqType t1 t2
      let b3 = eqBinderType rest1 rest2
      b1 && b2 && b3
  | otherwise =
      False


eqAttrData :: (Eq name) => AttrD.Attr name (BinderF WT.WeakType) -> AttrD.Attr name (BinderF WT.WeakType) -> Bool
eqAttrData attr1 attr2 = do
  let consNameList1 = AttrD.consNameList attr1
  let consNameList2 = AttrD.consNameList attr2
  let isConstLike1 = AttrD.isConstLike attr1
  let isConstLike2 = AttrD.isConstLike attr2
  length consNameList1 == length consNameList2
    && isConstLike1 == isConstLike2
    && all (uncurry eqConsInfo) (zip consNameList1 consNameList2)
  where
    eqConsInfo (cn1, binders1, cl1) (cn2, binders2, cl2) =
      cn1 == cn2 && cl1 == cl2 && eqBinderType binders1 binders2
