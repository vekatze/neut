module Language.WeakTerm.Eq (eq, eqType) where

import Control.Comonad.Cofree
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.Binder (BinderF)
import Language.Common.DecisionTree qualified as DT
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.DefaultArgs qualified as DefaultArgs
import Language.Common.ImpArgs qualified as ImpArgs
import Language.Common.LamKind qualified as LK
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.WeakTerm.WeakPrimValue qualified as WPV
import Language.WeakTerm.WeakTerm qualified as WT

-- syntactic equality for terms
eq :: WT.WeakTerm -> WT.WeakTerm -> Bool
eq (_ :< term1) (_ :< term2)
  | WT.Var x1 <- term1,
    WT.Var x2 <- term2 =
      x1 == x2
  | WT.VarGlobal _ dd1 <- term1,
    WT.VarGlobal _ dd2 <- term2 =
      dd1 == dd2
  | WT.PiIntro kind1 impArgs1 expArgs1 defaultArgs1 body1 <- term1,
    WT.PiIntro kind2 impArgs2 expArgs2 defaultArgs2 body2 <- term2,
    length impArgs1 == length impArgs2,
    length defaultArgs1 == length defaultArgs2,
    length expArgs1 == length expArgs2 =
      case (kind1, kind2) of
        (AttrL.Attr {lamKind = LK.Normal _ codType1}, AttrL.Attr {lamKind = LK.Normal _ codType2}) -> do
          let b1 = eqImpArgs impArgs1 impArgs2
          let b2 = eqDefaultArgs defaultArgs1 defaultArgs2
          let b3 = eqBinderType (impArgs1 ++ expArgs1 ++ map fst defaultArgs1) (impArgs2 ++ expArgs2 ++ map fst defaultArgs2)
          let b4 = eq body1 body2
          let b5 = eqType codType1 codType2
          b1 && b2 && b3 && b4 && b5
        (AttrL.Attr {lamKind = LK.Fix opacity1 mxt1}, AttrL.Attr {lamKind = LK.Fix opacity2 mxt2})
          | opacity1 == opacity2 -> do
          let b1 = eqImpArgs impArgs1 impArgs2
          let b2 = eqDefaultArgs defaultArgs1 defaultArgs2
          let b3 = eqBinderType (impArgs1 ++ expArgs1 ++ map fst defaultArgs1 ++ [mxt1]) (impArgs2 ++ expArgs2 ++ map fst defaultArgs2 ++ [mxt2])
          let b4 = eq body1 body2
          b1 && b2 && b3 && b4
        _ ->
          False
  | WT.PiElim isNoetic1 f1 ImpArgs.Unspecified expArgs1 defaultArgs1 <- term1,
    WT.PiElim isNoetic2 f2 ImpArgs.Unspecified expArgs2 defaultArgs2 <- term2,
    length expArgs1 == length expArgs2,
    isNoetic1 == isNoetic2 = do
      let b1 = eq f1 f2
      let b2 = eqDefaultOverrideArgs defaultArgs1 defaultArgs2
      let b3 = all (uncurry eq) $ zip expArgs1 expArgs2
      b1 && b2 && b3
  | WT.PiElim isNoetic1 f1 (ImpArgs.FullySpecified impArgs1) expArgs1 defaultArgs1 <- term1,
    WT.PiElim isNoetic2 f2 (ImpArgs.FullySpecified impArgs2) expArgs2 defaultArgs2 <- term2,
    length impArgs1 == length impArgs2,
    length expArgs1 == length expArgs2,
    isNoetic1 == isNoetic2 = do
      let b1 = eq f1 f2
      let b2 = all (uncurry eqType) $ zip impArgs1 impArgs2
      let b3 = eqDefaultOverrideArgs defaultArgs1 defaultArgs2
      let b4 = all (uncurry eq) $ zip expArgs1 expArgs2
      b1 && b2 && b3 && b4
  | WT.PiElimExact f1 <- term1,
    WT.PiElimExact f2 <- term2 =
      eq f1 f2
  | WT.DataIntro attr1 consName1 dataArgs1 consArgs1 <- term1,
    WT.DataIntro attr2 consName2 dataArgs2 consArgs2 <- term2,
    length dataArgs1 == length dataArgs2,
    length consArgs1 == length consArgs2 = do
      let b1 = consName1 == consName2
      let b2 = all (uncurry eqType) $ zip dataArgs1 dataArgs2
      let b3 = all (uncurry eq) $ zip consArgs1 consArgs2
      let b4 = eqAttrDataIntro attr1 attr2
      b1 && b2 && b3 && b4
  | WT.DataElim isNoetic1 oets1 tree1 <- term1,
    WT.DataElim isNoetic2 oets2 tree2 <- term2 = do
      let (os1, es1, ts1) = unzip3 oets1
      let (os2, es2, ts2) = unzip3 oets2
      let b1 = isNoetic1 == isNoetic2
      let b2 = os1 == os2
      let b3 = all (uncurry eq) $ zip es1 es2
      let b4 = all (uncurry eqType) $ zip ts1 ts2
      let b5 = eqDT tree1 tree2
      b1 && b2 && b3 && b4 && b5
  | WT.BoxIntro letSeq1 e1 <- term1,
    WT.BoxIntro letSeq2 e2 <- term2,
    length letSeq1 == length letSeq2 = do
      let (xts1, es1) = unzip letSeq1
      let (xts2, es2) = unzip letSeq2
      let b1 = eqBinderType xts1 xts2
      let b2 = all (uncurry eq) $ zip es1 es2
      let b3 = eq e1 e2
      b1 && b2 && b3
  | WT.BoxIntroLift e1 <- term1,
    WT.BoxIntroLift e2 <- term2 =
      eq e1 e2
  | WT.BoxElim castSeq1 mxt1 e1_1 uncastSeq1 e1_2 <- term1,
    WT.BoxElim castSeq2 mxt2 e2_1 uncastSeq2 e2_2 <- term2,
    length castSeq1 == length castSeq2,
    length uncastSeq1 == length uncastSeq2 = do
      let (xts1a, es1a) = unzip castSeq1
      let (xts2a, es2a) = unzip castSeq2
      let (xts1b, es1b) = unzip uncastSeq1
      let (xts2b, es2b) = unzip uncastSeq2
      let b1 = eqBinderType xts1a xts2a
      let b2 = all (uncurry eq) $ zip es1a es2a
      let b3 = eqBinderType [mxt1] [mxt2]
      let b4 = eq e1_1 e2_1
      let b5 = eqBinderType xts1b xts2b
      let b6 = all (uncurry eq) $ zip es1b es2b
      let b7 = eq e1_2 e2_2
      b1 && b2 && b3 && b4 && b5 && b6 && b7
  | WT.CodeIntro e1 <- term1,
    WT.CodeIntro e2 <- term2 =
      eq e1 e2
  | WT.CodeElim e1 <- term1,
    WT.CodeElim e2 <- term2 =
      eq e1 e2
  | WT.TauIntro ty1 <- term1,
    WT.TauIntro ty2 <- term2 =
      eqType ty1 ty2
  | WT.Actual e1 <- term1,
    WT.Actual e2 <- term2 = do
      eq e1 e2
  | WT.Let opacity1 mxt1 body1 cont1 <- term1,
    WT.Let opacity2 mxt2 body2 cont2 <- term2,
    opacity1 == opacity2 = do
      let b1 = eq body1 body2
      let b2 = eqBinderType [mxt1] [mxt2]
      let b3 = eq cont1 cont2
      b1 && b2 && b3
  | WT.Prim a1 <- term1,
    WT.Prim a2 <- term2 =
      eqWPV a1 a2
  | WT.Magic m1 <- term1,
    WT.Magic m2 <- term2 =
      eqM m1 m2
  | WT.Annotation _ _ e1 <- term1,
    WT.Annotation _ _ e2 <- term2 =
      eq e1 e2
  | otherwise =
      False

-- syntactic equality for types
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
  | WT.Pi _ impArgs1 expArgs1 defaultArgs1 cod1 <- ty1,
    WT.Pi _ impArgs2 expArgs2 defaultArgs2 cod2 <- ty2 = do
      let b1 = eqImpArgs impArgs1 impArgs2
      let b2 = eqBinderType defaultArgs1 defaultArgs2
      let b3 = eqBinderType (impArgs1 ++ expArgs1 ++ defaultArgs1) (impArgs2 ++ expArgs2 ++ defaultArgs2)
      let b4 = eqType cod1 cod2
      b1 && b2 && b3 && b4
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
  | WT.Resource _ id1 _ _ _ <- ty1,
    WT.Resource _ id2 _ _ _ <- ty2 =
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

eqDefaultOverrideArgs :: DefaultArgs.DefaultArgs WT.WeakTerm -> DefaultArgs.DefaultArgs WT.WeakTerm -> Bool
eqDefaultOverrideArgs args1 args2 =
  case (args1, args2) of
    (DefaultArgs.ByKey xs, DefaultArgs.ByKey ys) ->
      length xs == length ys && all (uncurry eqKeyArg) (zip xs ys)
    (DefaultArgs.Aligned xs, DefaultArgs.Aligned ys) ->
      length xs == length ys && all (uncurry eqMaybeTerm) (zip xs ys)
    _ ->
      False
  where
    eqKeyArg (k1, v1) (k2, v2) =
      k1 == k2 && eq v1 v2
    eqMaybeTerm mx my =
      case (mx, my) of
        (Just x, Just y) ->
          eq x y
        (Nothing, Nothing) ->
          True
        _ ->
          False

eqDefaultArgs :: [(BinderF WT.WeakType, WT.WeakTerm)] -> [(BinderF WT.WeakType, WT.WeakTerm)] -> Bool
eqDefaultArgs defaultArgs1 defaultArgs2
  | [] <- defaultArgs1,
    [] <- defaultArgs2 =
      True
  | (binder1, value1) : rest1 <- defaultArgs1,
    (binder2, value2) : rest2 <- defaultArgs2 = do
      let b1 = eqBinderType [binder1] [binder2]
      let b2 = eq value1 value2
      let b3 = eqDefaultArgs rest1 rest2
      b1 && b2 && b3
  | otherwise =
      False

eqBinderType :: [BinderF WT.WeakType] -> [BinderF WT.WeakType] -> Bool
eqBinderType xts1 xts2
  | [] <- xts1,
    [] <- xts2 =
      True
  | (_, x1, t1) : rest1 <- xts1,
    (_, x2, t2) : rest2 <- xts2 = do
      let b1 = x1 == x2
      let b2 = eqType t1 t2
      let b3 = eqBinderType rest1 rest2
      b1 && b2 && b3
  | otherwise =
      False

eqDT :: DT.DecisionTree WT.WeakType WT.WeakTerm -> DT.DecisionTree WT.WeakType WT.WeakTerm -> Bool
eqDT tree1 tree2
  | DT.Leaf xs1 letSeq1 cont1 <- tree1,
    DT.Leaf xs2 letSeq2 cont2 <- tree2,
    xs1 == xs2,
    length letSeq1 == length letSeq2 = do
      let (binder1, body1) = unzip letSeq1
      let (binder2, body2) = unzip letSeq2
      let b1 = eqBinderType binder1 binder2
      let b2 = all (uncurry eq) $ zip body1 body2
      let b3 = eq cont1 cont2
      b1 && b2 && b3
  | DT.Unreachable <- tree1,
    DT.Unreachable <- tree2 =
      True
  | DT.Switch (o1, t1) (fallback1, caseList1) <- tree1,
    DT.Switch (o2, t2) (fallback2, caseList2) <- tree2,
    o1 == o2,
    eqType t1 t2,
    eqDT fallback1 fallback2,
    length caseList1 == length caseList2 = do
      all (uncurry eqCase) $ zip caseList1 caseList2
  | otherwise =
      False

eqCase :: DT.Case WT.WeakType WT.WeakTerm -> DT.Case WT.WeakType WT.WeakTerm -> Bool
eqCase (DT.LiteralCase _ i1 cont1) (DT.LiteralCase _ i2 cont2) = do
  i1 == i2 && eqDT cont1 cont2
eqCase (DT.ConsCase case1) (DT.ConsCase case2) = do
  let b1 = DT.consDD case1 == DT.consDD case2
  let b2 = DT.isConstLike case1 == DT.isConstLike case2
  let b3 = DT.disc case1 == DT.disc case2
  let (ts1a, ts1b) = unzip $ DT.dataArgs case1
  let (ts2a, ts2b) = unzip $ DT.dataArgs case2
  let b4 = length ts1a == length ts2a
  let b5 = length ts1b == length ts2b
  let b6 = all (uncurry eqType) $ zip (ts1a ++ ts1b) (ts2a ++ ts2b)
  let b7 = eqBinderType (DT.consArgs case1) (DT.consArgs case2)
  let b8 = eqDT (DT.cont case1) (DT.cont case2)
  b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8
eqCase _ _ =
  False

eqWPV :: WPV.WeakPrimValue WT.WeakType -> WPV.WeakPrimValue WT.WeakType -> Bool
eqWPV v1 v2 =
  case (v1, v2) of
    (WPV.Int t1 x1, WPV.Int t2 x2) -> do
      let b1 = eqType t1 t2
      let b2 = x1 == x2
      b1 && b2
    (WPV.Float t1 x1, WPV.Float t2 x2) -> do
      let b1 = eqType t1 t2
      let b2 = x1 == x2
      b1 && b2
    (WPV.Op op1, WPV.Op op2) -> do
      op1 == op2
    (WPV.StaticText t1 x1, WPV.StaticText t2 x2) -> do
      let b1 = eqType t1 t2
      let b2 = x1 == x2
      b1 && b2
    (WPV.Rune r1, WPV.Rune r2) ->
      r1 == r2
    _ ->
      False

eqM :: M.WeakMagic WT.WeakType WT.WeakType WT.WeakTerm -> M.WeakMagic WT.WeakType WT.WeakType WT.WeakTerm -> Bool
eqM (M.WeakMagic m1) (M.WeakMagic m2)
  | M.LowMagic lowMagic1 <- m1,
    M.LowMagic lowMagic2 <- m2 = do
      case (lowMagic1, lowMagic2) of
        (LM.Cast from1 to1 e1, LM.Cast from2 to2 e2) -> do
          let b1 = eqType from1 from2
          let b2 = eqType to1 to2
          let b3 = eq e1 e2
          b1 && b2 && b3
        (LM.Store t1 unit1 value1 pointer1, LM.Store t2 unit2 value2 pointer2) -> do
          let b1 = eqType t1 t2
          let b2 = eqType unit1 unit2
          let b3 = eq value1 value2
          let b4 = eq pointer1 pointer2
          b1 && b2 && b3 && b4
        (LM.Load t1 pointer1, LM.Load t2 pointer2) -> do
          let b1 = eqType t1 t2
          let b2 = eq pointer1 pointer2
          b1 && b2
        (LM.Alloca t1 size1, LM.Alloca t2 size2) -> do
          let b1 = eqType t1 t2
          let b2 = eq size1 size2
          b1 && b2
        (LM.External domList1 cod1 funcName1 args1 varArgs1, LM.External domList2 cod2 funcName2 args2 varArgs2)
          | length domList1 == length domList2,
            length args1 == length args2,
            length varArgs1 == length varArgs2 -> do
              let b1 = all (uncurry eqType) $ zip domList1 domList2
              let b2 = eqCod cod1 cod2
              let b3 = funcName1 == funcName2
              let b4 = all (uncurry eq) $ zip args1 args2
              let (es1, ts1) = unzip varArgs1
              let (es2, ts2) = unzip varArgs2
              let b5 = all (uncurry eq) $ zip es1 es2
              let b6 = all (uncurry eqType) $ zip ts1 ts2
              b1 && b2 && b3 && b4 && b5 && b6
        (LM.Global name1 t1, LM.Global name2 t2) -> do
          let b1 = name1 == name2
          let b2 = eqType t1 t2
          b1 && b2
        (LM.OpaqueValue e1, LM.OpaqueValue e2) -> do
          eq e1 e2
        (LM.CallType func1 switch1 arg1, LM.CallType func2 switch2 arg2) -> do
          let b1 = eq func1 func2
          let b2 = eq switch1 switch2
          let b3 = eq arg1 arg2
          b1 && b2 && b3
        _ ->
          False
  | M.GetTypeTag mid1 t1 e1 <- m1,
    M.GetTypeTag mid2 t2 e2 <- m2 = do
      let b1 = mid1 == mid2
      let b2 = eqType t1 t2
      let b3 = eqType e1 e2
      b1 && b2 && b3
  | M.GetConsSize t1 <- m1,
    M.GetConsSize t2 <- m2 =
      eqType t1 t2
  | M.GetWrapperContentType t1 <- m1,
    M.GetWrapperContentType t2 <- m2 =
      eqType t1 t2
  | M.GetVectorContentType sgl1 t1 <- m1,
    M.GetVectorContentType sgl2 t2 <- m2 = do
      let b1 = sgl1 == sgl2
      let b2 = eqType t1 t2
      b1 && b2
  | M.GetConstructorArgTypes sgl1 t1a t1b e1 <- m1,
    M.GetConstructorArgTypes sgl2 t2a t2b e2 <- m2 = do
      let b1 = sgl1 == sgl2
      let b2 = eqType t1a t2a
      let b3 = eqType t1b t2b
      let b4 = eq e1 e2
      b1 && b2 && b3 && b4
  | M.CompileError t1 msg1 <- m1,
    M.CompileError t2 msg2 <- m2 = do
      let b1 = eqType t1 t2
      let b2 = eq msg1 msg2
      b1 && b2
  | otherwise =
      False

eqCod :: FCT.ForeignCodType WT.WeakType -> FCT.ForeignCodType WT.WeakType -> Bool
eqCod cod1 cod2
  | FCT.Void <- cod1,
    FCT.Void <- cod2 =
      True
  | FCT.Cod t1 <- cod1,
    FCT.Cod t2 <- cod2 =
      eqType t1 t2
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

eqAttrDataIntro :: (Eq name) => AttrDI.Attr name (BinderF WT.WeakType) -> AttrDI.Attr name (BinderF WT.WeakType) -> Bool
eqAttrDataIntro attr1 attr2 = do
  let consNameList1 = AttrDI.consNameList attr1
  let consNameList2 = AttrDI.consNameList attr2
  let isConstLike1 = AttrDI.isConstLike attr1
  let isConstLike2 = AttrDI.isConstLike attr2
  length consNameList1 == length consNameList2
    && isConstLike1 == isConstLike2
    && all (uncurry eqConsInfo) (zip consNameList1 consNameList2)
  where
    eqConsInfo (cn1, binders1, cl1) (cn2, binders2, cl2) =
      cn1 == cn2 && cl1 == cl2 && eqBinderType binders1 binders2
