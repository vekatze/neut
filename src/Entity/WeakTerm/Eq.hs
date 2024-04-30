module Entity.WeakTerm.Eq (eq) where

import Control.Comonad.Cofree
import Entity.Attr.Lam qualified as AttrL
import Entity.Binder (BinderF)
import Entity.DecisionTree qualified as DT
import Entity.LamKind qualified as LK
import Entity.Magic qualified as M
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Entity.WeakTerm qualified as WT

-- syntactic equality
eq :: WT.WeakTerm -> WT.WeakTerm -> Bool
eq (_ :< term1) (_ :< term2)
  | WT.Tau <- term1,
    WT.Tau <- term2 =
      True
  | WT.Var x1 <- term1,
    WT.Var x2 <- term2 =
      x1 == x2
  | WT.VarGlobal _ dd1 <- term1,
    WT.VarGlobal _ dd2 <- term2 =
      dd1 == dd2
  | WT.Pi impArgs1 expArgs1 cod1 <- term1,
    WT.Pi impArgs2 expArgs2 cod2 <- term2 = do
      let b1 = eqBinder (impArgs1 ++ expArgs1) (impArgs2 ++ expArgs2)
      let b2 = eq cod1 cod2
      b1 && b2
  | WT.PiIntro kind1 impArgs1 expArgs1 body1 <- term1,
    WT.PiIntro kind2 impArgs2 expArgs2 body2 <- term2,
    length impArgs1 == length impArgs2,
    length expArgs1 == length expArgs2 =
      case (kind1, kind2) of
        (AttrL.Attr {lamKind = LK.Normal codType1}, AttrL.Attr {lamKind = LK.Normal codType2}) -> do
          let b1 = eqBinder (impArgs1 ++ expArgs1) (impArgs2 ++ expArgs2)
          let b2 = eq body1 body2
          let b3 = eq codType1 codType2
          b1 && b2 && b3
        (AttrL.Attr {lamKind = LK.Fix mxt1}, AttrL.Attr {lamKind = LK.Fix mxt2}) -> do
          let b1 = eqBinder (impArgs1 ++ expArgs1 ++ [mxt1]) (impArgs2 ++ expArgs2 ++ [mxt2])
          let b2 = eq body1 body2
          b1 && b2
        _ ->
          False
  | WT.PiElim f1 args1 <- term1,
    WT.PiElim f2 args2 <- term2 = do
      let b1 = eq f1 f2
      let b2 = all (uncurry eq) $ zip args1 args2
      b1 && b2
  | WT.PiElimExact f1 <- term1,
    WT.PiElimExact f2 <- term2 =
      eq f1 f2
  | WT.Data _ name1 es1 <- term1,
    WT.Data _ name2 es2 <- term2 = do
      let b1 = name1 == name2
      let b2 = all (uncurry eq) $ zip es1 es2
      b1 && b2
  | WT.DataIntro _ consName1 dataArgs1 consArgs1 <- term1,
    WT.DataIntro _ consName2 dataArgs2 consArgs2 <- term2,
    length dataArgs1 == length dataArgs2,
    length consArgs1 == length consArgs2 = do
      let b1 = consName1 == consName2
      let b2 = all (uncurry eq) $ zip dataArgs1 dataArgs2
      let b3 = all (uncurry eq) $ zip consArgs1 consArgs2
      b1 && b2 && b3
  | WT.DataElim isNoetic1 oets1 tree1 <- term1,
    WT.DataElim isNoetic2 oets2 tree2 <- term2 = do
      let (os1, es1, ts1) = unzip3 oets1
      let (os2, es2, ts2) = unzip3 oets2
      let b1 = isNoetic1 == isNoetic2
      let b2 = os1 == os2
      let b3 = all (uncurry eq) $ zip es1 es2
      let b4 = all (uncurry eq) $ zip ts1 ts2
      let b5 = eqDT tree1 tree2
      b1 && b2 && b3 && b4 && b5
  | WT.Noema t1 <- term1,
    WT.Noema t2 <- term2 =
      eq t1 t2
  | WT.Embody t1 e1 <- term1,
    WT.Embody t2 e2 <- term2 = do
      let b1 = eq t1 t2
      let b2 = eq e1 e2
      b1 && b2
  | WT.Actual e1 <- term1,
    WT.Actual e2 <- term2 = do
      eq e1 e2
  | WT.Let opacity1 mxt1 body1 cont1 <- term1,
    WT.Let opacity2 mxt2 body2 cont2 <- term2,
    opacity1 == opacity2 = do
      let b1 = eq body1 body2
      let b2 = eqBinder [mxt1] [mxt2]
      let b3 = eq cont1 cont2
      b1 && b2 && b3
  | WT.Prim a1 <- term1,
    WT.Prim a2 <- term2 =
      eqWP a1 a2
  | WT.Magic m1 <- term1,
    WT.Magic m2 <- term2 =
      eqM m1 m2
  | WT.Hole h1 es1 <- term1,
    WT.Hole h2 es2 <- term2 = do
      let b1 = h1 == h2
      let b2 = all (uncurry eq) $ zip es1 es2
      b1 && b2
  | WT.Annotation _ _ e1 <- term1,
    WT.Annotation _ _ e2 <- term2 =
      eq e1 e2
  | WT.Resource id1 _ _ <- term1,
    WT.Resource id2 _ _ <- term2 =
      id1 == id2
  | WT.Use trope1 xts1 cont1 <- term1,
    WT.Use trope2 xts2 cont2 <- term2 = do
      let b1 = eq trope1 trope2
      let b2 = eqBinder xts1 xts2
      let b3 = eq cont1 cont2
      b1 && b2 && b3
  | otherwise =
      False

eqBinder :: [BinderF WT.WeakTerm] -> [BinderF WT.WeakTerm] -> Bool
eqBinder xts1 xts2
  | [] <- xts1,
    [] <- xts2 =
      True
  | (_, x1, t1) : rest1 <- xts1,
    (_, x2, t2) : rest2 <- xts2 = do
      let b1 = x1 == x2
      let b2 = eq t1 t2
      let b3 = eqBinder rest1 rest2
      b1 && b2 && b3
  | otherwise =
      False

eqDT :: DT.DecisionTree WT.WeakTerm -> DT.DecisionTree WT.WeakTerm -> Bool
eqDT tree1 tree2
  | DT.Leaf xs1 letSeq1 cont1 <- tree1,
    DT.Leaf xs2 letSeq2 cont2 <- tree2,
    xs1 == xs2,
    length letSeq1 == length letSeq2 = do
      let (binder1, body1) = unzip letSeq1
      let (binder2, body2) = unzip letSeq2
      let b1 = eqBinder binder1 binder2
      let b2 = all (uncurry eq) $ zip body1 body2
      let b3 = eq cont1 cont2
      b1 && b2 && b3
  | DT.Unreachable <- tree1,
    DT.Unreachable <- tree2 =
      True
  | DT.Switch (o1, t1) (fallback1, caseList1) <- tree1,
    DT.Switch (o2, t2) (fallback2, caseList2) <- tree2,
    o1 == o2,
    eq t1 t2,
    eqDT fallback1 fallback2,
    length caseList1 == length caseList2 = do
      all (uncurry eqCase) $ zip caseList1 caseList2
  | otherwise =
      False

eqCase :: DT.Case WT.WeakTerm -> DT.Case WT.WeakTerm -> Bool
eqCase (DT.LiteralIntCase _ i1 cont1) (DT.LiteralIntCase _ i2 cont2) = do
  i1 == i2 && eqDT cont1 cont2
eqCase case1@(DT.ConsCase {}) case2@(DT.ConsCase {}) = do
  let b1 = DT.consDD case1 == DT.consDD case2
  let b2 = DT.isConstLike case1 == DT.isConstLike case2
  let b3 = DT.disc case1 == DT.disc case2
  let (es1, ts1) = unzip $ DT.dataArgs case1
  let (es2, ts2) = unzip $ DT.dataArgs case2
  let b4 = length es1 == length es2
  let b5 = length ts1 == length ts2
  let b6 = all (uncurry eq) $ zip (es1 ++ ts1) (es2 ++ ts2)
  let b7 = eqBinder (DT.consArgs case1) (DT.consArgs case2)
  let b8 = eqDT (DT.cont case1) (DT.cont case2)
  b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8
eqCase _ _ =
  False

eqWP :: WP.WeakPrim WT.WeakTerm -> WP.WeakPrim WT.WeakTerm -> Bool
eqWP prim1 prim2
  | WP.Type t1 <- prim1,
    WP.Type t2 <- prim2 =
      t1 == t2
  | WP.Value v1 <- prim1,
    WP.Value v2 <- prim2 =
      case (v1, v2) of
        (WPV.Int t1 x1, WPV.Int t2 x2) -> do
          let b1 = eq t1 t2
          let b2 = x1 == x2
          b1 && b2
        (WPV.Float t1 x1, WPV.Float t2 x2) -> do
          let b1 = eq t1 t2
          let b2 = x1 == x2
          b1 && b2
        (WPV.Op op1, WPV.Op op2) -> do
          op1 == op2
        (WPV.StaticText t1 x1, WPV.StaticText t2 x2) -> do
          let b1 = eq t1 t2
          let b2 = x1 == x2
          b1 && b2
        _ ->
          False
  | otherwise =
      False

eqM :: M.Magic WT.WeakTerm -> M.Magic WT.WeakTerm -> Bool
eqM m1 m2
  | M.Cast from1 to1 e1 <- m1,
    M.Cast from2 to2 e2 <- m2 = do
      let b1 = eq from1 from2
      let b2 = eq to1 to2
      let b3 = eq e1 e2
      b1 && b2 && b3
  | M.Store lt1 value1 pointer1 <- m1,
    M.Store lt2 value2 pointer2 <- m2 = do
      let b1 = lt1 == lt2
      let b2 = eq value1 value2
      let b3 = eq pointer1 pointer2
      b1 && b2 && b3
  | M.Load lt1 pointer1 <- m1,
    M.Load lt2 pointer2 <- m2 = do
      let b1 = lt1 == lt2
      let b2 = eq pointer1 pointer2
      b1 && b2
  | M.External domList1 cod1 funcName1 args1 varArgs1 <- m1,
    M.External domList2 cod2 funcName2 args2 varArgs2 <- m2,
    length args1 == length args2,
    length varArgs1 == length varArgs2 = do
      let b1 = domList1 == domList2
      let b2 = cod1 == cod2
      let b3 = funcName1 == funcName2
      let b4 = all (uncurry eq) $ zip args1 args2
      let (es1, ts1) = unzip varArgs1
      let (es2, ts2) = unzip varArgs2
      let b5 = all (uncurry eq) $ zip es1 es2
      let b6 = ts1 == ts2
      b1 && b2 && b3 && b4 && b5 && b6
  | M.Global name1 lt1 <- m1,
    M.Global name2 lt2 <- m2 = do
      let b1 = name1 == name2
      let b2 = lt1 == lt2
      b1 && b2
  | otherwise =
      False
