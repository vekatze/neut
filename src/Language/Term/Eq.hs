module Language.Term.Eq (eqTerm, eqTerms) where

import Control.Comonad.Cofree
import Language.Common.BaseLowType qualified as BLT
import Language.Common.Binder (BinderF)
import Language.Common.DecisionTree qualified as DT
import Language.Common.Ident (Ident)
import Language.Common.Magic qualified as M
import Language.Common.LowMagic qualified as LM
import Language.Term.Prim qualified as P
import Language.Term.PrimValue qualified as PV
import Language.Term.Term qualified as TM

-- Term equality (syntactic equality, ignoring metadata)
eqTerm :: TM.Term -> TM.Term -> Bool
eqTerm (_ :< term1) (_ :< term2) =
  case (term1, term2) of
    (TM.Tau, TM.Tau) -> True
    (TM.Var x1, TM.Var x2) -> x1 == x2
    (TM.VarGlobal _ dd1, TM.VarGlobal _ dd2) -> dd1 == dd2
    (TM.Pi pk1 impArgs1 defaultArgs1 expArgs1 cod1, TM.Pi pk2 impArgs2 defaultArgs2 expArgs2 cod2) ->
      pk1 == pk2
        && eqTermImpArgs impArgs1 impArgs2
        && eqTermDefaultArgs defaultArgs1 defaultArgs2
        && eqTermBinders (impArgs1 ++ map fst defaultArgs1 ++ expArgs1) (impArgs2 ++ map fst defaultArgs2 ++ expArgs2)
        && eqTerm cod1 cod2
    (TM.PiIntro _ impArgs1 defaultArgs1 expArgs1 e1, TM.PiIntro _ impArgs2 defaultArgs2 expArgs2 e2) ->
      eqTermImpArgs impArgs1 impArgs2
        && eqTermDefaultArgs defaultArgs1 defaultArgs2
        && eqTermBinders (impArgs1 ++ map fst defaultArgs1 ++ expArgs1) (impArgs2 ++ map fst defaultArgs2 ++ expArgs2)
        && eqTerm e1 e2
    (TM.PiElim isNoetic1 e1 impArgs1 expArgs1, TM.PiElim isNoetic2 e2 impArgs2 expArgs2) ->
      isNoetic1 == isNoetic2
        && eqTerm e1 e2
        && eqTerms impArgs1 impArgs2
        && eqTerms expArgs1 expArgs2
    (TM.Data _ name1 es1, TM.Data _ name2 es2) ->
      name1 == name2 && eqTerms es1 es2
    (TM.DataIntro _ consName1 dataArgs1 consArgs1, TM.DataIntro _ consName2 dataArgs2 consArgs2) ->
      consName1 == consName2
        && eqTerms dataArgs1 dataArgs2
        && eqTerms consArgs1 consArgs2
    (TM.DataElim isNoetic1 oets1 dt1, TM.DataElim isNoetic2 oets2 dt2) ->
      isNoetic1 == isNoetic2
        && eqTermOETS oets1 oets2
        && eqTermDecisionTree dt1 dt2
    (TM.Box t1, TM.Box t2) -> eqTerm t1 t2
    (TM.BoxNoema t1, TM.BoxNoema t2) -> eqTerm t1 t2
    (TM.BoxIntro letSeq1 e1, TM.BoxIntro letSeq2 e2) ->
      eqTermLetSeq letSeq1 letSeq2 && eqTerm e1 e2
    (TM.BoxElim castSeq1 _ e1 uncastSeq1 e1', TM.BoxElim castSeq2 _ e2 uncastSeq2 e2') ->
      eqTermLetSeq castSeq1 castSeq2
        && eqTerm e1 e2
        && eqTermLetSeq uncastSeq1 uncastSeq2
        && eqTerm e1' e2'
    (TM.Code t1, TM.Code t2) ->
      eqTerm t1 t2
    (TM.CodeIntro e1, TM.CodeIntro e2) ->
      eqTerm e1 e2
    (TM.CodeElim e1, TM.CodeElim e2) ->
      eqTerm e1 e2
    (TM.Let opacity1 _ e1 e1', TM.Let opacity2 _ e2 e2') ->
      opacity1 == opacity2 && eqTerm e1 e2 && eqTerm e1' e2'
    (TM.Prim prim1, TM.Prim prim2) -> eqTermPrim prim1 prim2
    (TM.Magic magic1, TM.Magic magic2) -> eqTermMagic magic1 magic2
    (TM.Void, TM.Void) -> True
    (TM.Resource dd1 _ unitType1 discarder1 copier1 typeTag1, TM.Resource dd2 _ unitType2 discarder2 copier2 typeTag2) ->
      dd1 == dd2
        && eqTerm unitType1 unitType2
        && eqTerm discarder1 discarder2
        && eqTerm copier1 copier2
        && eqTerm typeTag1 typeTag2
    _ -> False

eqTermImpArgs :: [BinderF TM.Term] -> [BinderF TM.Term] -> Bool
eqTermImpArgs =
  eqTermBinders

eqTermDefaultArgs :: [(BinderF TM.Term, TM.Term)] -> [(BinderF TM.Term, TM.Term)] -> Bool
eqTermDefaultArgs args1 args2 =
  length args1 == length args2
    && and (zipWith eqTermDefaultArg args1 args2)

eqTermDefaultArg :: (BinderF TM.Term, TM.Term) -> (BinderF TM.Term, TM.Term) -> Bool
eqTermDefaultArg (binder1, value1) (binder2, value2) =
  eqTermBinders [binder1] [binder2]
    && eqTerm value1 value2

eqTermBinders :: [BinderF TM.Term] -> [BinderF TM.Term] -> Bool
eqTermBinders bs1 bs2 =
  length bs1 == length bs2
    && and (zipWith eqTermBinder bs1 bs2)

eqTermBinder :: BinderF TM.Term -> BinderF TM.Term -> Bool
eqTermBinder (_, _, t1) (_, _, t2) =
  eqTerm t1 t2

eqTermOETS :: [(Ident, TM.Term, TM.Term)] -> [(Ident, TM.Term, TM.Term)] -> Bool
eqTermOETS oets1 oets2 =
  length oets1 == length oets2
    && and (zipWith eqTermOET oets1 oets2)

eqTermOET :: (Ident, TM.Term, TM.Term) -> (Ident, TM.Term, TM.Term) -> Bool
eqTermOET (_, e1, t1) (_, e2, t2) =
  eqTerm e1 e2 && eqTerm t1 t2

eqTermLetSeq :: [(BinderF TM.Term, TM.Term)] -> [(BinderF TM.Term, TM.Term)] -> Bool
eqTermLetSeq seq1 seq2 =
  length seq1 == length seq2
    && and (zipWith eqTermLetBind seq1 seq2)

eqTermLetBind :: (BinderF TM.Term, TM.Term) -> (BinderF TM.Term, TM.Term) -> Bool
eqTermLetBind (_, e1) (_, e2) =
  eqTerm e1 e2

eqTermDecisionTree :: DT.DecisionTree TM.Term -> DT.DecisionTree TM.Term -> Bool
eqTermDecisionTree dt1 dt2 =
  case (dt1, dt2) of
    (DT.Leaf _ letSeq1 e1, DT.Leaf _ letSeq2 e2) ->
      eqTermLetSeq letSeq1 letSeq2 && eqTerm e1 e2
    (DT.Unreachable, DT.Unreachable) -> True
    (DT.Switch _ (fallback1, cases1), DT.Switch _ (fallback2, cases2)) ->
      eqTermDecisionTree fallback1 fallback2
        && length cases1 == length cases2
        && and (zipWith eqTermCase cases1 cases2)
    _ -> False

eqTermCase :: DT.Case TM.Term -> DT.Case TM.Term -> Bool
eqTermCase case1 case2 =
  case (case1, case2) of
    (DT.LiteralCase _ i1 cont1, DT.LiteralCase _ i2 cont2) ->
      i1 == i2 && eqTermDecisionTree cont1 cont2
    (DT.ConsCase record1, DT.ConsCase record2) ->
      DT.consDD record1 == DT.consDD record2
        && DT.disc record1 == DT.disc record2
        && eqTermDataArgs (DT.dataArgs record1) (DT.dataArgs record2)
        && eqTermBinders (DT.consArgs record1) (DT.consArgs record2)
        && eqTermDecisionTree (DT.cont record1) (DT.cont record2)
    _ -> False

eqTermDataArgs :: [(TM.Term, TM.Term)] -> [(TM.Term, TM.Term)] -> Bool
eqTermDataArgs args1 args2 =
  length args1 == length args2
    && and (zipWith eqTermDataArg args1 args2)

eqTermDataArg :: (TM.Term, TM.Term) -> (TM.Term, TM.Term) -> Bool
eqTermDataArg (t1, ty1) (t2, ty2) =
  eqTerm t1 t2 && eqTerm ty1 ty2

eqTermPrim :: P.Prim TM.Term -> P.Prim TM.Term -> Bool
eqTermPrim prim1 prim2 =
  case (prim1, prim2) of
    (P.Type pt1, P.Type pt2) -> pt1 == pt2
    (P.Value pv1, P.Value pv2) -> eqTermPrimValue pv1 pv2
    _ -> False

eqTermPrimValue :: PV.PrimValue TM.Term -> PV.PrimValue TM.Term -> Bool
eqTermPrimValue pv1 pv2 =
  case (pv1, pv2) of
    (PV.Int t1 size1 val1, PV.Int t2 size2 val2) ->
      eqTerm t1 t2 && size1 == size2 && val1 == val2
    (PV.Float t1 size1 val1, PV.Float t2 size2 val2) ->
      eqTerm t1 t2 && size1 == size2 && val1 == val2
    (PV.Op op1, PV.Op op2) -> op1 == op2
    (PV.StaticText t1 txt1, PV.StaticText t2 txt2) ->
      eqTerm t1 t2 && txt1 == txt2
    (PV.Rune r1, PV.Rune r2) -> r1 == r2
    _ -> False

eqTermMagic :: M.Magic BLT.BaseLowType TM.Term -> M.Magic BLT.BaseLowType TM.Term -> Bool
eqTermMagic magic1 magic2 =
  case (magic1, magic2) of
    (M.LowMagic magic1', M.LowMagic magic2') ->
      case (magic1', magic2') of
        (LM.Cast from1 to1 val1, LM.Cast from2 to2 val2) ->
          eqTerm from1 from2 && eqTerm to1 to2 && eqTerm val1 val2
        (LM.Store lt1 unit1 val1 ptr1, LM.Store lt2 unit2 val2 ptr2) ->
          lt1 == lt2 && eqTerm unit1 unit2 && eqTerm val1 val2 && eqTerm ptr1 ptr2
        (LM.Load lt1 ptr1, LM.Load lt2 ptr2) ->
          lt1 == lt2 && eqTerm ptr1 ptr2
        (LM.Alloca lt1 size1, LM.Alloca lt2 size2) ->
          lt1 == lt2 && eqTerm size1 size2
        (LM.External lts1 cod1 name1 args1 namedArgs1, LM.External lts2 cod2 name2 args2 namedArgs2) ->
          lts1 == lts2
            && cod1 == cod2
            && name1 == name2
            && eqTerms args1 args2
            && eqTermNamedArgs namedArgs1 namedArgs2
        (LM.Global name1 lt1, LM.Global name2 lt2) ->
          name1 == name2 && lt1 == lt2
        (LM.OpaqueValue val1, LM.OpaqueValue val2) ->
          eqTerm val1 val2
        (LM.CallType f1 arg1 ret1, LM.CallType f2 arg2 ret2) ->
          eqTerm f1 f2 && eqTerm arg1 arg2 && eqTerm ret1 ret2
        _ ->
          False
    _ -> False

eqTermNamedArgs :: [(TM.Term, BLT.BaseLowType)] -> [(TM.Term, BLT.BaseLowType)] -> Bool
eqTermNamedArgs args1 args2 =
  length args1 == length args2
    && and (zipWith eqTermNamedArg args1 args2)

eqTermNamedArg :: (TM.Term, BLT.BaseLowType) -> (TM.Term, BLT.BaseLowType) -> Bool
eqTermNamedArg (t1, lt1) (t2, lt2) =
  eqTerm t1 t2 && lt1 == lt2

eqTerms :: [TM.Term] -> [TM.Term] -> Bool
eqTerms ts1 ts2 =
  length ts1 == length ts2 && and (zipWith eqTerm ts1 ts2)
