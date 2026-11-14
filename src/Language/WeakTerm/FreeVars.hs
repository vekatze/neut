module Language.WeakTerm.FreeVars (freeVars, freeVarsAll, freeVarsType) where

import Control.Comonad.Cofree
import Data.Foldable (toList)
import Data.Maybe
import Data.Set qualified as S
import Language.Common.Annotation qualified as AN
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.Binder
import Language.Common.CallConvSpec qualified as CCS
import Language.Common.DecisionTree qualified as DT
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.Ident
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.Common.VarKind qualified as VK
import Language.WeakTerm.WeakTerm qualified as WT

type TypeVars =
  WT.WeakType -> S.Set Ident

freeVars :: WT.WeakTerm -> S.Set Ident
freeVars =
  freeVarsWith (const S.empty)

freeVarsAll :: WT.WeakTerm -> S.Set Ident
freeVarsAll =
  freeVarsWith freeVarsType

freeVarsWith :: TypeVars -> WT.WeakTerm -> S.Set Ident
freeVarsWith typeVars term =
  case term of
    _ :< WT.Var x ->
      S.singleton x
    _ :< WT.VarGlobal {} ->
      S.empty
    _ :< WT.PiIntro k impParams expParams defaultParams e -> do
      let body = freeVarsBindersWith typeVars (catMaybes [AttrL.fromAttr k]) (freeVarsWith typeVars e)
      freeVarsBindersWith typeVars (impParams ++ expParams) (freeVarsDefaultParamsWith typeVars defaultParams body)
    _ :< WT.PiElim spec e impArgs expArgs defaultArgs -> do
      let xs = freeVarsWith typeVars e
      let ys0 = S.unions $ map typeVars (CCS.types spec)
      let ys1 = S.unions $ map typeVars (toList impArgs)
      let ys2 = S.unions $ map (freeVarsWith typeVars) (toList defaultArgs)
      let ys3 = S.unions $ map (freeVarsWith typeVars) expArgs
      S.unions [xs, ys0, ys1, ys2, ys3]
    _ :< WT.PiElimExact e -> do
      freeVarsWith typeVars e
    _ :< WT.DataIntro _ _ dataArgs consImpArgs consArgs -> do
      let xs1 = S.unions $ map (freeVarsWith typeVars) consArgs
      let xs2 = S.unions $ map typeVars (dataArgs ++ consImpArgs)
      S.union xs1 xs2
    m :< WT.DataElim _ oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      let xs1 = S.unions $ map (freeVarsWith typeVars) es
      let binder = zipWith (\o t -> (m, VK.normal, o, t)) os ts
      let xs2 = freeVarsBindersWith typeVars binder (freeVarsDecisionTreeWith typeVars decisionTree)
      S.union xs1 xs2
    _ :< WT.BoxIntro letSeq e -> do
      let (mxts, es) = unzip letSeq
      freeVarsBindersWith typeVars mxts (S.unions $ map (freeVarsWith typeVars) (e : es))
    _ :< WT.BoxIntroLift _ e ->
      freeVarsWith typeVars e
    _ :< WT.EmbedIntro e ->
      freeVarsWith typeVars e
    _ :< WT.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      let (xts, es) = unzip $ castSeq ++ [(mxt, e1)] ++ uncastSeq
      freeVarsBindersWith typeVars xts (S.unions $ map (freeVarsWith typeVars) $ es ++ [e2])
    _ :< WT.CodeIntro _ e ->
      freeVarsWith typeVars e
    _ :< WT.CodeElim e ->
      freeVarsWith typeVars e
    _ :< WT.TauIntro ty ->
      typeVars ty
    _ :< WT.Let mxt e1 e2 -> do
      let set1 = freeVarsWith typeVars e1
      let set2 = freeVarsBindersWith typeVars [mxt] (freeVarsWith typeVars e2)
      S.union set1 set2
    _ :< WT.TauElim (_, _, x) e1 e2 ->
      S.union (freeVarsWith typeVars e1) (S.delete x (freeVarsWith typeVars e2))
    _ :< WT.LetType (_, x) t e ->
      S.union (typeVars t) (S.delete x (freeVarsWith typeVars e))
    _ :< WT.Invoke _ body ->
      freeVarsWith typeVars body
    _ :< WT.Prim prim ->
      foldMap typeVars prim
    _ :< WT.Magic der ->
      freeVarsMagicWith typeVars der
    _ :< WT.Annotation _ annot e -> do
      let xs1 = freeVarsWith typeVars e
      case annot of
        AN.Type t -> do
          let xs2 = typeVars t
          S.union xs1 xs2

freeVarsDefaultParamsWith :: TypeVars -> [(BinderF WT.WeakType, WT.WeakTerm)] -> S.Set Ident -> S.Set Ident
freeVarsDefaultParamsWith typeVars defaultParams zs =
  case defaultParams of
    [] ->
      zs
    (binder, value) : rest -> do
      let restVars = freeVarsDefaultParamsWith typeVars rest zs
      S.union (freeVarsWith typeVars value) (freeVarsBindersWith typeVars [binder] restVars)

freeVarsBindersWith :: TypeVars -> [BinderF WT.WeakType] -> S.Set Ident -> S.Set Ident
freeVarsBindersWith typeVars binder zs =
  case binder of
    [] ->
      zs
    ((_, _, x, t) : xts) -> do
      let hs1 = typeVars t
      let hs2 = freeVarsBindersWith typeVars xts zs
      S.union hs1 $ S.filter (/= x) hs2

freeVarsDecisionTreeWith :: TypeVars -> DT.DecisionTree WT.WeakType WT.WeakTerm -> S.Set Ident
freeVarsDecisionTreeWith typeVars tree =
  case tree of
    DT.Leaf _ letSeq e ->
      freeVarsWith typeVars (WT.fromLetSeq letSeq e)
    DT.Unreachable ->
      S.empty
    DT.Switch (_, cursor) caseList ->
      S.union (typeVars cursor) (freeVarsCaseListWith typeVars caseList)

freeVarsCaseListWith :: TypeVars -> DT.CaseList WT.WeakType WT.WeakTerm -> S.Set Ident
freeVarsCaseListWith typeVars (fallbackClause, clauseList) = do
  let xs1 = freeVarsDecisionTreeWith typeVars fallbackClause
  let xs2 = S.unions $ map (freeVarsCaseWith typeVars) clauseList
  S.union xs1 xs2

freeVarsCaseWith :: TypeVars -> DT.Case WT.WeakType WT.WeakTerm -> S.Set Ident
freeVarsCaseWith typeVars decisionCase = do
  case decisionCase of
    DT.LiteralCase _ _ cont -> do
      freeVarsDecisionTreeWith typeVars cont
    DT.ConsCase (DT.ConsCaseRecord {..}) -> do
      let (dataTypes1, dataTypes2) = unzip dataArgs
      let contVars = freeVarsDecisionTreeWith typeVars cont
      let binderVars = freeVarsBindersWith typeVars (consImpBinders ++ consBinders) contVars
      S.unions $ binderVars : map typeVars dataTypes1 ++ map typeVars dataTypes2

freeVarsMagicWith :: TypeVars -> M.WeakMagic WT.WeakType WT.WeakType WT.WeakTerm -> S.Set Ident
freeVarsMagicWith typeVars (M.WeakMagic magic) =
  case magic of
    M.LowMagic lowMagic ->
      freeVarsLowMagicWith typeVars lowMagic
    M.Calloc sizeType num size ->
      S.unions [typeVars sizeType, freeVarsWith typeVars num, freeVarsWith typeVars size]
    M.Malloc sizeType size ->
      S.union (typeVars sizeType) (freeVarsWith typeVars size)
    M.MallocOpaque sizeType size ->
      S.union (typeVars sizeType) (freeVarsWith typeVars size)
    M.Realloc sizeType ptr size ->
      S.unions [typeVars sizeType, freeVarsWith typeVars ptr, freeVarsWith typeVars size]
    M.Free unitType ptr ->
      S.union (typeVars unitType) (freeVarsWith typeVars ptr)
    M.InspectType _ typeValueExpr e ->
      S.union (typeVars typeValueExpr) (typeVars e)
    M.EqType _ typeExpr1 typeExpr2 ->
      S.union (typeVars typeExpr1) (typeVars typeExpr2)
    M.ShowType typeExpr ->
      typeVars typeExpr
    M.TextCons rune text ->
      S.union (freeVarsWith typeVars rune) (freeVarsWith typeVars text)
    M.TextUncons _ text ->
      freeVarsWith typeVars text
    M.MakeSwitch _ key fallback clauses ->
      S.unions [freeVarsWith typeVars key, freeVarsWith typeVars fallback, freeVarsWith typeVars clauses]
    M.FreeVariables _ code ->
      freeVarsWith typeVars code
    M.CompileError msg ->
      freeVarsWith typeVars msg
    M.GetOriginFileName ->
      S.empty
    M.GetOriginLine ->
      S.empty
    M.GetOriginColumn ->
      S.empty

freeVarsLowMagicWith :: TypeVars -> LM.LowMagic WT.WeakType WT.WeakType WT.WeakTerm -> S.Set Ident
freeVarsLowMagicWith typeVars lowMagic =
  case lowMagic of
    LM.Cast from to value ->
      S.unions [typeVars from, typeVars to, freeVarsWith typeVars value]
    LM.Store t unit value pointer ->
      S.unions [typeVars t, typeVars unit, freeVarsWith typeVars value, freeVarsWith typeVars pointer]
    LM.Load t pointer ->
      S.union (typeVars t) (freeVarsWith typeVars pointer)
    LM.Shift pointer offset ->
      S.union (freeVarsWith typeVars pointer) (freeVarsWith typeVars offset)
    LM.Alloca t size ->
      S.union (typeVars t) (freeVarsWith typeVars size)
    LM.External domList cod _ args varArgs -> do
      let domVars = S.unions $ map typeVars domList
      let codVars = case cod of
            FCT.Cod t -> typeVars t
            FCT.Void -> S.empty
      let argsVars = S.unions $ map (freeVarsWith typeVars) args
      let varArgsVars = S.unions $ map (\(a, t) -> S.union (freeVarsWith typeVars a) (typeVars t)) varArgs
      S.unions [domVars, codVars, argsVars, varArgsVars]
    LM.Global _ t ->
      typeVars t
    LM.OpaqueValue e ->
      freeVarsWith typeVars e
    LM.CallType func arg1 arg2 arg3 ->
      S.unions $ map (freeVarsWith typeVars) [func, arg1, arg2, arg3]

freeVarsType :: WT.WeakType -> S.Set Ident
freeVarsType ty =
  case ty of
    _ :< WT.Tau ->
      S.empty
    _ :< WT.TVar x ->
      S.singleton x
    _ :< WT.TVarGlobal {} ->
      S.empty
    _ :< WT.TyApp t args -> do
      S.unions $ freeVarsType t : map freeVarsType args
    _ :< WT.Pi _ impParams expParams defaultParams t ->
      freeVarsBindersWith freeVarsType (impParams ++ expParams ++ defaultParams) (freeVarsType t)
    _ :< WT.Data _ _ es ->
      S.unions $ map freeVarsType es
    _ :< WT.Box t ->
      freeVarsType t
    _ :< WT.BoxNoema t ->
      freeVarsType t
    _ :< WT.Embed t ->
      freeVarsType t
    _ :< WT.Code t ->
      freeVarsType t
    _ :< WT.PrimType {} ->
      S.empty
    _ :< WT.Void ->
      S.empty
    _ :< WT.Resource _ _ -> do
      S.empty
    _ :< WT.TypeHole _ es ->
      S.unions $ map freeVarsType es
