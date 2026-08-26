module Language.Term.Extend
  ( extend,
    extendType,
    extendBinder,
    extendStmtKindTerm,
    extendStmtKindType,
  )
where

import Control.Comonad.Cofree
import Data.Bifunctor (second)
import Language.Common.Binder
import Language.Common.DataInfo qualified as DI
import Language.Common.DecisionTree qualified as DT
import Language.Common.StmtKind
import Language.Term.Term qualified as TM
import Language.Term.Trace qualified as Trace
import Logger.Hint (Hint, internalHint)

{-# INLINE _m #-}
_m :: Hint
_m =
  internalHint

extend :: Trace.Remapping -> Cofree TM.TermF () -> TM.Term
extend remapping term =
  case term of
    () :< TM.Var x ->
      _m :< TM.Var x
    () :< TM.VarGlobal g argNum ->
      _m :< TM.VarGlobal g argNum
    () :< TM.PiIntro attr impArgs expArgs defaultArgs e -> do
      let defaultArgs' = map (extendDefaultArg remapping) defaultArgs
      let e' = extend remapping e
      _m :< TM.PiIntro attr impArgs expArgs defaultArgs' e'
    () :< TM.PiElim traceID b e impArgs expArgs defaultArgs -> do
      let traceID' = Trace.remapOrDrop remapping traceID
      let e' = extend remapping e
      let expArgs' = map (extend remapping) expArgs
      let defaultArgs' = map (fmap (extend remapping)) defaultArgs
      _m :< TM.PiElim traceID' b e' impArgs expArgs' defaultArgs'
    () :< TM.DataIntro attr consName dataArgs consArgs -> do
      let consArgs' = map (extend remapping) consArgs
      _m :< TM.DataIntro attr consName dataArgs consArgs'
    () :< TM.DataElim traceID isNoetic oets tree -> do
      let traceID' = Trace.remapOrDrop remapping traceID
      let (os, es, ts) = unzip3 oets
      let es' = map (extend remapping) es
      let tree' = extendDecisionTree remapping tree
      _m :< TM.DataElim traceID' isNoetic (zip3 os es' ts) tree'
    () :< TM.BoxIntro traceID letSeq e -> do
      let traceID' = Trace.remapOrDrop remapping traceID
      let letSeq' = map (extendLet remapping) letSeq
      let e' = extend remapping e
      _m :< TM.BoxIntro traceID' letSeq' e'
    () :< TM.BoxIntroLift t e -> do
      let e' = extend remapping e
      _m :< TM.BoxIntroLift t e'
    () :< TM.EmbedIntro e -> do
      let e' = extend remapping e
      _m :< TM.EmbedIntro e'
    () :< TM.BoxElim traceID castSeq mxt e1 uncastSeq e2 -> do
      let traceID' = Trace.remapOrDrop remapping traceID
      let castSeq' = map (extendLet remapping) castSeq
      let e1' = extend remapping e1
      let uncastSeq' = map (extendLet remapping) uncastSeq
      let e2' = extend remapping e2
      _m :< TM.BoxElim traceID' castSeq' mxt e1' uncastSeq' e2'
    () :< TM.CodeIntro e -> do
      let e' = extend remapping e
      _m :< TM.CodeIntro e'
    () :< TM.CodeElim traceID e -> do
      let traceID' = Trace.remapOrDrop remapping traceID
      let e' = extend remapping e
      _m :< TM.CodeElim traceID' e'
    () :< TM.TauIntro ty ->
      _m :< TM.TauIntro ty
    () :< TM.TauElim traceID (mx, k, x) e1 e2 -> do
      let traceID' = Trace.remapOrDrop remapping traceID
      let e1' = extend remapping e1
      let e2' = extend remapping e2
      _m :< TM.TauElim traceID' (mx, k, x) e1' e2'
    () :< TM.Let mxt e1 e2 -> do
      let e1' = extend remapping e1
      let e2' = extend remapping e2
      _m :< TM.Let mxt e1' e2'
    () :< TM.Invoke tropeNames body -> do
      let body' = extend remapping body
      _m :< TM.Invoke tropeNames body'
    () :< TM.Prim prim ->
      _m :< TM.Prim prim
    () :< TM.Magic traceID der -> do
      let traceID' = Trace.remapOrDrop remapping traceID
      let der' = fmap (extend remapping) der
      _m :< TM.Magic traceID' der'

extendType :: Cofree TM.TypeF () -> TM.Type
extendType ty =
  case ty of
    () :< TM.Tau ->
      _m :< TM.Tau
    () :< TM.TVar x ->
      _m :< TM.TVar x
    () :< TM.TVarGlobal attr g ->
      _m :< TM.TVarGlobal attr g
    () :< TM.TyApp t args ->
      _m :< TM.TyApp (extendType t) (map extendType args)
    () :< TM.Pi piKind impArgs expArgs defaultArgs cod ->
      _m :< TM.Pi piKind (map extendBinder impArgs) (map extendBinder expArgs) (map extendBinder defaultArgs) (extendType cod)
    () :< TM.Data attr name es ->
      _m :< TM.Data attr name (map extendType es)
    () :< TM.Box t ->
      _m :< TM.Box (extendType t)
    () :< TM.BoxNoema t ->
      _m :< TM.BoxNoema (extendType t)
    () :< TM.Embed t ->
      _m :< TM.Embed (extendType t)
    () :< TM.Code t ->
      _m :< TM.Code (extendType t)
    () :< TM.PrimType pt ->
      _m :< TM.PrimType pt
    () :< TM.Void ->
      _m :< TM.Void
    () :< TM.Resource dd resourceID ->
      _m :< TM.Resource dd resourceID

extendBinder :: BinderF (Cofree TM.TypeF ()) -> BinderF TM.Type
extendBinder (m, k, x, t) =
  (m, k, x, extendType t)

extendDefaultArg :: Trace.Remapping -> (BinderF TM.Type, Cofree TM.TermF ()) -> (BinderF TM.Type, TM.Term)
extendDefaultArg remapping (binder, e) =
  (binder, extend remapping e)

extendLet :: Trace.Remapping -> (BinderF TM.Type, Cofree TM.TermF ()) -> (BinderF TM.Type, TM.Term)
extendLet =
  extendDefaultArg

extendDecisionTree :: Trace.Remapping -> DT.DecisionTree TM.Type (Cofree TM.TermF ()) -> DT.DecisionTree TM.Type TM.Term
extendDecisionTree remapping tree =
  case tree of
    DT.Leaf xs letSeq e -> do
      let letSeq' = map (extendLet remapping) letSeq
      let e' = extend remapping e
      DT.Leaf xs letSeq' e'
    DT.Unreachable ->
      DT.Unreachable
    DT.Switch cursor caseList -> do
      let caseList' = extendCaseList remapping caseList
      DT.Switch cursor caseList'

extendCaseList :: Trace.Remapping -> DT.CaseList TM.Type (Cofree TM.TermF ()) -> DT.CaseList TM.Type TM.Term
extendCaseList remapping (fallbackClause, clauseList) = do
  let fallbackClause' = extendDecisionTree remapping fallbackClause
  let clauseList' = map (extendCase remapping) clauseList
  (fallbackClause', clauseList')

extendCase :: Trace.Remapping -> DT.Case TM.Type (Cofree TM.TermF ()) -> DT.Case TM.Type TM.Term
extendCase remapping decisionCase =
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      let cont' = extendDecisionTree remapping cont
      DT.LiteralCase mPat i cont'
    DT.ConsCase record@(DT.ConsCaseRecord {..}) -> do
      let cont' = extendDecisionTree remapping cont
      DT.ConsCase record {DT.cont = cont'}

extendStmtKindTerm :: StmtKindTerm (Cofree TM.TypeF ()) -> StmtKindTerm TM.Type
extendStmtKindTerm stmtKind =
  case stmtKind of
    Define ->
      Define
    DestPassing ->
      DestPassing
    DestPassingInline ->
      DestPassingInline
    Inline ->
      Inline
    Constant ->
      Constant
    ConstantMeta ->
      ConstantMeta
    Macro ->
      Macro
    MacroInline ->
      MacroInline
    Main t ->
      Main (extendType t)
    DataIntro name dataArgs consArgs disc ->
      DataIntro name (map extendBinder dataArgs) (map extendBinder consArgs) disc

extendStmtKindType :: StmtKindType (Cofree TM.TypeF ()) -> StmtKindType TM.Type
extendStmtKindType stmtKind =
  case stmtKind of
    Alias ->
      Alias
    AliasOpaque ->
      AliasOpaque
    Data name args consInfoList isNominal ->
      Data name (map extendBinder args) (map (second extendConsInfo) consInfoList) isNominal

extendConsInfo ::
  DI.ConsInfo (BinderF (Cofree TM.TypeF ())) ->
  DI.ConsInfo (BinderF TM.Type)
extendConsInfo consInfo =
  consInfo {DI.consArgs = map extendBinder (DI.consArgs consInfo)}
