module Language.Term.Extend
  ( extend,
    extendType,
    extendBinder,
    extendStmtKind,
  )
where

import Control.Comonad.Cofree
import Language.Common.Binder
import Language.Common.DecisionTree qualified as DT
import Language.Common.Discriminant qualified as D
import Language.Common.IsConstLike (IsConstLike)
import Language.Common.StmtKind
import Language.Term.Term qualified as TM
import Logger.Hint (Hint, SavedHint, internalHint)

{-# INLINE _m #-}
_m :: Hint
_m =
  internalHint

-- | Extend a compressed term (with () annotation) to a full term (with Hint annotation)
-- Note: Types inside TermF are already TM.Type (with Hint), only the term annotation changes
extend :: Cofree TM.TermF () -> TM.Term
extend term =
  case term of
    () :< TM.Var x ->
      _m :< TM.Var x
    () :< TM.VarGlobal g argNum ->
      _m :< TM.VarGlobal g argNum
    () :< TM.PiIntro attr impArgs defaultArgs expArgs e ->
      _m :< TM.PiIntro attr impArgs (map extendDefaultArg defaultArgs) expArgs (extend e)
    () :< TM.PiElim b e impArgs expArgs ->
      _m :< TM.PiElim b (extend e) impArgs (map extend expArgs)
    () :< TM.DataIntro attr consName dataArgs consArgs ->
      _m :< TM.DataIntro attr consName dataArgs (map extend consArgs)
    () :< TM.DataElim isNoetic oets tree ->
      let (os, es, ts) = unzip3 oets
          es' = map extend es
          tree' = extendDecisionTree tree
       in _m :< TM.DataElim isNoetic (zip3 os es' ts) tree'
    () :< TM.BoxIntro letSeq e ->
      _m :< TM.BoxIntro (map extendLet letSeq) (extend e)
    () :< TM.BoxElim castSeq mxt e1 uncastSeq e2 ->
      _m :< TM.BoxElim (map extendLet castSeq) mxt (extend e1) (map extendLet uncastSeq) (extend e2)
    () :< TM.CodeIntro e ->
      _m :< TM.CodeIntro (extend e)
    () :< TM.CodeElim e ->
      _m :< TM.CodeElim (extend e)
    () :< TM.Let opacity mxt e1 e2 ->
      _m :< TM.Let opacity mxt (extend e1) (extend e2)
    () :< TM.Prim prim ->
      _m :< TM.Prim prim
    () :< TM.Magic der ->
      _m :< TM.Magic (fmap extend der)

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
    () :< TM.Pi piKind impArgs defaultArgs expArgs cod ->
      _m :< TM.Pi piKind (map extendBinder impArgs) (map extendTypeDefaultArg defaultArgs) (map extendBinder expArgs) (extendType cod)
    () :< TM.Data attr name es ->
      _m :< TM.Data (fmap extendBinder attr) name (map extendType es)
    () :< TM.Box t ->
      _m :< TM.Box (extendType t)
    () :< TM.BoxNoema t ->
      _m :< TM.BoxNoema (extendType t)
    () :< TM.Code t ->
      _m :< TM.Code (extendType t)
    () :< TM.PrimType pt ->
      _m :< TM.PrimType pt
    () :< TM.Void ->
      _m :< TM.Void
    () :< TM.Resource dd resourceID unitType discarder copier typeTag ->
      _m :< TM.Resource dd resourceID unitType discarder copier typeTag

extendBinder :: BinderF (Cofree TM.TypeF ()) -> BinderF TM.Type
extendBinder (m, x, t) =
  (m, x, extendType t)

extendDefaultArg :: (BinderF TM.Type, Cofree TM.TermF ()) -> (BinderF TM.Type, TM.Term)
extendDefaultArg (binder, e) = (binder, extend e)

extendTypeDefaultArg :: (BinderF (Cofree TM.TypeF ()), TM.Term) -> (BinderF TM.Type, TM.Term)
extendTypeDefaultArg (binder, e) = (extendBinder binder, e)

extendLet :: (BinderF TM.Type, Cofree TM.TermF ()) -> (BinderF TM.Type, TM.Term)
extendLet (binder, e) = (binder, extend e)

extendDecisionTree :: DT.DecisionTree TM.Type (Cofree TM.TermF ()) -> DT.DecisionTree TM.Type TM.Term
extendDecisionTree tree =
  case tree of
    DT.Leaf xs letSeq e ->
      DT.Leaf xs (map extendLet letSeq) (extend e)
    DT.Unreachable ->
      DT.Unreachable
    DT.Switch cursor caseList ->
      DT.Switch cursor (extendCaseList caseList)

extendCaseList :: DT.CaseList TM.Type (Cofree TM.TermF ()) -> DT.CaseList TM.Type TM.Term
extendCaseList (fallbackClause, clauseList) =
  (extendDecisionTree fallbackClause, map extendCase clauseList)

extendCase :: DT.Case TM.Type (Cofree TM.TermF ()) -> DT.Case TM.Type TM.Term
extendCase decisionCase =
  case decisionCase of
    DT.LiteralCase mPat i cont ->
      DT.LiteralCase mPat i (extendDecisionTree cont)
    DT.ConsCase record@(DT.ConsCaseRecord {..}) ->
      DT.ConsCase $
        record
          { DT.cont = extendDecisionTree cont
          }

extendStmtKind :: StmtKind (Cofree TM.TypeF ()) -> StmtKind TM.Type
extendStmtKind stmtKind =
  case stmtKind of
    Normal opacity ->
      Normal opacity
    Inline ->
      Inline
    Main opacity t ->
      Main opacity (extendType t)
    Alias ->
      Alias
    Data name args consInfoList ->
      Data name (map extendBinder args) (map extendConsInfo consInfoList)
    DataIntro name dataArgs consArgs disc ->
      DataIntro name (map extendBinder dataArgs) (map extendBinder consArgs) disc

extendConsInfo ::
  (SavedHint, name, IsConstLike, [BinderF (Cofree TM.TypeF ())], D.Discriminant) ->
  (SavedHint, name, IsConstLike, [BinderF TM.Type], D.Discriminant)
extendConsInfo (hint, name, isConstLike, binders, disc) =
  (hint, name, isConstLike, map extendBinder binders, disc)
