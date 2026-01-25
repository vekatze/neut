module Language.Term.Compress
  ( compress,
    compressType,
    compressBinder,
    compressStmtKindTerm,
    compressStmtKindType,
  )
where

import Control.Comonad.Cofree
import Language.Common.Binder
import Language.Common.DecisionTree qualified as DT
import Language.Common.Discriminant qualified as D
import Language.Common.IsConstLike (IsConstLike)
import Language.Common.StmtKind
import Language.Term.Term qualified as TM
import Logger.Hint (SavedHint)

compress :: TM.Term -> Cofree TM.TermF ()
compress term =
  case term of
    _ :< TM.Var x ->
      () :< TM.Var x
    _ :< TM.VarGlobal g argNum ->
      () :< TM.VarGlobal g argNum
    _ :< TM.PiIntro attr impArgs expArgs defaultArgs e ->
      () :< TM.PiIntro attr impArgs expArgs (map compressDefaultArg defaultArgs) (compress e)
    _ :< TM.PiElim b e impArgs expArgs defaultArgs ->
      () :< TM.PiElim b (compress e) impArgs (map compress expArgs) (map (fmap compress) defaultArgs)
    _ :< TM.DataIntro attr consName dataArgs consArgs ->
      () :< TM.DataIntro attr consName dataArgs (map compress consArgs)
    _ :< TM.DataElim isNoetic oets tree -> do
      let (os, es, ts) = unzip3 oets
      let es' = map compress es
      let tree' = compressDecisionTree tree
      () :< TM.DataElim isNoetic (zip3 os es' ts) tree'
    _ :< TM.BoxIntro letSeq e ->
      () :< TM.BoxIntro (map compressLet letSeq) (compress e)
    _ :< TM.BoxElim castSeq mxt e1 uncastSeq e2 ->
      () :< TM.BoxElim (map compressLet castSeq) mxt (compress e1) (map compressLet uncastSeq) (compress e2)
    _ :< TM.CodeIntro e ->
      () :< TM.CodeIntro (compress e)
    _ :< TM.CodeElim e ->
      () :< TM.CodeElim (compress e)
    _ :< TM.TauIntro ty ->
      () :< TM.TauIntro ty
    _ :< TM.TauElim (mx, x) e1 e2 ->
      () :< TM.TauElim (mx, x) (compress e1) (compress e2)
    _ :< TM.Let opacity mxt e1 e2 ->
      () :< TM.Let opacity mxt (compress e1) (compress e2)
    _ :< TM.Prim prim ->
      () :< TM.Prim prim
    _ :< TM.Magic der ->
      () :< TM.Magic (fmap compress der)

compressType :: TM.Type -> Cofree TM.TypeF ()
compressType ty =
  case ty of
    _ :< TM.Tau ->
      () :< TM.Tau
    _ :< TM.TVar x ->
      () :< TM.TVar x
    _ :< TM.TVarGlobal attr g ->
      () :< TM.TVarGlobal attr g
    _ :< TM.TyApp t args ->
      () :< TM.TyApp (compressType t) (map compressType args)
    _ :< TM.Pi piKind impArgs expArgs defaultArgs cod ->
      () :< TM.Pi piKind (map compressBinder impArgs) (map compressBinder expArgs) (map compressBinder defaultArgs) (compressType cod)
    _ :< TM.Data attr name es ->
      () :< TM.Data (fmap compressBinder attr) name (map compressType es)
    _ :< TM.Box t ->
      () :< TM.Box (compressType t)
    _ :< TM.BoxNoema t ->
      () :< TM.BoxNoema (compressType t)
    _ :< TM.Code t ->
      () :< TM.Code (compressType t)
    _ :< TM.PrimType pt ->
      () :< TM.PrimType pt
    _ :< TM.Void ->
      () :< TM.Void
    _ :< TM.Resource dd resourceID ->
      () :< TM.Resource dd resourceID

compressBinder :: BinderF TM.Type -> BinderF (Cofree TM.TypeF ())
compressBinder (m, k, x, t) =
  (m, k, x, compressType t)

compressDefaultArg :: (BinderF TM.Type, TM.Term) -> (BinderF TM.Type, Cofree TM.TermF ())
compressDefaultArg (binder, e) = (binder, compress e)

compressLet :: (BinderF TM.Type, TM.Term) -> (BinderF TM.Type, Cofree TM.TermF ())
compressLet (binder, e) = (binder, compress e)

compressDecisionTree :: DT.DecisionTree TM.Type TM.Term -> DT.DecisionTree TM.Type (Cofree TM.TermF ())
compressDecisionTree tree =
  case tree of
    DT.Leaf xs letSeq e ->
      DT.Leaf xs (map compressLet letSeq) (compress e)
    DT.Unreachable ->
      DT.Unreachable
    DT.Switch cursor caseList ->
      DT.Switch cursor (compressCaseList caseList)

compressCaseList :: DT.CaseList TM.Type TM.Term -> DT.CaseList TM.Type (Cofree TM.TermF ())
compressCaseList (fallbackClause, clauseList) =
  (compressDecisionTree fallbackClause, map compressCase clauseList)

compressCase :: DT.Case TM.Type TM.Term -> DT.Case TM.Type (Cofree TM.TermF ())
compressCase decisionCase =
  case decisionCase of
    DT.LiteralCase mPat i cont ->
      DT.LiteralCase mPat i (compressDecisionTree cont)
    DT.ConsCase record@(DT.ConsCaseRecord {..}) ->
      DT.ConsCase $
        record
          { DT.cont = compressDecisionTree cont
          }

compressStmtKindTerm :: StmtKindTerm TM.Type -> StmtKindTerm (Cofree TM.TypeF ())
compressStmtKindTerm stmtKind =
  case stmtKind of
    Define ->
      Define
    Inline ->
      Inline
    Macro ->
      Macro
    MacroInline ->
      MacroInline
    Main t ->
      Main (compressType t)
    DataIntro name dataArgs consArgs disc ->
      DataIntro name (map compressBinder dataArgs) (map compressBinder consArgs) disc

compressStmtKindType :: StmtKindType TM.Type -> StmtKindType (Cofree TM.TypeF ())
compressStmtKindType stmtKind =
  case stmtKind of
    Alias ->
      Alias
    AliasOpaque ->
      AliasOpaque
    Data name args consInfoList ->
      Data name (map compressBinder args) (map compressConsInfo consInfoList)

compressConsInfo ::
  (SavedHint, name, IsConstLike, [BinderF TM.Type], D.Discriminant) ->
  (SavedHint, name, IsConstLike, [BinderF (Cofree TM.TypeF ())], D.Discriminant)
compressConsInfo (hint, name, isConstLike, binders, disc) =
  (hint, name, isConstLike, map compressBinder binders, disc)
