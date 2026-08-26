module Language.Term.Compress
  ( compress,
    compressCollect,
    compressDiscardingTrace,
    compressType,
    compressBinder,
    compressStmtKindTerm,
    compressStmtKindType,
  )
where

import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.State.Strict
import Data.Bifunctor (second)
import Data.IntSet qualified as IntSet
import Language.Common.Binder
import Language.Common.DataInfo qualified as DI
import Language.Common.DecisionTree qualified as DT
import Language.Common.StmtKind
import Language.Term.Term qualified as TM
import Language.Term.TraceID (TraceID (..), noTrace)

data TraceMode
  = PreserveTrace
  | DiscardTrace

compress :: TM.Term -> Cofree TM.TermF ()
compress term =
  evalState (compressCollect term) IntSet.empty

compressCollect :: TM.Term -> State IntSet.IntSet (Cofree TM.TermF ())
compressCollect =
  compressWithTrace PreserveTrace

compressDiscardingTrace :: TM.Term -> State IntSet.IntSet (Cofree TM.TermF ())
compressDiscardingTrace =
  compressWithTrace DiscardTrace

compressWithTrace :: TraceMode -> TM.Term -> State IntSet.IntSet (Cofree TM.TermF ())
compressWithTrace traceMode term = do
  case term of
    _ :< TM.Var x -> do
      return $ () :< TM.Var x
    _ :< TM.VarGlobal g argNum -> do
      return $ () :< TM.VarGlobal g argNum
    _ :< TM.PiIntro attr impArgs expArgs defaultArgs e -> do
      defaultArgs' <- mapM (compressDefaultArg traceMode) defaultArgs
      e' <- compressWithTrace traceMode e
      return $ () :< TM.PiIntro attr impArgs expArgs defaultArgs' e'
    _ :< TM.PiElim traceID b e impArgs expArgs defaultArgs -> do
      traceID' <- prepareTraceID traceMode traceID
      e' <- compressWithTrace traceMode e
      expArgs' <- mapM (compressWithTrace traceMode) expArgs
      defaultArgs' <- mapM (traverse $ compressWithTrace traceMode) defaultArgs
      return $ () :< TM.PiElim traceID' b e' impArgs expArgs' defaultArgs'
    _ :< TM.DataIntro attr consName dataArgs consArgs -> do
      consArgs' <- mapM (compressWithTrace traceMode) consArgs
      return $ () :< TM.DataIntro attr consName dataArgs consArgs'
    _ :< TM.DataElim traceID isNoetic oets tree -> do
      traceID' <- prepareTraceID traceMode traceID
      let (os, es, ts) = unzip3 oets
      es' <- mapM (compressWithTrace traceMode) es
      tree' <- compressDecisionTree traceMode tree
      return $ () :< TM.DataElim traceID' isNoetic (zip3 os es' ts) tree'
    _ :< TM.BoxIntro traceID letSeq e -> do
      traceID' <- prepareTraceID traceMode traceID
      letSeq' <- mapM (compressLet traceMode) letSeq
      e' <- compressWithTrace traceMode e
      return $ () :< TM.BoxIntro traceID' letSeq' e'
    _ :< TM.BoxIntroLift t e -> do
      e' <- compressWithTrace traceMode e
      return $ () :< TM.BoxIntroLift t e'
    _ :< TM.EmbedIntro e -> do
      e' <- compressWithTrace traceMode e
      return $ () :< TM.EmbedIntro e'
    _ :< TM.BoxElim traceID castSeq mxt e1 uncastSeq e2 -> do
      traceID' <- prepareTraceID traceMode traceID
      castSeq' <- mapM (compressLet traceMode) castSeq
      e1' <- compressWithTrace traceMode e1
      uncastSeq' <- mapM (compressLet traceMode) uncastSeq
      e2' <- compressWithTrace traceMode e2
      return $ () :< TM.BoxElim traceID' castSeq' mxt e1' uncastSeq' e2'
    _ :< TM.CodeIntro e -> do
      e' <- compressWithTrace traceMode e
      return $ () :< TM.CodeIntro e'
    _ :< TM.CodeElim traceID e -> do
      traceID' <- prepareTraceID traceMode traceID
      e' <- compressWithTrace traceMode e
      return $ () :< TM.CodeElim traceID' e'
    _ :< TM.TauIntro ty -> do
      return $ () :< TM.TauIntro ty
    _ :< TM.TauElim traceID (mx, k, x) e1 e2 -> do
      traceID' <- prepareTraceID traceMode traceID
      e1' <- compressWithTrace traceMode e1
      e2' <- compressWithTrace traceMode e2
      return $ () :< TM.TauElim traceID' (mx, k, x) e1' e2'
    _ :< TM.Let mxt e1 e2 -> do
      e1' <- compressWithTrace traceMode e1
      e2' <- compressWithTrace traceMode e2
      return $ () :< TM.Let mxt e1' e2'
    _ :< TM.Invoke tropeNames body -> do
      body' <- compressWithTrace traceMode body
      return $ () :< TM.Invoke tropeNames body'
    _ :< TM.Prim prim -> do
      return $ () :< TM.Prim prim
    _ :< TM.Magic traceID der -> do
      traceID' <- prepareTraceID traceMode traceID
      der' <- mapM (compressWithTrace traceMode) der
      return $ () :< TM.Magic traceID' der'

prepareTraceID :: TraceMode -> TraceID -> State IntSet.IntSet TraceID
prepareTraceID traceMode traceID = do
  case traceMode of
    PreserveTrace -> do
      collectTraceID traceID
      return traceID
    DiscardTrace ->
      return noTrace

collectTraceID :: TraceID -> State IntSet.IntSet ()
collectTraceID (TraceID rawID) = do
  when (rawID /= 0) $ modify' $ IntSet.insert rawID

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
      () :< TM.Data attr name (map compressType es)
    _ :< TM.Box t ->
      () :< TM.Box (compressType t)
    _ :< TM.BoxNoema t ->
      () :< TM.BoxNoema (compressType t)
    _ :< TM.Embed t ->
      () :< TM.Embed (compressType t)
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

compressDefaultArg :: TraceMode -> (BinderF TM.Type, TM.Term) -> State IntSet.IntSet (BinderF TM.Type, Cofree TM.TermF ())
compressDefaultArg traceMode (binder, e) = do
  e' <- compressWithTrace traceMode e
  return (binder, e')

compressLet :: TraceMode -> (BinderF TM.Type, TM.Term) -> State IntSet.IntSet (BinderF TM.Type, Cofree TM.TermF ())
compressLet = compressDefaultArg

compressDecisionTree :: TraceMode -> DT.DecisionTree TM.Type TM.Term -> State IntSet.IntSet (DT.DecisionTree TM.Type (Cofree TM.TermF ()))
compressDecisionTree traceMode tree = do
  case tree of
    DT.Leaf xs letSeq e -> do
      letSeq' <- mapM (compressLet traceMode) letSeq
      e' <- compressWithTrace traceMode e
      return $ DT.Leaf xs letSeq' e'
    DT.Unreachable ->
      return DT.Unreachable
    DT.Switch cursor caseList -> do
      caseList' <- compressCaseList traceMode caseList
      return $ DT.Switch cursor caseList'

compressCaseList :: TraceMode -> DT.CaseList TM.Type TM.Term -> State IntSet.IntSet (DT.CaseList TM.Type (Cofree TM.TermF ()))
compressCaseList traceMode (fallbackClause, clauseList) = do
  fallbackClause' <- compressDecisionTree traceMode fallbackClause
  clauseList' <- mapM (compressCase traceMode) clauseList
  return (fallbackClause', clauseList')

compressCase :: TraceMode -> DT.Case TM.Type TM.Term -> State IntSet.IntSet (DT.Case TM.Type (Cofree TM.TermF ()))
compressCase traceMode decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      cont' <- compressDecisionTree traceMode cont
      return $ DT.LiteralCase mPat i cont'
    DT.ConsCase record@(DT.ConsCaseRecord {..}) -> do
      cont' <- compressDecisionTree traceMode cont
      return $ DT.ConsCase record {DT.cont = cont'}

compressStmtKindTerm :: StmtKindTerm TM.Type -> StmtKindTerm (Cofree TM.TypeF ())
compressStmtKindTerm stmtKind =
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
    Data name args consInfoList isNominal ->
      Data name (map compressBinder args) (map (second compressConsInfo) consInfoList) isNominal

compressConsInfo ::
  DI.ConsInfo (BinderF TM.Type) ->
  DI.ConsInfo (BinderF (Cofree TM.TypeF ()))
compressConsInfo consInfo =
  consInfo {DI.consArgs = map compressBinder (DI.consArgs consInfo)}
