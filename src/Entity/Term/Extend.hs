module Entity.Term.Extend (extend, extendStmtKind, extendBinder) where

import Control.Comonad.Cofree
import Data.Bifunctor
import Data.List (unzip5, zip5)
import Entity.DecisionTree qualified as DT
import Entity.Hint
import Entity.Ident
import Entity.LamKind qualified as LK
import Entity.Prim qualified as P
import Entity.PrimValue qualified as PV
import Entity.StmtKind
import Entity.Term qualified as TM

{-# INLINE _m #-}
_m :: Hint
_m =
  internalHint

extend :: Cofree TM.TermF () -> TM.Term
extend term =
  case term of
    _ :< TM.Tau ->
      _m :< TM.Tau
    _ :< TM.Var x ->
      _m :< TM.Var x
    _ :< TM.VarGlobal g argNum ->
      _m :< TM.VarGlobal g argNum
    _ :< TM.Pi xts t ->
      _m :< TM.Pi (map extendBinder xts) (extend t)
    _ :< TM.PiIntro kind xts e -> do
      let kind' = extendKind kind
      let xts' = map extendBinder xts
      let e' = extend e
      _m :< TM.PiIntro kind' xts' e'
    _ :< TM.PiElim e es -> do
      let e' = extend e
      let es' = map extend es
      _m :< TM.PiElim e' es'
    _ :< TM.Data attr name es -> do
      let es' = map extend es
      _m :< TM.Data attr name es'
    _ :< TM.DataIntro attr consName dataArgs consArgs -> do
      let dataArgs' = map extend dataArgs
      let consArgs' = map extend consArgs
      _m :< TM.DataIntro attr consName dataArgs' consArgs'
    _ :< TM.DataElim isNoetic oets tree -> do
      let (os, es, ts) = unzip3 oets
      let es' = map extend es
      let ts' = map extend ts
      let tree' = extendDecisionTree tree
      _m :< TM.DataElim isNoetic (zip3 os es' ts') tree'
    _ :< TM.Noema t ->
      _m :< TM.Noema (extend t)
    _ :< TM.Embody t e ->
      _m :< TM.Embody (extend t) (extend e)
    _ :< TM.Let opacity mxt e1 e2 ->
      _m :< TM.Let opacity (extendBinder mxt) (extend e1) (extend e2)
    _ :< TM.Prim prim ->
      _m :< TM.Prim (extendPrim prim)
    _ :< TM.ResourceType name ->
      _m :< TM.ResourceType name
    _ :< TM.Magic der -> do
      _m :< TM.Magic (fmap extend der)

extendBinder :: (Hint, Ident, Cofree TM.TermF ()) -> (Hint, Ident, TM.Term)
extendBinder (m, x, t) =
  (m, x, extend t)

extendKind :: LK.LamKindF (Cofree TM.TermF ()) -> LK.LamKindF TM.Term
extendKind kind =
  case kind of
    LK.Normal ->
      LK.Normal
    LK.Fix xt ->
      LK.Fix (extendBinder xt)

extendPrim :: P.Prim (Cofree TM.TermF ()) -> P.Prim TM.Term
extendPrim prim =
  case prim of
    P.Type t ->
      P.Type t
    P.Value v ->
      P.Value $
        case v of
          PV.Int size integer ->
            PV.Int size integer
          PV.Float size float ->
            PV.Float size float
          PV.Op op ->
            PV.Op op
          PV.StaticText t text ->
            PV.StaticText (extend t) text

extendDecisionTree :: DT.DecisionTree (Cofree TM.TermF ()) -> DT.DecisionTree TM.Term
extendDecisionTree tree =
  case tree of
    DT.Leaf xs e -> do
      let e' = extend e
      DT.Leaf xs e'
    DT.Unreachable ->
      DT.Unreachable
    DT.Switch (cursorVar, cursor) caseList -> do
      let cursor' = extend cursor
      let caseList' = extendCaseList caseList
      DT.Switch (cursorVar, cursor') caseList'

extendCaseList :: DT.CaseList (Cofree TM.TermF ()) -> DT.CaseList TM.Term
extendCaseList (fallbackClause, clauseList) = do
  let fallbackClause' = extendDecisionTree fallbackClause
  let clauseList' = map extendCase clauseList
  (fallbackClause', clauseList')

extendCase :: DT.Case (Cofree TM.TermF ()) -> DT.Case TM.Term
extendCase decisionCase = do
  let dataArgs' = map (bimap extend extend) $ DT.dataArgs decisionCase
  let consArgs' = map extendBinder $ DT.consArgs decisionCase
  let cont' = extendDecisionTree $ DT.cont decisionCase
  decisionCase
    { DT.dataArgs = dataArgs',
      DT.consArgs = consArgs',
      DT.cont = cont'
    }

extendStmtKind :: StmtKind (Cofree TM.TermF ()) -> StmtKind TM.Term
extendStmtKind stmtKind =
  case stmtKind of
    Normal opacity ->
      Normal opacity
    Data dataName dataArgs consInfoList -> do
      let dataArgs' = map extendBinder dataArgs
      let (hintList, consNameList, constLikeList, consArgsList, discriminantList) = unzip5 consInfoList
      let consArgsList' = map (map extendBinder) consArgsList
      let consInfoList' = zip5 hintList consNameList constLikeList consArgsList' discriminantList
      Data dataName dataArgs' consInfoList'
    DataIntro dataName dataArgs consArgs discriminant -> do
      let dataArgs' = map extendBinder dataArgs
      let consArgs' = map extendBinder consArgs
      DataIntro dataName dataArgs' consArgs' discriminant