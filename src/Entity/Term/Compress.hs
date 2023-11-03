module Entity.Term.Compress (compress) where

import Control.Comonad.Cofree
import Data.Bifunctor
import Entity.DecisionTree qualified as DT
import Entity.Hint
import Entity.Ident
import Entity.LamKind qualified as LK
import Entity.Prim qualified as P
import Entity.PrimValue qualified as PV
import Entity.Term qualified as TM

_m :: Hint
_m =
  internalHint

compress :: TM.Term -> TM.Term
compress term =
  case term of
    _ :< TM.Tau ->
      _m :< TM.Tau
    _ :< TM.Var x ->
      _m :< TM.Var x
    _ :< TM.VarGlobal g argNum ->
      _m :< TM.VarGlobal g argNum
    _ :< TM.Pi xts t ->
      _m :< TM.Pi (map compressBinder xts) (compress t)
    _ :< TM.PiIntro kind xts e -> do
      let kind' = compressKind kind
      let xts' = map compressBinder xts
      let e' = compress e
      _m :< TM.PiIntro kind' xts' e'
    _ :< TM.PiElim e es -> do
      let e' = compress e
      let es' = map compress es
      _m :< TM.PiElim e' es'
    _ :< TM.Data attr name es -> do
      let es' = map compress es
      _m :< TM.Data attr name es'
    _ :< TM.DataIntro attr consName dataArgs consArgs -> do
      let dataArgs' = map compress dataArgs
      let consArgs' = map compress consArgs
      _m :< TM.DataIntro attr consName dataArgs' consArgs'
    _ :< TM.DataElim isNoetic oets tree -> do
      let (os, es, ts) = unzip3 oets
      let es' = map compress es
      let ts' = map compress ts
      let tree' = compressDecisionTree tree
      _m :< TM.DataElim isNoetic (zip3 os es' ts') tree'
    _ :< TM.Noema t ->
      _m :< TM.Noema (compress t)
    _ :< TM.Embody t e ->
      _m :< TM.Embody (compress t) (compress e)
    _ :< TM.Let opacity mxt e1 e2 ->
      _m :< TM.Let opacity (compressBinder mxt) (compress e1) (compress e2)
    _ :< TM.Prim prim ->
      _m :< TM.Prim (compressPrim prim)
    _ :< TM.ResourceType name ->
      _m :< TM.ResourceType name
    _ :< TM.Magic der -> do
      _m :< TM.Magic (fmap compress der)

compressBinder :: (Hint, Ident, TM.Term) -> (Hint, Ident, TM.Term)
compressBinder (m, x, t) =
  (m, x, compress t)

compressKind :: LK.LamKindF TM.Term -> LK.LamKindF TM.Term
compressKind kind =
  case kind of
    LK.Normal ->
      LK.Normal
    LK.Fix xt ->
      LK.Fix (compressBinder xt)

compressPrim :: P.Prim TM.Term -> P.Prim TM.Term
compressPrim prim =
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
            PV.StaticText (compress t) text

compressDecisionTree :: DT.DecisionTree TM.Term -> DT.DecisionTree TM.Term
compressDecisionTree tree =
  case tree of
    DT.Leaf xs e -> do
      let e' = compress e
      DT.Leaf xs e'
    DT.Unreachable ->
      DT.Unreachable
    DT.Switch (cursorVar, cursor) caseList -> do
      let cursor' = compress cursor
      let caseList' = compressCaseList caseList
      DT.Switch (cursorVar, cursor') caseList'

compressCaseList :: DT.CaseList TM.Term -> DT.CaseList TM.Term
compressCaseList (fallbackClause, clauseList) = do
  let fallbackClause' = compressDecisionTree fallbackClause
  let clauseList' = map compressCase clauseList
  (fallbackClause', clauseList')

compressCase :: DT.Case TM.Term -> DT.Case TM.Term
compressCase decisionCase = do
  let dataArgs' = map (bimap compress compress) $ DT.dataArgs decisionCase
  let consArgs' = map compressBinder $ DT.consArgs decisionCase
  let cont' = compressDecisionTree $ DT.cont decisionCase
  decisionCase
    { DT.dataArgs = dataArgs',
      DT.consArgs = consArgs',
      DT.cont = cont'
    }
