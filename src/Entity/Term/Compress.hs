module Entity.Term.Compress (compress, compressStmtKind, compressBinder) where

import Control.Comonad.Cofree
import Data.Bifunctor
import Data.List (unzip5, zip5)
import Entity.Attr.Lam qualified as AttrL
import Entity.DecisionTree qualified as DT
import Entity.Hint
import Entity.Ident
import Entity.LamKind qualified as LK
import Entity.Prim qualified as P
import Entity.PrimValue qualified as PV
import Entity.StmtKind
import Entity.Term qualified as TM

compress :: TM.Term -> Cofree TM.TermF ()
compress term =
  case term of
    _ :< TM.Tau ->
      () :< TM.Tau
    _ :< TM.Var x ->
      () :< TM.Var x
    _ :< TM.VarGlobal g argNum ->
      () :< TM.VarGlobal g argNum
    _ :< TM.Pi xts t ->
      () :< TM.Pi (map compressBinder xts) (compress t)
    _ :< TM.PiIntro attr xts e -> do
      let attr' = compressAttr attr
      let xts' = map compressBinder xts
      let e' = compress e
      () :< TM.PiIntro attr' xts' e'
    _ :< TM.PiElim e es -> do
      let e' = compress e
      let es' = map compress es
      () :< TM.PiElim e' es'
    _ :< TM.Data attr name es -> do
      let es' = map compress es
      () :< TM.Data attr name es'
    _ :< TM.DataIntro attr consName dataArgs consArgs -> do
      let dataArgs' = map compress dataArgs
      let consArgs' = map compress consArgs
      () :< TM.DataIntro attr consName dataArgs' consArgs'
    _ :< TM.DataElim isNoetic oets tree -> do
      let (os, es, ts) = unzip3 oets
      let es' = map compress es
      let ts' = map compress ts
      let tree' = compressDecisionTree tree
      () :< TM.DataElim isNoetic (zip3 os es' ts') tree'
    _ :< TM.Noema t ->
      () :< TM.Noema (compress t)
    _ :< TM.Embody t e ->
      () :< TM.Embody (compress t) (compress e)
    _ :< TM.Let opacity mxt e1 e2 ->
      () :< TM.Let opacity (compressBinder mxt) (compress e1) (compress e2)
    _ :< TM.Prim prim ->
      () :< TM.Prim (compressPrim prim)
    _ :< TM.ResourceType name ->
      () :< TM.ResourceType name
    _ :< TM.Magic der -> do
      () :< TM.Magic (fmap compress der)

compressBinder :: (Hint, Ident, TM.Term) -> (Hint, Ident, Cofree TM.TermF ())
compressBinder (m, x, t) =
  (m, x, compress t)

compressAttr :: AttrL.Attr TM.Term -> AttrL.Attr (Cofree TM.TermF ())
compressAttr (AttrL.Attr {lamKind}) =
  case lamKind of
    LK.Normal ->
      AttrL.normal
    LK.Fix xt ->
      AttrL.Attr {lamKind = LK.Fix (compressBinder xt)}

compressPrim :: P.Prim TM.Term -> P.Prim (Cofree TM.TermF ())
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

compressDecisionTree :: DT.DecisionTree TM.Term -> DT.DecisionTree (Cofree TM.TermF ())
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

compressCaseList :: DT.CaseList TM.Term -> DT.CaseList (Cofree TM.TermF ())
compressCaseList (fallbackClause, clauseList) = do
  let fallbackClause' = compressDecisionTree fallbackClause
  let clauseList' = map compressCase clauseList
  (fallbackClause', clauseList')

compressCase :: DT.Case TM.Term -> DT.Case (Cofree TM.TermF ())
compressCase decisionCase = do
  let dataArgs' = map (bimap compress compress) $ DT.dataArgs decisionCase
  let consArgs' = map compressBinder $ DT.consArgs decisionCase
  let cont' = compressDecisionTree $ DT.cont decisionCase
  decisionCase
    { DT.dataArgs = dataArgs',
      DT.consArgs = consArgs',
      DT.cont = cont'
    }

compressStmtKind :: StmtKind TM.Term -> StmtKind (Cofree TM.TermF ())
compressStmtKind stmtKind =
  case stmtKind of
    Normal opacity ->
      Normal opacity
    Data dataName dataArgs consInfoList -> do
      let dataArgs' = map compressBinder dataArgs
      let (hintList, consNameList, constLikeList, consArgsList, discriminantList) = unzip5 consInfoList
      let consArgsList' = map (map compressBinder) consArgsList
      let consInfoList' = zip5 hintList consNameList constLikeList consArgsList' discriminantList
      Data dataName dataArgs' consInfoList'
    DataIntro dataName dataArgs consArgs discriminant -> do
      let dataArgs' = map compressBinder dataArgs
      let consArgs' = map compressBinder consArgs
      DataIntro dataName dataArgs' consArgs' discriminant
