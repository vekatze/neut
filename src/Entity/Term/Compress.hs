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
    _ :< TM.Pi impArgs expArgs t ->
      () :< TM.Pi (map compressBinder impArgs) (map compressBinder expArgs) (compress t)
    _ :< TM.PiIntro attr impArgs expArgs e -> do
      let attr' = compressAttr attr
      let impArgs' = map compressBinder impArgs
      let expArgs' = map compressBinder expArgs
      let e' = compress e
      () :< TM.PiIntro attr' impArgs' expArgs' e'
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
    _ :< TM.Box t ->
      () :< TM.Box (compress t)
    _ :< TM.BoxIntro xets e -> do
      let (xs, es, ts) = unzip3 xets
      let es' = map compress es
      let ts' = map compress ts
      () :< TM.BoxIntro (zip3 xs es' ts') (compress e)
    _ :< TM.Noema t ->
      () :< TM.Noema (compress t)
    _ :< TM.Embody t e ->
      () :< TM.Embody (compress t) (compress e)
    _ :< TM.Let opacity mxt e1 e2 ->
      () :< TM.Let opacity (compressBinder mxt) (compress e1) (compress e2)
    _ :< TM.Prim prim ->
      () :< TM.Prim (compressPrim prim)
    _ :< TM.Magic der -> do
      () :< TM.Magic (fmap compress der)
    _ :< TM.Resource resourceID discarder copier -> do
      () :< TM.Resource resourceID (compress discarder) (compress copier)

compressBinder :: (Hint, Ident, TM.Term) -> (Hint, Ident, Cofree TM.TermF ())
compressBinder (m, x, t) =
  (m, x, compress t)

compressAttr :: AttrL.Attr TM.Term -> AttrL.Attr (Cofree TM.TermF ())
compressAttr (AttrL.Attr {lamKind, identity}) =
  case lamKind of
    LK.Normal codType ->
      AttrL.normal identity (compress codType)
    LK.Fix xt ->
      AttrL.Attr {lamKind = LK.Fix (compressBinder xt), identity}

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
    DT.Leaf xs letSeq e -> do
      let (binderSeq, varSeq) = unzip letSeq
      let letSeq' = zip (map compressBinder binderSeq) (map compress varSeq)
      let e' = compress e
      DT.Leaf xs letSeq' e'
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
  case decisionCase of
    DT.LiteralIntCase mPat i cont -> do
      let cont' = compressDecisionTree cont
      DT.LiteralIntCase mPat i cont'
    DT.ConsCase {..} -> do
      let dataArgs' = map (bimap compress compress) dataArgs
      let consArgs' = map compressBinder consArgs
      let cont' = compressDecisionTree cont
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
