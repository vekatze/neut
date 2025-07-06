module Language.Term.Term.Extend (extend, extendStmtKind, extendBinder) where

import Control.Comonad.Cofree
import Data.Bifunctor
import Data.List (unzip5, zip5)
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.Binder
import Language.Common.DecisionTree qualified as DT
import Language.Common.Ident
import Language.Common.LamKind qualified as LK
import Language.Common.StmtKind
import Language.Term.Prim qualified as P
import Language.Term.PrimValue qualified as PV
import Language.Term.Term qualified as TM
import Logger.Hint

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
    _ :< TM.Pi piKind impArgs expArgs t ->
      _m :< TM.Pi piKind (map (bimap extendBinder (fmap extend)) impArgs) (map extendBinder expArgs) (extend t)
    _ :< TM.PiIntro attr impArgs expArgs e -> do
      let attr' = extendAttr attr
      let impArgs' = map (bimap extendBinder (fmap extend)) impArgs
      let expArgs' = map extendBinder expArgs
      let e' = extend e
      _m :< TM.PiIntro attr' impArgs' expArgs' e'
    _ :< TM.PiElim b e impArgs expArgs -> do
      let e' = extend e
      let impArgs' = map extend impArgs
      let expArgs' = map extend expArgs
      _m :< TM.PiElim b e' impArgs' expArgs'
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
    _ :< TM.Box t ->
      _m :< TM.Box (extend t)
    _ :< TM.BoxNoema t ->
      _m :< TM.BoxNoema (extend t)
    _ :< TM.BoxIntro letSeq e -> do
      let (xts, es) = unzip letSeq
      let letSeq' = zip (map extendBinder xts) (map extend es)
      _m :< TM.BoxIntro letSeq' (extend e)
    _ :< TM.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      let castSeq' = map extendLet castSeq
      let (mxt', e1') = extendLet (mxt, e1)
      let uncastSeq' = map extendLet uncastSeq
      let e2' = extend e2
      _m :< TM.BoxElim castSeq' mxt' e1' uncastSeq' e2'
    _ :< TM.Let opacity mxt e1 e2 ->
      _m :< TM.Let opacity (extendBinder mxt) (extend e1) (extend e2)
    _ :< TM.Prim prim ->
      _m :< TM.Prim (extendPrim prim)
    _ :< TM.Magic der -> do
      _m :< TM.Magic (fmap extend der)
    _ :< TM.Resource dd resourceID unitType discarder copier typeTag -> do
      _m :< TM.Resource dd resourceID (extend unitType) (extend discarder) (extend copier) (extend typeTag)
    _ :< TM.Void ->
      _m :< TM.Void

extendBinder :: (Hint, Ident, Cofree TM.TermF ()) -> (Hint, Ident, TM.Term)
extendBinder (m, x, t) =
  (m, x, extend t)

extendLet :: (BinderF (Cofree TM.TermF ()), Cofree TM.TermF ()) -> (BinderF TM.Term, TM.Term)
extendLet ((m, x, t), e) =
  ((m, x, extend t), extend e)

extendAttr :: AttrL.Attr (Cofree TM.TermF ()) -> AttrL.Attr TM.Term
extendAttr AttrL.Attr {lamKind, identity} =
  case lamKind of
    LK.Normal name codType ->
      AttrL.normal' name identity (extend codType)
    LK.Fix xt ->
      AttrL.Attr {lamKind = LK.Fix (extendBinder xt), identity}

extendPrim :: P.Prim (Cofree TM.TermF ()) -> P.Prim TM.Term
extendPrim prim =
  case prim of
    P.Type t ->
      P.Type t
    P.Value v ->
      P.Value $
        case v of
          PV.Int t size integer ->
            PV.Int (extend t) size integer
          PV.Float t size float ->
            PV.Float (extend t) size float
          PV.Op op ->
            PV.Op op
          PV.StaticText t text ->
            PV.StaticText (extend t) text
          PV.Rune r ->
            PV.Rune r

extendDecisionTree :: DT.DecisionTree (Cofree TM.TermF ()) -> DT.DecisionTree TM.Term
extendDecisionTree tree =
  case tree of
    DT.Leaf xs letSeq e -> do
      let (binderSeq, varSeq) = unzip letSeq
      let letSeq' = zip (map extendBinder binderSeq) (map extend varSeq)
      let e' = extend e
      DT.Leaf xs letSeq' e'
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
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      let cont' = extendDecisionTree cont
      DT.LiteralCase mPat i cont'
    DT.ConsCase record@(DT.ConsCaseRecord {..}) -> do
      let dataArgs' = map (bimap extend extend) dataArgs
      let consArgs' = map extendBinder consArgs
      let cont' = extendDecisionTree cont
      DT.ConsCase $
        record
          { DT.dataArgs = dataArgs',
            DT.consArgs = consArgs',
            DT.cont = cont'
          }

extendStmtKind :: StmtKind (Cofree TM.TermF ()) -> StmtKind TM.Term
extendStmtKind stmtKind =
  case stmtKind of
    Normal opacity ->
      Normal opacity
    Main opacity t ->
      Main opacity (extend t)
    Data dataName dataArgs consInfoList -> do
      let dataArgs' = map extendBinder dataArgs
      let (hintList, consNameList, constLikeList, consArgsList, discriminantList) = unzip5 consInfoList
      let consArgsList' = map (map extendBinder) consArgsList
      let consInfoList' = zip5 hintList consNameList constLikeList consArgsList' discriminantList
      Data dataName dataArgs' consInfoList'
    DataIntro dataName dataArgs expConsArgs discriminant -> do
      let dataArgs' = map extendBinder dataArgs
      let expConsArgs' = map extendBinder expConsArgs
      DataIntro dataName dataArgs' expConsArgs' discriminant
