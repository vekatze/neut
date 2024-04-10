module Entity.Term.Weaken
  ( weaken,
    weakenBinder,
    weakenStmt,
    weakenDecisionTree,
  )
where

import Control.Comonad.Cofree
import Data.Bifunctor
import Data.List
import Entity.Attr.Lam qualified as AttrL
import Entity.DecisionTree qualified as DT
import Entity.Hint
import Entity.Ident
import Entity.LamKind qualified as LK
import Entity.Prim qualified as P
import Entity.PrimType qualified as PT
import Entity.PrimValue qualified as PV
import Entity.Stmt
import Entity.StmtKind
import Entity.Term qualified as TM
import Entity.Term.FromPrimNum
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Entity.WeakTerm (reflectOpacity)
import Entity.WeakTerm qualified as WT

weakenStmt :: Stmt -> WeakStmt
weakenStmt stmt = do
  case stmt of
    StmtDefine isConstLike stmtKind (SavedHint m) name impArgs expArgs codType e -> do
      let stmtKind' = weakenStmtKind stmtKind
      let impArgs' = map weakenBinder impArgs
      let expArgs' = map weakenBinder expArgs
      let codType' = weaken codType
      let e' = weaken e
      WeakStmtDefine isConstLike stmtKind' m name impArgs' expArgs' codType' e'
    StmtDefineConst (SavedHint m) dd t v -> do
      let t' = weaken t
      let v' = weaken v
      WeakStmtDefineConst m dd t' v'
    StmtForeign foreignList ->
      WeakStmtForeign foreignList

weaken :: TM.Term -> WT.WeakTerm
weaken term =
  case term of
    m :< TM.Tau ->
      m :< WT.Tau
    m :< TM.Var x ->
      m :< WT.Var x
    m :< TM.VarGlobal g argNum ->
      m :< WT.VarGlobal g argNum
    m :< TM.Pi impArgs expArgs t ->
      m :< WT.Pi (map weakenBinder impArgs) (map weakenBinder expArgs) (weaken t)
    m :< TM.PiIntro attr impArgs expArgs e -> do
      let attr' = weakenAttr attr
      let impArgs' = map weakenBinder impArgs
      let expArgs' = map weakenBinder expArgs
      let e' = weaken e
      m :< WT.PiIntro attr' impArgs' expArgs' e'
    m :< TM.PiElim e es -> do
      let e' = weaken e
      let es' = map weaken es
      m :< WT.PiElim e' es'
    m :< TM.Data attr name es -> do
      let es' = map weaken es
      m :< WT.Data attr name es'
    m :< TM.DataIntro attr consName dataArgs consArgs -> do
      let dataArgs' = map weaken dataArgs
      let consArgs' = map weaken consArgs
      m :< WT.DataIntro attr consName dataArgs' consArgs'
    m :< TM.DataElim isNoetic oets tree -> do
      let (os, es, ts) = unzip3 oets
      let es' = map weaken es
      let ts' = map weaken ts
      let tree' = weakenDecisionTree tree
      m :< WT.DataElim isNoetic (zip3 os es' ts') tree'
    m :< TM.Noema t ->
      m :< WT.Noema (weaken t)
    m :< TM.Embody t e ->
      m :< WT.Embody (weaken t) (weaken e)
    m :< TM.Let opacity mxt e1 e2 ->
      m :< WT.Let (reflectOpacity opacity) (weakenBinder mxt) (weaken e1) (weaken e2)
    m :< TM.Prim prim ->
      m :< WT.Prim (weakenPrim m prim)
    m :< TM.Magic der -> do
      m :< WT.Magic (fmap weaken der)
    m :< TM.Resource resourceID discarder copier -> do
      m :< WT.Resource resourceID (weaken discarder) (weaken copier)

weakenBinder :: (Hint, Ident, TM.Term) -> (Hint, Ident, WT.WeakTerm)
weakenBinder (m, x, t) =
  (m, x, weaken t)

weakenAttr :: AttrL.Attr TM.Term -> AttrL.Attr WT.WeakTerm
weakenAttr AttrL.Attr {lamKind, identity} =
  case lamKind of
    LK.Normal codType ->
      AttrL.normal identity (weaken codType)
    LK.Fix xt ->
      AttrL.Attr {lamKind = LK.Fix (weakenBinder xt), identity}

weakenPrim :: Hint -> P.Prim TM.Term -> WP.WeakPrim WT.WeakTerm
weakenPrim m prim =
  case prim of
    P.Type t ->
      WP.Type t
    P.Value v ->
      WP.Value $
        case v of
          PV.Int size integer ->
            WPV.Int (weaken (fromPrimNum m (PT.Int size))) integer
          PV.Float size float ->
            WPV.Float (weaken (fromPrimNum m (PT.Float size))) float
          PV.Op op ->
            WPV.Op op
          PV.StaticText t text ->
            WPV.StaticText (weaken t) text

weakenDecisionTree :: DT.DecisionTree TM.Term -> DT.DecisionTree WT.WeakTerm
weakenDecisionTree tree =
  case tree of
    DT.Leaf xs letSeq e -> do
      let letSeq' = map (bimap weakenBinder weaken) letSeq
      let e' = weaken e
      DT.Leaf xs letSeq' e'
    DT.Unreachable ->
      DT.Unreachable
    DT.Switch (cursorVar, cursor) caseList -> do
      let cursor' = weaken cursor
      let caseList' = weakenCaseList caseList
      DT.Switch (cursorVar, cursor') caseList'

weakenCaseList :: DT.CaseList TM.Term -> DT.CaseList WT.WeakTerm
weakenCaseList (fallbackClause, clauseList) = do
  let fallbackClause' = weakenDecisionTree fallbackClause
  let clauseList' = map weakenCase clauseList
  (fallbackClause', clauseList')

weakenCase :: DT.Case TM.Term -> DT.Case WT.WeakTerm
weakenCase decisionCase = do
  let dataArgs' = map (bimap weaken weaken) $ DT.dataArgs decisionCase
  let consArgs' = map weakenBinder $ DT.consArgs decisionCase
  let cont' = weakenDecisionTree $ DT.cont decisionCase
  decisionCase
    { DT.dataArgs = dataArgs',
      DT.consArgs = consArgs',
      DT.cont = cont'
    }

weakenStmtKind :: StmtKind TM.Term -> StmtKind WT.WeakTerm
weakenStmtKind stmtKind =
  case stmtKind of
    Normal opacity ->
      Normal opacity
    Data dataName dataArgs consInfoList -> do
      let dataArgs' = map weakenBinder dataArgs
      let (hintList, consNameList, constLikeList, consArgsList, discriminantList) = unzip5 consInfoList
      let consArgsList' = map (map weakenBinder) consArgsList
      let consInfoList' = zip5 hintList consNameList constLikeList consArgsList' discriminantList
      Data dataName dataArgs' consInfoList'
    DataIntro dataName dataArgs consArgs discriminant -> do
      let dataArgs' = map weakenBinder dataArgs
      let consArgs' = map weakenBinder consArgs
      DataIntro dataName dataArgs' consArgs' discriminant
