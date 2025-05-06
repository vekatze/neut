module Language.Term.Rule.Term.Weaken
  ( weaken,
    weakenBinder,
    weakenStmt,
    weakenDecisionTree,
  )
where

import Control.Comonad.Cofree
import Data.Bifunctor
import Data.List qualified as List
import Language.Common.Rule.Attr.Lam qualified as AttrL
import Language.Common.Rule.BaseLowType qualified as BLT
import Language.Common.Rule.Binder
import Language.Common.Rule.DecisionTree qualified as DT
import Language.Common.Rule.Foreign qualified as F
import Language.Common.Rule.Ident
import Language.Common.Rule.LamKind qualified as LK
import Language.Common.Rule.Magic qualified as M
import Language.Common.Rule.StmtKind
import Language.Term.Rule.Prim qualified as P
import Language.Term.Rule.PrimValue qualified as PV
import Language.Term.Rule.Stmt
import Language.Term.Rule.Term qualified as TM
import Language.WeakTerm.Rule.WeakPrim qualified as WP
import Language.WeakTerm.Rule.WeakPrimValue qualified as WPV
import Language.WeakTerm.Rule.WeakStmt
import Language.WeakTerm.Rule.WeakTerm (reflectOpacity)
import Language.WeakTerm.Rule.WeakTerm qualified as WT
import Logger.Rule.Hint

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
    StmtForeign foreignList ->
      WeakStmtForeign $ map weakenForeign foreignList

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
    m :< TM.Box t ->
      m :< WT.Box (weaken t)
    m :< TM.BoxNoema t ->
      m :< WT.BoxNoema (weaken t)
    m :< TM.BoxIntro letSeq e -> do
      m :< WT.BoxIntro (map weakenLet letSeq) (weaken e)
    m :< TM.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      let castSeq' = map weakenLet castSeq
      let (mxt', e1') = weakenLet (mxt, e1)
      let uncastSeq' = map weakenLet uncastSeq
      let e2' = weaken e2
      m :< WT.BoxElim castSeq' mxt' e1' uncastSeq' e2'
    m :< TM.Let opacity mxt e1 e2 ->
      m :< WT.Let (reflectOpacity opacity) (weakenBinder mxt) (weaken e1) (weaken e2)
    m :< TM.Prim prim ->
      m :< WT.Prim (weakenPrim prim)
    m :< TM.Magic magic -> do
      m :< WT.Magic (weakenMagic m magic)
    m :< TM.Resource dd resourceID unitType discarder copier -> do
      m :< WT.Resource dd resourceID (weaken unitType) (weaken discarder) (weaken copier)
    m :< TM.Void ->
      m :< WT.Void

weakenMagic :: Hint -> M.Magic BLT.BaseLowType TM.Term -> M.WeakMagic WT.WeakTerm
weakenMagic m magic = do
  case magic of
    M.Cast from to value ->
      M.WeakMagic $ M.Cast (weaken from) (weaken to) (weaken value)
    M.Store t unit value pointer ->
      M.WeakMagic $ M.Store (WT.fromBaseLowType m t) (weaken unit) (weaken value) (weaken pointer)
    M.Load t pointer ->
      M.WeakMagic $ M.Load (WT.fromBaseLowType m t) (weaken pointer)
    M.Alloca t size ->
      M.WeakMagic $ M.Alloca (WT.fromBaseLowType m t) (weaken size)
    M.External domList cod extFunName args varArgs -> do
      let domList' = map (WT.fromBaseLowType m) domList
      let cod' = fmap (WT.fromBaseLowType m) cod
      let varArgs' = map (bimap weaken (WT.fromBaseLowType m)) varArgs
      M.WeakMagic $ M.External domList' cod' extFunName (fmap weaken args) varArgs'
    M.Global name t ->
      M.WeakMagic $ M.Global name (WT.fromBaseLowType m t)
    M.OpaqueValue e ->
      M.WeakMagic $ M.OpaqueValue (weaken e)

weakenBinder :: (Hint, Ident, TM.Term) -> (Hint, Ident, WT.WeakTerm)
weakenBinder (m, x, t) =
  (m, x, weaken t)

weakenLet :: (BinderF TM.Term, TM.Term) -> (BinderF WT.WeakTerm, WT.WeakTerm)
weakenLet ((m, x, t), e) =
  ((m, x, weaken t), weaken e)

weakenAttr :: AttrL.Attr TM.Term -> AttrL.Attr WT.WeakTerm
weakenAttr AttrL.Attr {lamKind, identity} =
  case lamKind of
    LK.Normal codType ->
      AttrL.normal identity (weaken codType)
    LK.Fix xt ->
      AttrL.Attr {lamKind = LK.Fix (weakenBinder xt), identity}

weakenPrim :: P.Prim TM.Term -> WP.WeakPrim WT.WeakTerm
weakenPrim prim =
  case prim of
    P.Type t ->
      WP.Type t
    P.Value v ->
      WP.Value $
        case v of
          PV.Int t _ integer ->
            WPV.Int (weaken t) integer
          PV.Float t _ float ->
            WPV.Float (weaken t) float
          PV.Op op ->
            WPV.Op op
          PV.StaticText t text ->
            WPV.StaticText (weaken t) text
          PV.Rune r ->
            WPV.Rune r

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
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      let cont' = weakenDecisionTree cont
      DT.LiteralCase mPat i cont'
    DT.ConsCase record@(DT.ConsCaseRecord {..}) -> do
      let dataArgs' = map (bimap weaken weaken) dataArgs
      let consArgs' = map weakenBinder consArgs
      let cont' = weakenDecisionTree cont
      DT.ConsCase $
        record
          { DT.dataArgs = dataArgs',
            DT.consArgs = consArgs',
            DT.cont = cont'
          }

weakenStmtKind :: StmtKind TM.Term -> StmtKind WT.WeakTerm
weakenStmtKind stmtKind =
  case stmtKind of
    Normal opacity ->
      Normal opacity
    Main opacity t ->
      Main opacity (weaken t)
    Data dataName dataArgs consInfoList -> do
      let dataArgs' = map weakenBinder dataArgs
      let (hintList, consNameList, constLikeList, consArgsList, discriminantList) = List.unzip5 consInfoList
      let consArgsList' = map (map weakenBinder) consArgsList
      let consInfoList' = List.zip5 hintList consNameList constLikeList consArgsList' discriminantList
      Data dataName dataArgs' consInfoList'
    DataIntro dataName dataArgs consArgs discriminant -> do
      let dataArgs' = map weakenBinder dataArgs
      let consArgs' = map weakenBinder consArgs
      DataIntro dataName dataArgs' consArgs' discriminant

weakenForeign :: F.Foreign -> WT.WeakForeign
weakenForeign foreignItem@(F.Foreign m _ _ _) =
  fmap (WT.fromBaseLowType m) foreignItem
