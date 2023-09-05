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
    StmtDefine isConstLike stmtKind m name impArgNum xts codType e -> do
      let stmtKind' = weakenStmtKind stmtKind
      let xts' = map weakenBinder xts
      let codType' = weaken codType
      let e' = weaken e
      WeakStmtDefine isConstLike stmtKind' m name impArgNum xts' codType' e'
    StmtDefineResource m name discarder copier -> do
      let discarder' = weaken discarder
      let copier' = weaken copier
      WeakStmtDefineResource m name discarder' copier'

weaken :: TM.Term -> WT.WeakTerm
weaken term =
  case term of
    m :< TM.Tau ->
      m :< WT.Tau
    m :< TM.Var x ->
      m :< WT.Var x
    m :< TM.VarGlobal g argNum ->
      m :< WT.VarGlobal g argNum
    m :< TM.Pi xts t ->
      m :< WT.Pi (map weakenBinder xts) (weaken t)
    m :< TM.PiIntro kind xts e -> do
      let kind' = weakenKind kind
      let xts' = map weakenBinder xts
      let e' = weaken e
      m :< WT.PiIntro kind' xts' e'
    m :< TM.PiElim e es -> do
      let e' = weaken e
      let es' = map weaken es
      m :< WT.PiElim e' es'
    m :< TM.Data name consNameList es -> do
      let es' = map weaken es
      m :< WT.Data name consNameList es'
    m :< TM.DataIntro dataName consName consNameList disc dataArgs consArgs -> do
      let dataArgs' = map weaken dataArgs
      let consArgs' = map weaken consArgs
      m :< WT.DataIntro dataName consName consNameList disc dataArgs' consArgs'
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
    m :< TM.ResourceType name ->
      m :< WT.ResourceType name
    m :< TM.Magic der -> do
      m :< WT.Magic (fmap weaken der)
    m :< TM.Flow pVar t ->
      m :< WT.Flow pVar (weaken t)
    m :< TM.FlowIntro pVar var (e, t) ->
      m :< WT.FlowIntro pVar var (weaken e, weaken t)
    m :< TM.FlowElim pVar var (e, t) ->
      m :< WT.FlowElim pVar var (weaken e, weaken t)

weakenBinder :: (Hint, Ident, TM.Term) -> (Hint, Ident, WT.WeakTerm)
weakenBinder (m, x, t) =
  (m, x, weaken t)

weakenKind :: LK.LamKindF TM.Term -> LK.LamKindF WT.WeakTerm
weakenKind kind =
  case kind of
    LK.Normal opacity ->
      LK.Normal opacity
    LK.Fix xt ->
      LK.Fix (weakenBinder xt)

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
    DT.Leaf xs e -> do
      let e' = weaken e
      DT.Leaf xs e'
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
weakenCase decisonCase = do
  case decisonCase of
    DT.Cons m dd disc dataArgs consArgs tree -> do
      let dataArgs' = map (bimap weaken weaken) dataArgs
      let consArgs' = map weakenBinder consArgs
      let tree' = weakenDecisionTree tree
      DT.Cons m dd disc dataArgs' consArgs' tree'

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
