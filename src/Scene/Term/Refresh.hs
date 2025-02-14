module Scene.Term.Refresh (refresh, refresh', refresh'', refreshDecisionTree) where

import Context.App
import Context.Gensym qualified as Gensym
import Control.Comonad.Cofree
import Entity.Attr.Lam qualified as AttrL
import Entity.Binder
import Entity.DecisionTree qualified as DT
import Entity.LamKind qualified as LK
import Entity.Term qualified as TM

refresh :: TM.Term -> App TM.Term
refresh term =
  case term of
    _ :< TM.Tau ->
      return term
    _ :< TM.Var {} ->
      return term
    _ :< TM.VarGlobal {} ->
      return term
    m :< TM.Pi impArgs expArgs t -> do
      impArgs' <- refreshBinder impArgs
      expArgs' <- refreshBinder expArgs
      t' <- refresh t
      return (m :< TM.Pi impArgs' expArgs' t')
    m :< TM.PiIntro (AttrL.Attr {lamKind}) impArgs expArgs e -> do
      newLamID <- Gensym.newCount
      case lamKind of
        LK.Fix xt -> do
          impArgs' <- refreshBinder impArgs
          expArgs' <- refreshBinder expArgs
          [xt'] <- refreshBinder [xt]
          e' <- refresh e
          let fixAttr = AttrL.Attr {lamKind = LK.Fix xt', identity = newLamID}
          return (m :< TM.PiIntro fixAttr impArgs' expArgs' e')
        LK.Normal codType -> do
          impArgs' <- refreshBinder impArgs
          expArgs' <- refreshBinder expArgs
          codType' <- refresh codType
          e' <- refresh e
          let lamAttr = AttrL.Attr {lamKind = LK.Normal codType', identity = newLamID}
          return (m :< TM.PiIntro lamAttr impArgs' expArgs' e')
    m :< TM.PiElim e es -> do
      e' <- refresh e
      es' <- mapM refresh es
      return (m :< TM.PiElim e' es')
    m :< TM.Data attr name es -> do
      es' <- mapM refresh es
      return $ m :< TM.Data attr name es'
    m :< TM.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM refresh dataArgs
      consArgs' <- mapM refresh consArgs
      return $ m :< TM.DataIntro attr consName dataArgs' consArgs'
    m :< TM.DataElim isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM refresh es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      (binder', decisionTree') <- refresh'' binder decisionTree
      let (_, os', ts') = unzip3 binder'
      return $ m :< TM.DataElim isNoetic (zip3 os' es' ts') decisionTree'
    m :< TM.Box t -> do
      t' <- refresh t
      return $ m :< TM.Box t'
    m :< TM.BoxNoema t -> do
      t' <- refresh t
      return $ m :< TM.BoxNoema t'
    m :< TM.BoxIntro letSeq e -> do
      letSeq' <- mapM refreshLet letSeq
      e' <- refresh e
      return $ m :< TM.BoxIntro letSeq' e'
    m :< TM.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      castSeq' <- mapM refreshLet castSeq
      (mxt', e1') <- refreshLet (mxt, e1)
      uncastSeq' <- mapM refreshLet uncastSeq
      e2' <- refresh e2
      return $ m :< TM.BoxElim castSeq' mxt' e1' uncastSeq' e2'
    m :< TM.Let opacity mxt e1 e2 -> do
      e1' <- refresh e1
      ([mxt'], e2') <- refresh' [mxt] e2
      return $ m :< TM.Let opacity mxt' e1' e2'
    _ :< TM.Prim _ ->
      return term
    m :< TM.Magic der -> do
      der' <- traverse refresh der
      return (m :< TM.Magic der')
    m :< TM.Resource dd resourceID unitType discarder copier -> do
      unitType' <- refresh unitType
      discarder' <- refresh discarder
      copier' <- refresh copier
      return $ m :< TM.Resource dd resourceID unitType' discarder' copier'
    _ :< TM.Void ->
      return term

refreshBinder ::
  [BinderF TM.Term] ->
  App [BinderF TM.Term]
refreshBinder binder =
  case binder of
    [] -> do
      return []
    ((m, x, t) : xts) -> do
      t' <- refresh t
      xts' <- refreshBinder xts
      return ((m, x, t') : xts')

refreshLet ::
  (BinderF TM.Term, TM.Term) ->
  App (BinderF TM.Term, TM.Term)
refreshLet ((m, x, t), e) = do
  t' <- refresh t
  e' <- refresh e
  return ((m, x, t'), e')

refresh' ::
  [BinderF TM.Term] ->
  TM.Term ->
  App ([BinderF TM.Term], TM.Term)
refresh' binder e =
  case binder of
    [] -> do
      e' <- refresh e
      return ([], e')
    ((m, x, t) : xts) -> do
      t' <- refresh t
      (xts', e') <- refresh' xts e
      return ((m, x, t') : xts', e')

refresh'' ::
  [BinderF TM.Term] ->
  DT.DecisionTree TM.Term ->
  App ([BinderF TM.Term], DT.DecisionTree TM.Term)
refresh'' binder decisionTree =
  case binder of
    [] -> do
      decisionTree' <- refreshDecisionTree decisionTree
      return ([], decisionTree')
    ((m, x, t) : xts) -> do
      t' <- refresh t
      (xts', e') <- refresh'' xts decisionTree
      return ((m, x, t') : xts', e')

refreshBinder1 ::
  (BinderF TM.Term, TM.Term) ->
  App (BinderF TM.Term, TM.Term)
refreshBinder1 ((m, x, t), e) = do
  e' <- refresh e
  t' <- refresh t
  return ((m, x, t'), e')

refreshLetSeq ::
  [(BinderF TM.Term, TM.Term)] ->
  App [(BinderF TM.Term, TM.Term)]
refreshLetSeq letSeq = do
  case letSeq of
    [] ->
      return []
    letPair : rest -> do
      letPair' <- refreshBinder1 letPair
      rest' <- refreshLetSeq rest
      return (letPair' : rest')

refreshDecisionTree ::
  DT.DecisionTree TM.Term ->
  App (DT.DecisionTree TM.Term)
refreshDecisionTree tree =
  case tree of
    DT.Leaf xs letSeq e -> do
      letSeq' <- refreshLetSeq letSeq
      e' <- refresh e
      return $ DT.Leaf xs letSeq' e'
    DT.Unreachable ->
      return tree
    DT.Switch (cursorVar, cursor) caseList -> do
      cursor' <- refresh cursor
      caseList' <- refreshCaseList caseList
      return $ DT.Switch (cursorVar, cursor') caseList'

refreshCaseList ::
  DT.CaseList TM.Term ->
  App (DT.CaseList TM.Term)
refreshCaseList (fallbackClause, clauseList) = do
  fallbackClause' <- refreshDecisionTree fallbackClause
  clauseList' <- mapM refreshCase clauseList
  return (fallbackClause', clauseList')

refreshCase ::
  DT.Case TM.Term ->
  App (DT.Case TM.Term)
refreshCase decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      cont' <- refreshDecisionTree cont
      return $ DT.LiteralCase mPat i cont'
    DT.ConsCase record@(DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      dataTerms' <- mapM refresh dataTerms
      dataTypes' <- mapM refresh dataTypes
      (consArgs', cont') <- refresh'' consArgs cont
      return $
        DT.ConsCase $
          record
            { DT.dataArgs = zip dataTerms' dataTypes',
              DT.consArgs = consArgs',
              DT.cont = cont'
            }
