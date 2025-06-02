module Language.Term.Move.Refresh
  ( Handle,
    new,
    refresh,
  )
where

import Control.Comonad.Cofree
import Control.Monad.IO.Class
import Gensym.Move.Gensym qualified as Gensym
import Gensym.Rule.Handle qualified as Gensym
import Language.Common.Rule.Attr.Lam qualified as AttrL
import Language.Common.Rule.Binder
import Language.Common.Rule.DecisionTree qualified as DT
import Language.Common.Rule.LamKind qualified as LK
import Language.Term.Rule.Term qualified as TM

newtype Handle = Handle
  { gensymHandle :: Gensym.Handle
  }

new :: Gensym.Handle -> Handle
new gensymHandle = do
  Handle {..}

refresh :: Handle -> TM.Term -> IO TM.Term
refresh h term =
  case term of
    _ :< TM.Tau ->
      return term
    _ :< TM.Var {} ->
      return term
    _ :< TM.VarGlobal {} ->
      return term
    m :< TM.Pi impArgs expArgs t -> do
      impArgs' <- refreshBinder h impArgs
      expArgs' <- refreshBinder h expArgs
      t' <- refresh h t
      return (m :< TM.Pi impArgs' expArgs' t')
    m :< TM.PiIntro (AttrL.Attr {lamKind}) impArgs expArgs e -> do
      newLamID <- liftIO $ Gensym.newCount (gensymHandle h)
      case lamKind of
        LK.Fix xt -> do
          impArgs' <- refreshBinder h impArgs
          expArgs' <- refreshBinder h expArgs
          [xt'] <- refreshBinder h [xt]
          e' <- refresh h e
          let fixAttr = AttrL.Attr {lamKind = LK.Fix xt', identity = newLamID}
          return (m :< TM.PiIntro fixAttr impArgs' expArgs' e')
        LK.Normal name codType -> do
          impArgs' <- refreshBinder h impArgs
          expArgs' <- refreshBinder h expArgs
          codType' <- refresh h codType
          e' <- refresh h e
          let lamAttr = AttrL.Attr {lamKind = LK.Normal name codType', identity = newLamID}
          return (m :< TM.PiIntro lamAttr impArgs' expArgs' e')
    m :< TM.PiElim b e es -> do
      e' <- refresh h e
      es' <- mapM (refresh h) es
      return (m :< TM.PiElim b e' es')
    m :< TM.Data attr name es -> do
      es' <- mapM (refresh h) es
      return $ m :< TM.Data attr name es'
    m :< TM.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (refresh h) dataArgs
      consArgs' <- mapM (refresh h) consArgs
      return $ m :< TM.DataIntro attr consName dataArgs' consArgs'
    m :< TM.DataElim isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (refresh h) es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      (binder', decisionTree') <- refresh'' h binder decisionTree
      let (_, os', ts') = unzip3 binder'
      return $ m :< TM.DataElim isNoetic (zip3 os' es' ts') decisionTree'
    m :< TM.Box t -> do
      t' <- refresh h t
      return $ m :< TM.Box t'
    m :< TM.BoxNoema t -> do
      t' <- refresh h t
      return $ m :< TM.BoxNoema t'
    m :< TM.BoxIntro letSeq e -> do
      letSeq' <- mapM (refreshLet h) letSeq
      e' <- refresh h e
      return $ m :< TM.BoxIntro letSeq' e'
    m :< TM.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      castSeq' <- mapM (refreshLet h) castSeq
      (mxt', e1') <- refreshLet h (mxt, e1)
      uncastSeq' <- mapM (refreshLet h) uncastSeq
      e2' <- refresh h e2
      return $ m :< TM.BoxElim castSeq' mxt' e1' uncastSeq' e2'
    m :< TM.Let opacity mxt e1 e2 -> do
      e1' <- refresh h e1
      ([mxt'], e2') <- refresh' h [mxt] e2
      return $ m :< TM.Let opacity mxt' e1' e2'
    _ :< TM.Prim _ ->
      return term
    m :< TM.Magic der -> do
      der' <- traverse (refresh h) der
      return (m :< TM.Magic der')
    m :< TM.Resource dd resourceID unitType discarder copier -> do
      unitType' <- refresh h unitType
      discarder' <- refresh h discarder
      copier' <- refresh h copier
      return $ m :< TM.Resource dd resourceID unitType' discarder' copier'
    _ :< TM.Void ->
      return term

refreshBinder ::
  Handle ->
  [BinderF TM.Term] ->
  IO [BinderF TM.Term]
refreshBinder h binder =
  case binder of
    [] -> do
      return []
    ((m, x, t) : xts) -> do
      t' <- refresh h t
      xts' <- refreshBinder h xts
      return ((m, x, t') : xts')

refreshLet ::
  Handle ->
  (BinderF TM.Term, TM.Term) ->
  IO (BinderF TM.Term, TM.Term)
refreshLet h ((m, x, t), e) = do
  t' <- refresh h t
  e' <- refresh h e
  return ((m, x, t'), e')

refresh' ::
  Handle ->
  [BinderF TM.Term] ->
  TM.Term ->
  IO ([BinderF TM.Term], TM.Term)
refresh' h binder e =
  case binder of
    [] -> do
      e' <- refresh h e
      return ([], e')
    ((m, x, t) : xts) -> do
      t' <- refresh h t
      (xts', e') <- refresh' h xts e
      return ((m, x, t') : xts', e')

refresh'' ::
  Handle ->
  [BinderF TM.Term] ->
  DT.DecisionTree TM.Term ->
  IO ([BinderF TM.Term], DT.DecisionTree TM.Term)
refresh'' h binder decisionTree =
  case binder of
    [] -> do
      decisionTree' <- refreshDecisionTree h decisionTree
      return ([], decisionTree')
    ((m, x, t) : xts) -> do
      t' <- refresh h t
      (xts', e') <- refresh'' h xts decisionTree
      return ((m, x, t') : xts', e')

refreshBinder1 ::
  Handle ->
  (BinderF TM.Term, TM.Term) ->
  IO (BinderF TM.Term, TM.Term)
refreshBinder1 h ((m, x, t), e) = do
  e' <- refresh h e
  t' <- refresh h t
  return ((m, x, t'), e')

refreshLetSeq ::
  Handle ->
  [(BinderF TM.Term, TM.Term)] ->
  IO [(BinderF TM.Term, TM.Term)]
refreshLetSeq h letSeq = do
  case letSeq of
    [] ->
      return []
    letPair : rest -> do
      letPair' <- refreshBinder1 h letPair
      rest' <- refreshLetSeq h rest
      return (letPair' : rest')

refreshDecisionTree ::
  Handle ->
  DT.DecisionTree TM.Term ->
  IO (DT.DecisionTree TM.Term)
refreshDecisionTree h tree =
  case tree of
    DT.Leaf xs letSeq e -> do
      letSeq' <- refreshLetSeq h letSeq
      e' <- refresh h e
      return $ DT.Leaf xs letSeq' e'
    DT.Unreachable ->
      return tree
    DT.Switch (cursorVar, cursor) caseList -> do
      cursor' <- refresh h cursor
      caseList' <- refreshCaseList h caseList
      return $ DT.Switch (cursorVar, cursor') caseList'

refreshCaseList ::
  Handle ->
  DT.CaseList TM.Term ->
  IO (DT.CaseList TM.Term)
refreshCaseList h (fallbackClause, clauseList) = do
  fallbackClause' <- refreshDecisionTree h fallbackClause
  clauseList' <- mapM (refreshCase h) clauseList
  return (fallbackClause', clauseList')

refreshCase ::
  Handle ->
  DT.Case TM.Term ->
  IO (DT.Case TM.Term)
refreshCase h decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      cont' <- refreshDecisionTree h cont
      return $ DT.LiteralCase mPat i cont'
    DT.ConsCase record@(DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      dataTerms' <- mapM (refresh h) dataTerms
      dataTypes' <- mapM (refresh h) dataTypes
      (consArgs', cont') <- refresh'' h consArgs cont
      return $
        DT.ConsCase $
          record
            { DT.dataArgs = zip dataTerms' dataTypes',
              DT.consArgs = consArgs',
              DT.cont = cont'
            }
