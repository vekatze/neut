module Move.Scene.Term.Subst
  ( Handle,
    new,
    subst,
    subst',
    subst'',
    substDecisionTree,
  )
where

import Control.Comonad.Cofree
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IntMap qualified as IntMap
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Move.Context.App
import Move.Language.Utility.Gensym qualified as Gensym
import Rule.Attr.Lam qualified as AttrL
import Rule.Binder
import Rule.DecisionTree qualified as DT
import Rule.Ident
import Rule.Ident.Reify qualified as Ident
import Rule.LamKind qualified as LK
import Rule.Term qualified as TM
import Rule.Term.FreeVars qualified as TM

type SubstTerm =
  IntMap.IntMap (Either Ident TM.Term)

newtype Handle
  = Handle
  { gensymHandle :: Gensym.Handle
  }

new :: App Handle
new = do
  gensymHandle <- Gensym.new
  return $ Handle {..}

subst :: Handle -> SubstTerm -> TM.Term -> IO TM.Term
subst h sub term =
  case term of
    _ :< TM.Tau ->
      return term
    m :< TM.Var x
      | Just varOrTerm <- IntMap.lookup (Ident.toInt x) sub ->
          case varOrTerm of
            Left x' ->
              return $ m :< TM.Var x'
            Right e ->
              return e
      | otherwise ->
          return term
    _ :< TM.VarGlobal {} ->
      return term
    m :< TM.Pi impArgs expArgs t -> do
      (impArgs', sub') <- substBinder h sub impArgs
      (expArgs', sub'') <- substBinder h sub' expArgs
      t' <- subst h sub'' t
      return (m :< TM.Pi impArgs' expArgs' t')
    m :< TM.PiIntro (AttrL.Attr {lamKind}) impArgs expArgs e -> do
      let fvs = S.map Ident.toInt $ TM.freeVars term
      let subDomSet = S.fromList $ IntMap.keys sub
      if S.intersection fvs subDomSet == S.empty
        then return term
        else do
          newLamID <- liftIO $ Gensym.newCount (gensymHandle h)
          case lamKind of
            LK.Fix xt -> do
              (impArgs', sub') <- substBinder h sub impArgs
              (expArgs', sub'') <- substBinder h sub' expArgs
              ([xt'], sub''') <- substBinder h sub'' [xt]
              e' <- subst h sub''' e
              let fixAttr = AttrL.Attr {lamKind = LK.Fix xt', identity = newLamID}
              return (m :< TM.PiIntro fixAttr impArgs' expArgs' e')
            LK.Normal codType -> do
              (impArgs', sub') <- substBinder h sub impArgs
              (expArgs', sub'') <- substBinder h sub' expArgs
              codType' <- subst h sub'' codType
              e' <- subst h sub'' e
              let lamAttr = AttrL.Attr {lamKind = LK.Normal codType', identity = newLamID}
              return (m :< TM.PiIntro lamAttr impArgs' expArgs' e')
    m :< TM.PiElim e es -> do
      e' <- subst h sub e
      es' <- mapM (subst h sub) es
      return (m :< TM.PiElim e' es')
    m :< TM.Data attr name es -> do
      es' <- mapM (subst h sub) es
      return $ m :< TM.Data attr name es'
    m :< TM.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (subst h sub) dataArgs
      consArgs' <- mapM (subst h sub) consArgs
      return $ m :< TM.DataIntro attr consName dataArgs' consArgs'
    m :< TM.DataElim isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (subst h sub) es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      (binder', decisionTree') <- subst'' h sub binder decisionTree
      let (_, os', ts') = unzip3 binder'
      return $ m :< TM.DataElim isNoetic (zip3 os' es' ts') decisionTree'
    m :< TM.Box t -> do
      t' <- subst h sub t
      return $ m :< TM.Box t'
    m :< TM.BoxNoema t -> do
      t' <- subst h sub t
      return $ m :< TM.BoxNoema t'
    m :< TM.BoxIntro letSeq e -> do
      (letSeq', sub') <- substLetSeq h sub letSeq
      e' <- subst h sub' e
      return $ m :< TM.BoxIntro letSeq' e'
    m :< TM.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      (castSeq', sub1) <- substLetSeq h sub castSeq
      ((mxt', e1'), sub2) <- substLet h sub1 (mxt, e1)
      (uncastSeq', sub3) <- substLetSeq h sub2 uncastSeq
      e2' <- subst h sub3 e2
      return $ m :< TM.BoxElim castSeq' mxt' e1' uncastSeq' e2'
    m :< TM.Let opacity mxt e1 e2 -> do
      e1' <- subst h sub e1
      ([mxt'], e2') <- subst' h sub [mxt] e2
      return $ m :< TM.Let opacity mxt' e1' e2'
    _ :< TM.Prim _ ->
      return term
    m :< TM.Magic der -> do
      der' <- traverse (subst h sub) der
      return (m :< TM.Magic der')
    m :< TM.Resource dd resourceID unitType discarder copier -> do
      unitType' <- subst h sub unitType
      discarder' <- subst h sub discarder
      copier' <- subst h sub copier
      return $ m :< TM.Resource dd resourceID unitType' discarder' copier'
    _ :< TM.Void ->
      return term

substBinder ::
  Handle ->
  SubstTerm ->
  [BinderF TM.Term] ->
  IO ([BinderF TM.Term], SubstTerm)
substBinder h sub binder =
  case binder of
    [] -> do
      return ([], sub)
    ((m, x, t) : xts) -> do
      t' <- subst h sub t
      x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
      let sub' = IntMap.insert (Ident.toInt x) (Left x') sub
      (xts', sub'') <- substBinder h sub' xts
      return ((m, x', t') : xts', sub'')

subst' ::
  Handle ->
  SubstTerm ->
  [BinderF TM.Term] ->
  TM.Term ->
  IO ([BinderF TM.Term], TM.Term)
subst' h sub binder e =
  case binder of
    [] -> do
      e' <- subst h sub e
      return ([], e')
    ((m, x, t) : xts) -> do
      t' <- subst h sub t
      x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
      let sub' = IntMap.insert (Ident.toInt x) (Left x') sub
      (xts', e') <- subst' h sub' xts e
      return ((m, x', t') : xts', e')

subst'' ::
  Handle ->
  SubstTerm ->
  [BinderF TM.Term] ->
  DT.DecisionTree TM.Term ->
  IO ([BinderF TM.Term], DT.DecisionTree TM.Term)
subst'' h sub binder decisionTree =
  case binder of
    [] -> do
      decisionTree' <- substDecisionTree h sub decisionTree
      return ([], decisionTree')
    ((m, x, t) : xts) -> do
      t' <- subst h sub t
      x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
      let sub' = IntMap.insert (Ident.toInt x) (Left x') sub
      (xts', e') <- subst'' h sub' xts decisionTree
      return ((m, x', t') : xts', e')

substLet ::
  Handle ->
  SubstTerm ->
  (BinderF TM.Term, TM.Term) ->
  IO ((BinderF TM.Term, TM.Term), SubstTerm)
substLet h sub ((m, x, t), e) = do
  e' <- subst h sub e
  t' <- subst h sub t
  x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
  let sub' = IntMap.insert (Ident.toInt x) (Left x') sub
  return (((m, x', t'), e'), sub')

substLetSeq ::
  Handle ->
  SubstTerm ->
  [(BinderF TM.Term, TM.Term)] ->
  IO ([(BinderF TM.Term, TM.Term)], SubstTerm)
substLetSeq h sub letSeq = do
  case letSeq of
    [] ->
      return ([], sub)
    letPair : rest -> do
      (letPair', sub') <- substLet h sub letPair
      (rest', sub'') <- substLetSeq h sub' rest
      return (letPair' : rest', sub'')

substDecisionTree ::
  Handle ->
  SubstTerm ->
  DT.DecisionTree TM.Term ->
  IO (DT.DecisionTree TM.Term)
substDecisionTree h sub tree =
  case tree of
    DT.Leaf xs letSeq e -> do
      let xs' = mapMaybe (substLeafVar sub) xs
      (letSeq', sub') <- substLetSeq h sub letSeq
      e' <- subst h sub' e
      return $ DT.Leaf xs' letSeq' e'
    DT.Unreachable ->
      return tree
    DT.Switch (cursorVar, cursor) caseList -> do
      let cursorVar' = substVar sub cursorVar
      cursor' <- subst h sub cursor
      caseList' <- substCaseList h sub caseList
      return $ DT.Switch (cursorVar', cursor') caseList'

substCaseList ::
  Handle ->
  SubstTerm ->
  DT.CaseList TM.Term ->
  IO (DT.CaseList TM.Term)
substCaseList h sub (fallbackClause, clauseList) = do
  fallbackClause' <- substDecisionTree h sub fallbackClause
  clauseList' <- mapM (substCase h sub) clauseList
  return (fallbackClause', clauseList')

substCase ::
  Handle ->
  SubstTerm ->
  DT.Case TM.Term ->
  IO (DT.Case TM.Term)
substCase h sub decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      cont' <- substDecisionTree h sub cont
      return $ DT.LiteralCase mPat i cont'
    DT.ConsCase record@(DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      dataTerms' <- mapM (subst h sub) dataTerms
      dataTypes' <- mapM (subst h sub) dataTypes
      (consArgs', cont') <- subst'' h sub consArgs cont
      return $
        DT.ConsCase
          record
            { DT.dataArgs = zip dataTerms' dataTypes',
              DT.consArgs = consArgs',
              DT.cont = cont'
            }

substLeafVar :: SubstTerm -> Ident -> Maybe Ident
substLeafVar sub leafVar =
  case IntMap.lookup (Ident.toInt leafVar) sub of
    Just (Left leafVar') ->
      return leafVar'
    Just (Right _) ->
      Nothing
    Nothing ->
      return leafVar

substVar :: SubstTerm -> Ident -> Ident
substVar sub x =
  case IntMap.lookup (Ident.toInt x) sub of
    Just (Left x') ->
      x'
    _ ->
      x
