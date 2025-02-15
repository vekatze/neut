module Scene.WeakTerm.Fill
  ( fill,
  )
where

import Context.App
import Control.Comonad.Cofree
import Control.Monad
import Data.Bitraversable (bimapM)
import Data.IntMap qualified as IntMap
import Data.Maybe
import Entity.Annotation qualified as AN
import Entity.Attr.Lam qualified as AttrL
import Entity.Binder
import Entity.DecisionTree qualified as DT
import Entity.HoleSubst
import Entity.Ident.Reify qualified as Ident
import Entity.LamKind qualified as LK
import Entity.WeakTerm qualified as WT
import Entity.WeakTerm.ToText (toText)
import Scene.WeakTerm.Reduce
import Scene.WeakTerm.Subst
import Prelude hiding (lookup)

fill :: HoleSubst -> WT.WeakTerm -> App WT.WeakTerm
fill sub term =
  case term of
    _ :< WT.Tau ->
      return term
    _ :< WT.Var {} ->
      return term
    _ :< WT.VarGlobal {} ->
      return term
    m :< WT.Pi impArgs expArgs t -> do
      impArgs' <- fillBinder sub impArgs
      expArgs' <- fillBinder sub expArgs
      t' <- fill sub t
      return $ m :< WT.Pi impArgs' expArgs' t'
    m :< WT.PiIntro attr@(AttrL.Attr {lamKind}) impArgs expArgs e -> do
      impArgs' <- fillBinder sub impArgs
      expArgs' <- fillBinder sub expArgs
      case lamKind of
        LK.Fix xt -> do
          [xt'] <- fillBinder sub [xt]
          e' <- fill sub e
          return $ m :< WT.PiIntro (attr {AttrL.lamKind = LK.Fix xt'}) impArgs' expArgs' e'
        _ -> do
          e' <- fill sub e
          return $ m :< WT.PiIntro attr impArgs' expArgs' e'
    m :< WT.PiElim e es -> do
      e' <- fill sub e
      es' <- mapM (fill sub) es
      return $ m :< WT.PiElim e' es'
    m :< WT.PiElimExact e -> do
      e' <- fill sub e
      return $ m :< WT.PiElimExact e'
    m :< WT.Data name consNameList es -> do
      es' <- mapM (fill sub) es
      return $ m :< WT.Data name consNameList es'
    m :< WT.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (fill sub) dataArgs
      consArgs' <- mapM (fill sub) consArgs
      return $ m :< WT.DataIntro attr consName dataArgs' consArgs'
    m :< WT.DataElim isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (fill sub) es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      (binder', decisionTree') <- fill''' sub binder decisionTree
      let (_, os', ts') = unzip3 binder'
      return $ m :< WT.DataElim isNoetic (zip3 os' es' ts') decisionTree'
    m :< WT.Box t -> do
      t' <- fill sub t
      return $ m :< WT.Box t'
    m :< WT.BoxNoema t -> do
      t' <- fill sub t
      return $ m :< WT.BoxNoema t'
    m :< WT.BoxIntro letSeq e -> do
      let (xts, es) = unzip letSeq
      xts' <- fillBinder sub xts
      es' <- mapM (fill sub) es
      e' <- fill sub e
      return $ m :< WT.BoxIntro (zip xts' es') e'
    m :< WT.BoxIntroQuote e -> do
      e' <- fill sub e
      return $ m :< WT.BoxIntroQuote e'
    m :< WT.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      castSeq' <- fillLetSeq sub castSeq
      (mxt', e1') <- fillLet sub (mxt, e1)
      uncastSeq' <- fillLetSeq sub uncastSeq
      e2' <- fill sub e2
      return $ m :< WT.BoxElim castSeq' mxt' e1' uncastSeq' e2'
    m :< WT.Actual e -> do
      e' <- fill sub e
      return $ m :< WT.Actual e'
    m :< WT.Let opacity mxt e1 e2 -> do
      e1' <- fill sub e1
      (mxt', _, e2') <- fill'' sub mxt [] e2
      return $ m :< WT.Let opacity mxt' e1' e2'
    m :< WT.Prim prim -> do
      prim' <- mapM (fill sub) prim
      return $ m :< WT.Prim prim'
    m :< WT.Hole i es -> do
      es' <- mapM (fill sub) es
      case lookup i sub of
        Just (xs, body)
          | length xs == length es -> do
              let varList = map Ident.toInt xs
              subst (IntMap.fromList $ zip varList (map Right es')) body >>= reduce
          | otherwise -> do
              error $ "Entity.WeakTerm.Fill (assertion failure; arity mismatch)\n" ++ show xs ++ "\n" ++ show (map toText es') ++ "\nhole id = " ++ show i
        Nothing ->
          return $ m :< WT.Hole i es'
    m :< WT.Magic der -> do
      der' <- mapM (fill sub) der
      return $ m :< WT.Magic der'
    m :< WT.Annotation logLevel annot e -> do
      e' <- fill sub e
      case annot of
        AN.Type t -> do
          t' <- fill sub t
          return $ m :< WT.Annotation logLevel (AN.Type t') e'
    m :< WT.Resource dd resourceID unitType discarder copier -> do
      unitType' <- fill sub unitType
      discarder' <- fill sub discarder
      copier' <- fill sub copier
      return $ m :< WT.Resource dd resourceID unitType' discarder' copier'
    m :< WT.Use e xts cont -> do
      e' <- fill sub e
      xts' <- fillBinder sub xts
      cont' <- fill sub cont
      return $ m :< WT.Use e' xts' cont'
    _ :< WT.Void ->
      return term

fillBinder ::
  HoleSubst ->
  [BinderF WT.WeakTerm] ->
  App [BinderF WT.WeakTerm]
fillBinder sub binder =
  case binder of
    [] -> do
      return []
    (m, x, t) : xts -> do
      t' <- fill sub t
      xts' <- fillBinder sub xts
      return $ (m, x, t') : xts'

fillLet ::
  HoleSubst ->
  (BinderF WT.WeakTerm, WT.WeakTerm) ->
  App (BinderF WT.WeakTerm, WT.WeakTerm)
fillLet sub ((m, x, t), e) = do
  e' <- fill sub e
  t' <- fill sub t
  return ((m, x, t'), e')

fillLetSeq ::
  HoleSubst ->
  [(BinderF WT.WeakTerm, WT.WeakTerm)] ->
  App [(BinderF WT.WeakTerm, WT.WeakTerm)]
fillLetSeq sub letSeq = do
  case letSeq of
    [] ->
      return []
    letPair : rest -> do
      letPair' <- fillLet sub letPair
      rest' <- fillLetSeq sub rest
      return $ letPair' : rest'

fillSingleBinder ::
  HoleSubst ->
  BinderF WT.WeakTerm ->
  App (BinderF WT.WeakTerm)
fillSingleBinder sub (m, x, t) = do
  t' <- fill sub t
  return (m, x, t')

fill' ::
  HoleSubst ->
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  App ([BinderF WT.WeakTerm], WT.WeakTerm)
fill' sub binder e =
  case binder of
    [] -> do
      e' <- fill sub e
      return ([], e')
    (m, x, t) : xts -> do
      (xts', e') <- fill' sub xts e
      t' <- fill sub t
      return ((m, x, t') : xts', e')

fill'' ::
  HoleSubst ->
  BinderF WT.WeakTerm ->
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  App (BinderF WT.WeakTerm, [BinderF WT.WeakTerm], WT.WeakTerm)
fill'' sub (m, x, t) binder e = do
  (xts', e') <- fill' sub binder e
  t' <- fill sub t
  return ((m, x, t'), xts', e')

fill''' ::
  HoleSubst ->
  [BinderF WT.WeakTerm] ->
  DT.DecisionTree WT.WeakTerm ->
  App ([BinderF WT.WeakTerm], DT.DecisionTree WT.WeakTerm)
fill''' sub binder decisionTree =
  case binder of
    [] -> do
      decisionTree' <- fillDecisionTree sub decisionTree
      return ([], decisionTree')
    ((m, x, t) : xts) -> do
      t' <- fill sub t
      (xts', e') <- fill''' sub xts decisionTree
      return ((m, x, t') : xts', e')

fillDecisionTree ::
  HoleSubst ->
  DT.DecisionTree WT.WeakTerm ->
  App (DT.DecisionTree WT.WeakTerm)
fillDecisionTree sub tree =
  case tree of
    DT.Leaf xs letSeq e -> do
      letSeq' <- mapM (bimapM (fillSingleBinder sub) (fill sub)) letSeq
      e' <- fill sub e
      return $ DT.Leaf xs letSeq' e'
    DT.Unreachable ->
      return tree
    DT.Switch (cursorVar, cursor) caseList -> do
      cursor' <- fill sub cursor
      caseList' <- fillCaseList sub caseList
      return $ DT.Switch (cursorVar, cursor') caseList'

fillCaseList ::
  HoleSubst ->
  DT.CaseList WT.WeakTerm ->
  App (DT.CaseList WT.WeakTerm)
fillCaseList sub (fallbackClause, clauseList) = do
  fallbackClause' <- fillDecisionTree sub fallbackClause
  clauseList' <- mapM (fillCase sub) clauseList
  return (fallbackClause', clauseList')

fillCase ::
  HoleSubst ->
  DT.Case WT.WeakTerm ->
  App (DT.Case WT.WeakTerm)
fillCase sub decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      cont' <- fillDecisionTree sub cont
      return $ DT.LiteralCase mPat i cont'
    DT.ConsCase record@(DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      dataTerms' <- mapM (fill sub) dataTerms
      dataTypes' <- mapM (fill sub) dataTypes
      (consArgs', cont') <- fill''' sub consArgs cont
      return $
        DT.ConsCase
          record
            { DT.dataArgs = zip dataTerms' dataTypes',
              DT.consArgs = consArgs',
              DT.cont = cont'
            }
