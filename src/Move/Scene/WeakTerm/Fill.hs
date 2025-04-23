module Move.Scene.WeakTerm.Fill
  ( Handle,
    new,
    fill,
  )
where

import Control.Comonad.Cofree
import Control.Monad
import Data.Bitraversable (bimapM)
import Data.IntMap qualified as IntMap
import Data.Maybe
import Move.Context.App
import Move.Context.EIO (EIO)
import Move.Scene.WeakTerm.Reduce qualified as Reduce
import Move.Scene.WeakTerm.Subst qualified as Subst
import Rule.Annotation qualified as AN
import Rule.Attr.Lam qualified as AttrL
import Rule.Binder
import Rule.DecisionTree qualified as DT
import Rule.HoleSubst
import Rule.Ident.Reify qualified as Ident
import Rule.LamKind qualified as LK
import Rule.WeakTerm qualified as WT
import Rule.WeakTerm.ToText (toText)
import Prelude hiding (lookup)

data Handle
  = Handle
  { holeSubst :: HoleSubst,
    substHandle :: Subst.Handle,
    reduceHandle :: Reduce.Handle
  }

new :: HoleSubst -> App Handle
new holeSubst = do
  substHandle <- Subst.new
  reduceHandle <- Reduce.new
  return $ Handle {..}

fill :: Handle -> WT.WeakTerm -> EIO WT.WeakTerm
fill h term =
  case term of
    _ :< WT.Tau ->
      return term
    _ :< WT.Var {} ->
      return term
    _ :< WT.VarGlobal {} ->
      return term
    m :< WT.Pi impArgs expArgs t -> do
      impArgs' <- fillBinder h impArgs
      expArgs' <- fillBinder h expArgs
      t' <- fill h t
      return $ m :< WT.Pi impArgs' expArgs' t'
    m :< WT.PiIntro attr@(AttrL.Attr {lamKind}) impArgs expArgs e -> do
      impArgs' <- fillBinder h impArgs
      expArgs' <- fillBinder h expArgs
      case lamKind of
        LK.Fix xt -> do
          [xt'] <- fillBinder h [xt]
          e' <- fill h e
          return $ m :< WT.PiIntro (attr {AttrL.lamKind = LK.Fix xt'}) impArgs' expArgs' e'
        _ -> do
          e' <- fill h e
          return $ m :< WT.PiIntro attr impArgs' expArgs' e'
    m :< WT.PiElim e es -> do
      e' <- fill h e
      es' <- mapM (fill h) es
      return $ m :< WT.PiElim e' es'
    m :< WT.PiElimExact e -> do
      e' <- fill h e
      return $ m :< WT.PiElimExact e'
    m :< WT.Data name consNameList es -> do
      es' <- mapM (fill h) es
      return $ m :< WT.Data name consNameList es'
    m :< WT.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (fill h) dataArgs
      consArgs' <- mapM (fill h) consArgs
      return $ m :< WT.DataIntro attr consName dataArgs' consArgs'
    m :< WT.DataElim isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (fill h) es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      (binder', decisionTree') <- fill''' h binder decisionTree
      let (_, os', ts') = unzip3 binder'
      return $ m :< WT.DataElim isNoetic (zip3 os' es' ts') decisionTree'
    m :< WT.Box t -> do
      t' <- fill h t
      return $ m :< WT.Box t'
    m :< WT.BoxNoema t -> do
      t' <- fill h t
      return $ m :< WT.BoxNoema t'
    m :< WT.BoxIntro letSeq e -> do
      let (xts, es) = unzip letSeq
      xts' <- fillBinder h xts
      es' <- mapM (fill h) es
      e' <- fill h e
      return $ m :< WT.BoxIntro (zip xts' es') e'
    m :< WT.BoxIntroQuote e -> do
      e' <- fill h e
      return $ m :< WT.BoxIntroQuote e'
    m :< WT.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      castSeq' <- fillLetSeq h castSeq
      (mxt', e1') <- fillLet h (mxt, e1)
      uncastSeq' <- fillLetSeq h uncastSeq
      e2' <- fill h e2
      return $ m :< WT.BoxElim castSeq' mxt' e1' uncastSeq' e2'
    m :< WT.Actual e -> do
      e' <- fill h e
      return $ m :< WT.Actual e'
    m :< WT.Let opacity mxt e1 e2 -> do
      e1' <- fill h e1
      (mxt', _, e2') <- fill'' h mxt [] e2
      return $ m :< WT.Let opacity mxt' e1' e2'
    m :< WT.Prim prim -> do
      prim' <- mapM (fill h) prim
      return $ m :< WT.Prim prim'
    m :< WT.Hole i es -> do
      es' <- mapM (fill h) es
      case lookup i (holeSubst h) of
        Just (xs, body)
          | length xs == length es -> do
              let varList = map Ident.toInt xs
              Subst.subst (substHandle h) (IntMap.fromList $ zip varList (map Right es')) body
                >>= Reduce.reduce (reduceHandle h)
          | otherwise -> do
              error $ "Rule.WeakTerm.Fill (assertion failure; arity mismatch)\n" ++ show xs ++ "\n" ++ show (map toText es') ++ "\nhole id = " ++ show i
        Nothing ->
          return $ m :< WT.Hole i es'
    m :< WT.Magic der -> do
      der' <- mapM (fill h) der
      return $ m :< WT.Magic der'
    m :< WT.Annotation logLevel annot e -> do
      e' <- fill h e
      case annot of
        AN.Type t -> do
          t' <- fill h t
          return $ m :< WT.Annotation logLevel (AN.Type t') e'
    m :< WT.Resource dd resourceID unitType discarder copier -> do
      unitType' <- fill h unitType
      discarder' <- fill h discarder
      copier' <- fill h copier
      return $ m :< WT.Resource dd resourceID unitType' discarder' copier'
    m :< WT.Use e xts cont -> do
      e' <- fill h e
      xts' <- fillBinder h xts
      cont' <- fill h cont
      return $ m :< WT.Use e' xts' cont'
    _ :< WT.Void ->
      return term

fillBinder ::
  Handle ->
  [BinderF WT.WeakTerm] ->
  EIO [BinderF WT.WeakTerm]
fillBinder h binder =
  case binder of
    [] -> do
      return []
    (m, x, t) : xts -> do
      t' <- fill h t
      xts' <- fillBinder h xts
      return $ (m, x, t') : xts'

fillLet ::
  Handle ->
  (BinderF WT.WeakTerm, WT.WeakTerm) ->
  EIO (BinderF WT.WeakTerm, WT.WeakTerm)
fillLet h ((m, x, t), e) = do
  e' <- fill h e
  t' <- fill h t
  return ((m, x, t'), e')

fillLetSeq ::
  Handle ->
  [(BinderF WT.WeakTerm, WT.WeakTerm)] ->
  EIO [(BinderF WT.WeakTerm, WT.WeakTerm)]
fillLetSeq h letSeq = do
  case letSeq of
    [] ->
      return []
    letPair : rest -> do
      letPair' <- fillLet h letPair
      rest' <- fillLetSeq h rest
      return $ letPair' : rest'

fillSingleBinder ::
  Handle ->
  BinderF WT.WeakTerm ->
  EIO (BinderF WT.WeakTerm)
fillSingleBinder h (m, x, t) = do
  t' <- fill h t
  return (m, x, t')

fill' ::
  Handle ->
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  EIO ([BinderF WT.WeakTerm], WT.WeakTerm)
fill' h binder e =
  case binder of
    [] -> do
      e' <- fill h e
      return ([], e')
    (m, x, t) : xts -> do
      (xts', e') <- fill' h xts e
      t' <- fill h t
      return ((m, x, t') : xts', e')

fill'' ::
  Handle ->
  BinderF WT.WeakTerm ->
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  EIO (BinderF WT.WeakTerm, [BinderF WT.WeakTerm], WT.WeakTerm)
fill'' h (m, x, t) binder e = do
  (xts', e') <- fill' h binder e
  t' <- fill h t
  return ((m, x, t'), xts', e')

fill''' ::
  Handle ->
  [BinderF WT.WeakTerm] ->
  DT.DecisionTree WT.WeakTerm ->
  EIO ([BinderF WT.WeakTerm], DT.DecisionTree WT.WeakTerm)
fill''' h binder decisionTree =
  case binder of
    [] -> do
      decisionTree' <- fillDecisionTree h decisionTree
      return ([], decisionTree')
    ((m, x, t) : xts) -> do
      t' <- fill h t
      (xts', e') <- fill''' h xts decisionTree
      return ((m, x, t') : xts', e')

fillDecisionTree ::
  Handle ->
  DT.DecisionTree WT.WeakTerm ->
  EIO (DT.DecisionTree WT.WeakTerm)
fillDecisionTree h tree =
  case tree of
    DT.Leaf xs letSeq e -> do
      letSeq' <- mapM (bimapM (fillSingleBinder h) (fill h)) letSeq
      e' <- fill h e
      return $ DT.Leaf xs letSeq' e'
    DT.Unreachable ->
      return tree
    DT.Switch (cursorVar, cursor) caseList -> do
      cursor' <- fill h cursor
      caseList' <- fillCaseList h caseList
      return $ DT.Switch (cursorVar, cursor') caseList'

fillCaseList ::
  Handle ->
  DT.CaseList WT.WeakTerm ->
  EIO (DT.CaseList WT.WeakTerm)
fillCaseList h (fallbackClause, clauseList) = do
  fallbackClause' <- fillDecisionTree h fallbackClause
  clauseList' <- mapM (fillCase h) clauseList
  return (fallbackClause', clauseList')

fillCase ::
  Handle ->
  DT.Case WT.WeakTerm ->
  EIO (DT.Case WT.WeakTerm)
fillCase h decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      cont' <- fillDecisionTree h cont
      return $ DT.LiteralCase mPat i cont'
    DT.ConsCase record@(DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      dataTerms' <- mapM (fill h) dataTerms
      dataTypes' <- mapM (fill h) dataTypes
      (consArgs', cont') <- fill''' h consArgs cont
      return $
        DT.ConsCase
          record
            { DT.dataArgs = zip dataTerms' dataTypes',
              DT.consArgs = consArgs',
              DT.cont = cont'
            }
