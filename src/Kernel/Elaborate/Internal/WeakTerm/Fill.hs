module Kernel.Elaborate.Internal.WeakTerm.Fill
  ( Handle,
    new,
    fill,
  )
where

import App.App (App)
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class
import Data.Bitraversable (bimapM)
import Data.IntMap qualified as IntMap
import Data.Maybe
import Kernel.Elaborate.HoleSubst
import Language.Common.Annotation qualified as AN
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.Binder
import Language.Common.DecisionTree qualified as DT
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.ImpArgs qualified as ImpArgs
import Language.Common.LamKind qualified as LK
import Language.WeakTerm.Reduce qualified as Reduce
import Language.WeakTerm.Subst qualified as Subst
import Language.WeakTerm.ToText (toText)
import Language.WeakTerm.WeakTerm qualified as WT
import Prelude hiding (lookup)

data Handle = Handle
  { substHandle :: Subst.Handle,
    reduceHandle :: Reduce.Handle
  }

new :: Subst.Handle -> Reduce.Handle -> Handle
new substHandle reduceHandle = do
  Handle {..}

fill :: Handle -> HoleSubst -> WT.WeakTerm -> App WT.WeakTerm
fill h holeSubst term =
  case term of
    _ :< WT.Tau ->
      return term
    _ :< WT.Var {} ->
      return term
    _ :< WT.VarGlobal {} ->
      return term
    m :< WT.Pi piKind impArgs expArgs t -> do
      impArgs' <- fillBinderWithMaybeType h holeSubst impArgs
      expArgs' <- fillBinder h holeSubst expArgs
      t' <- fill h holeSubst t
      return $ m :< WT.Pi piKind impArgs' expArgs' t'
    m :< WT.PiIntro attr@(AttrL.Attr {lamKind}) impArgs expArgs e -> do
      impArgs' <- fillBinderWithMaybeType h holeSubst impArgs
      expArgs' <- fillBinder h holeSubst expArgs
      case lamKind of
        LK.Fix xt -> do
          [xt'] <- fillBinder h holeSubst [xt]
          e' <- fill h holeSubst e
          return $ m :< WT.PiIntro (attr {AttrL.lamKind = LK.Fix xt'}) impArgs' expArgs' e'
        _ -> do
          e' <- fill h holeSubst e
          return $ m :< WT.PiIntro attr impArgs' expArgs' e'
    m :< WT.PiElim b e impArgs expArgs -> do
      e' <- fill h holeSubst e
      impArgs' <- ImpArgs.traverseImpArgs (fill h holeSubst) impArgs
      expArgs' <- mapM (fill h holeSubst) expArgs
      return $ m :< WT.PiElim b e' impArgs' expArgs'
    m :< WT.PiElimExact e -> do
      e' <- fill h holeSubst e
      return $ m :< WT.PiElimExact e'
    m :< WT.Data name consNameList es -> do
      es' <- mapM (fill h holeSubst) es
      return $ m :< WT.Data name consNameList es'
    m :< WT.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (fill h holeSubst) dataArgs
      consArgs' <- mapM (fill h holeSubst) consArgs
      return $ m :< WT.DataIntro attr consName dataArgs' consArgs'
    m :< WT.DataElim isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (fill h holeSubst) es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      (binder', decisionTree') <- fill''' h holeSubst binder decisionTree
      let (_, os', ts') = unzip3 binder'
      return $ m :< WT.DataElim isNoetic (zip3 os' es' ts') decisionTree'
    m :< WT.Box t -> do
      t' <- fill h holeSubst t
      return $ m :< WT.Box t'
    m :< WT.BoxNoema t -> do
      t' <- fill h holeSubst t
      return $ m :< WT.BoxNoema t'
    m :< WT.BoxIntro letSeq e -> do
      let (xts, es) = unzip letSeq
      xts' <- fillBinder h holeSubst xts
      es' <- mapM (fill h holeSubst) es
      e' <- fill h holeSubst e
      return $ m :< WT.BoxIntro (zip xts' es') e'
    m :< WT.BoxIntroQuote e -> do
      e' <- fill h holeSubst e
      return $ m :< WT.BoxIntroQuote e'
    m :< WT.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      castSeq' <- fillLetSeq h holeSubst castSeq
      (mxt', e1') <- fillLet h holeSubst (mxt, e1)
      uncastSeq' <- fillLetSeq h holeSubst uncastSeq
      e2' <- fill h holeSubst e2
      return $ m :< WT.BoxElim castSeq' mxt' e1' uncastSeq' e2'
    m :< WT.Actual e -> do
      e' <- fill h holeSubst e
      return $ m :< WT.Actual e'
    m :< WT.Let opacity mxt e1 e2 -> do
      e1' <- fill h holeSubst e1
      (mxt', _, e2') <- fill'' h holeSubst mxt [] e2
      return $ m :< WT.Let opacity mxt' e1' e2'
    m :< WT.Prim prim -> do
      prim' <- mapM (fill h holeSubst) prim
      return $ m :< WT.Prim prim'
    m :< WT.Hole i es -> do
      es' <- mapM (fill h holeSubst) es
      case lookup i holeSubst of
        Just (xs, body)
          | length xs == length es -> do
              let varList = map Ident.toInt xs
              liftIO (Subst.subst (substHandle h) (IntMap.fromList $ zip varList (map Right es')) body)
                >>= Reduce.reduce (reduceHandle h)
          | otherwise -> do
              error $ "Rule.WeakTerm.Fill (assertion failure; arity mismatch)\n" ++ show xs ++ "\n" ++ show (map toText es') ++ "\nhole id = " ++ show i
        Nothing ->
          return $ m :< WT.Hole i es'
    m :< WT.Magic der -> do
      der' <- mapM (fill h holeSubst) der
      return $ m :< WT.Magic der'
    m :< WT.Annotation logLevel annot e -> do
      e' <- fill h holeSubst e
      case annot of
        AN.Type t -> do
          t' <- fill h holeSubst t
          return $ m :< WT.Annotation logLevel (AN.Type t') e'
    m :< WT.Resource dd resourceID unitType discarder copier typeTag -> do
      unitType' <- fill h holeSubst unitType
      discarder' <- fill h holeSubst discarder
      copier' <- fill h holeSubst copier
      typeTag' <- fill h holeSubst typeTag
      return $ m :< WT.Resource dd resourceID unitType' discarder' copier' typeTag'
    _ :< WT.Void ->
      return term

fillBinder ::
  Handle ->
  HoleSubst ->
  [BinderF WT.WeakTerm] ->
  App [BinderF WT.WeakTerm]
fillBinder h holeSubst binder =
  case binder of
    [] -> do
      return []
    (m, x, t) : xts -> do
      t' <- fill h holeSubst t
      xts' <- fillBinder h holeSubst xts
      return $ (m, x, t') : xts'

fillBinderWithMaybeType ::
  Handle ->
  HoleSubst ->
  [(BinderF WT.WeakTerm, Maybe WT.WeakTerm)] ->
  App [(BinderF WT.WeakTerm, Maybe WT.WeakTerm)]
fillBinderWithMaybeType h holeSubst binderList =
  case binderList of
    [] -> do
      return []
    ((m, x, t), maybeType) : rest -> do
      t' <- fill h holeSubst t
      maybeType' <- traverse (fill h holeSubst) maybeType
      rest' <- fillBinderWithMaybeType h holeSubst rest
      return $ ((m, x, t'), maybeType') : rest'

fillLet ::
  Handle ->
  HoleSubst ->
  (BinderF WT.WeakTerm, WT.WeakTerm) ->
  App (BinderF WT.WeakTerm, WT.WeakTerm)
fillLet h holeSubst ((m, x, t), e) = do
  e' <- fill h holeSubst e
  t' <- fill h holeSubst t
  return ((m, x, t'), e')

fillLetSeq ::
  Handle ->
  HoleSubst ->
  [(BinderF WT.WeakTerm, WT.WeakTerm)] ->
  App [(BinderF WT.WeakTerm, WT.WeakTerm)]
fillLetSeq h holeSubst letSeq = do
  case letSeq of
    [] ->
      return []
    letPair : rest -> do
      letPair' <- fillLet h holeSubst letPair
      rest' <- fillLetSeq h holeSubst rest
      return $ letPair' : rest'

fillSingleBinder ::
  Handle ->
  HoleSubst ->
  BinderF WT.WeakTerm ->
  App (BinderF WT.WeakTerm)
fillSingleBinder h holeSubst (m, x, t) = do
  t' <- fill h holeSubst t
  return (m, x, t')

fill' ::
  Handle ->
  HoleSubst ->
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  App ([BinderF WT.WeakTerm], WT.WeakTerm)
fill' h holeSubst binder e =
  case binder of
    [] -> do
      e' <- fill h holeSubst e
      return ([], e')
    (m, x, t) : xts -> do
      (xts', e') <- fill' h holeSubst xts e
      t' <- fill h holeSubst t
      return ((m, x, t') : xts', e')

fill'' ::
  Handle ->
  HoleSubst ->
  BinderF WT.WeakTerm ->
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  App (BinderF WT.WeakTerm, [BinderF WT.WeakTerm], WT.WeakTerm)
fill'' h holeSubst (m, x, t) binder e = do
  (xts', e') <- fill' h holeSubst binder e
  t' <- fill h holeSubst t
  return ((m, x, t'), xts', e')

fill''' ::
  Handle ->
  HoleSubst ->
  [BinderF WT.WeakTerm] ->
  DT.DecisionTree WT.WeakTerm ->
  App ([BinderF WT.WeakTerm], DT.DecisionTree WT.WeakTerm)
fill''' h holeSubst binder decisionTree =
  case binder of
    [] -> do
      decisionTree' <- fillDecisionTree h holeSubst decisionTree
      return ([], decisionTree')
    ((m, x, t) : xts) -> do
      t' <- fill h holeSubst t
      (xts', e') <- fill''' h holeSubst xts decisionTree
      return ((m, x, t') : xts', e')

fillDecisionTree ::
  Handle ->
  HoleSubst ->
  DT.DecisionTree WT.WeakTerm ->
  App (DT.DecisionTree WT.WeakTerm)
fillDecisionTree h holeSubst tree =
  case tree of
    DT.Leaf xs letSeq e -> do
      letSeq' <- mapM (bimapM (fillSingleBinder h holeSubst) (fill h holeSubst)) letSeq
      e' <- fill h holeSubst e
      return $ DT.Leaf xs letSeq' e'
    DT.Unreachable ->
      return tree
    DT.Switch (cursorVar, cursor) caseList -> do
      cursor' <- fill h holeSubst cursor
      caseList' <- fillCaseList h holeSubst caseList
      return $ DT.Switch (cursorVar, cursor') caseList'

fillCaseList ::
  Handle ->
  HoleSubst ->
  DT.CaseList WT.WeakTerm ->
  App (DT.CaseList WT.WeakTerm)
fillCaseList h holeSubst (fallbackClause, clauseList) = do
  fallbackClause' <- fillDecisionTree h holeSubst fallbackClause
  clauseList' <- mapM (fillCase h holeSubst) clauseList
  return (fallbackClause', clauseList')

fillCase ::
  Handle ->
  HoleSubst ->
  DT.Case WT.WeakTerm ->
  App (DT.Case WT.WeakTerm)
fillCase h holeSubst decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      cont' <- fillDecisionTree h holeSubst cont
      return $ DT.LiteralCase mPat i cont'
    DT.ConsCase record@(DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      dataTerms' <- mapM (fill h holeSubst) dataTerms
      dataTypes' <- mapM (fill h holeSubst) dataTypes
      (consArgs', cont') <- fill''' h holeSubst consArgs cont
      return $
        DT.ConsCase
          record
            { DT.dataArgs = zip dataTerms' dataTypes',
              DT.consArgs = consArgs',
              DT.cont = cont'
            }
