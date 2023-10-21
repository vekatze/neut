module Entity.Term.Chain
  ( chainOf,
    chainOfClauseList,
  )
where

import Control.Comonad.Cofree
import Data.Containers.ListUtils qualified as ListUtils
import Data.IntMap qualified as IntMap
import Data.Maybe
import Entity.Binder
import Entity.DecisionTree qualified as DT
import Entity.Hint
import Entity.Ident
import Entity.Ident.Reify qualified as Ident
import Entity.LamKind qualified as LK
import Entity.Term qualified as TM

chainOf :: TM.TypeEnv -> [TM.Term] -> [BinderF TM.Term]
chainOf tenv term =
  nubFreeVariables $ concatMap (chainOf' tenv) term

chainOfClauseList :: TM.TypeEnv -> Hint -> DT.CaseList TM.Term -> [BinderF TM.Term]
chainOfClauseList tenv m clauseList =
  nubFreeVariables $ chainOfCaseList tenv m clauseList

chainOf' :: TM.TypeEnv -> TM.Term -> [BinderF TM.Term]
chainOf' tenv term =
  case term of
    _ :< TM.Tau ->
      []
    m :< TM.Var x -> do
      chainOfVar tenv m x
    _ :< TM.VarGlobal {} ->
      []
    _ :< TM.Pi {} ->
      []
    _ :< TM.PiIntro kind xts e ->
      chainOfBinder tenv (catMaybes [LK.fromLamKind kind] ++ xts) [e]
    _ :< TM.PiElim e es -> do
      let xs1 = chainOf' tenv e
      let xs2 = concatMap (chainOf' tenv) es
      xs1 ++ xs2
    _ :< TM.Data _ _ es ->
      concatMap (chainOf' tenv) es
    _ :< TM.DataIntro _ _ _ _ dataArgs consArgs ->
      concatMap (chainOf' tenv) $ dataArgs ++ consArgs
    m :< TM.DataElim _ xets tree -> do
      let (xs, es, ts) = unzip3 xets
      let xs1 = concatMap (chainOf' tenv) es
      let mxts = zipWith (\x t -> (m, x, t)) xs ts
      let xs2 = chainOfDecisionTree' tenv m mxts tree
      xs1 ++ xs2
    _ :< TM.Noema t ->
      chainOf' tenv t
    _ :< TM.Embody t e -> do
      let xs1 = chainOf' tenv t
      let xs2 = chainOf' tenv e
      xs1 ++ xs2
    _ :< TM.Let _ mxt e1 e2 -> do
      let xs1 = chainOf' tenv e1
      let xs2 = chainOfBinder tenv [mxt] [e2]
      xs1 ++ xs2
    _ :< TM.Prim _ ->
      []
    _ :< TM.ResourceType {} ->
      []
    _ :< TM.Magic der ->
      foldMap (chainOf' tenv) der
    _ :< TM.Flow _ t ->
      chainOf' tenv t
    _ :< TM.FlowIntro _ _ (e, t) ->
      concatMap (chainOf' tenv) [e, t]
    _ :< TM.FlowElim _ _ (e, t) ->
      concatMap (chainOf' tenv) [e, t]

chainOfBinder :: TM.TypeEnv -> [BinderF TM.Term] -> [TM.Term] -> [BinderF TM.Term]
chainOfBinder tenv binder es =
  chainOfBinder' tenv binder $ \tenv' -> concatMap (chainOf' tenv') es

chainOfBinder' :: TM.TypeEnv -> [BinderF TM.Term] -> (TM.TypeEnv -> [BinderF TM.Term]) -> [BinderF TM.Term]
chainOfBinder' tenv mxts f =
  case mxts of
    [] ->
      f tenv
    (mxt@(_, x, t) : xts) -> do
      let hs1 = chainOf' tenv t
      let hs2 = chainOfBinder' (TM.insTypeEnv [mxt] tenv) xts f
      hs1 ++ filter (\(_, y, _) -> y /= x) hs2

chainOfDecisionTree :: TM.TypeEnv -> Hint -> DT.DecisionTree TM.Term -> [BinderF TM.Term]
chainOfDecisionTree tenv m tree =
  case tree of
    DT.Leaf xs e -> do
      concatMap (chainOfVar tenv m) xs ++ chainOf' tenv e
    DT.Unreachable ->
      []
    DT.Switch (cursor, _) caseList ->
      -- the cursor must be treated as an immediate
      (m, cursor, m :< TM.Tau) : chainOfCaseList tenv m caseList

chainOfDecisionTree' :: TM.TypeEnv -> Hint -> [BinderF TM.Term] -> DT.DecisionTree TM.Term -> [BinderF TM.Term]
chainOfDecisionTree' tenv m xts tree =
  chainOfBinder' tenv xts $ \tenv' -> chainOfDecisionTree tenv' m tree

chainOfCaseList :: TM.TypeEnv -> Hint -> DT.CaseList TM.Term -> [BinderF TM.Term]
chainOfCaseList tenv m (fallbackClause, clauseList) = do
  let xs1 = chainOfDecisionTree tenv m fallbackClause
  let xs2 = concatMap (chainOfCase tenv m) clauseList
  xs1 ++ xs2

chainOfCase :: TM.TypeEnv -> Hint -> DT.Case TM.Term -> [BinderF TM.Term]
chainOfCase tenv m decisionCase = do
  let (dataTerms, dataTypes) = unzip $ DT.dataArgs decisionCase
  let xs1 = concatMap (chainOf' tenv) dataTerms
  let xs2 = concatMap (chainOf' tenv) dataTypes
  let xs3 = chainOfDecisionTree' tenv m (DT.consArgs decisionCase) (DT.cont decisionCase)
  xs1 ++ xs2 ++ xs3

nubFreeVariables :: [BinderF TM.Term] -> [BinderF TM.Term]
nubFreeVariables =
  ListUtils.nubOrdOn (\(_, x, _) -> x)

chainOfVar :: TM.TypeEnv -> Hint -> Ident -> [BinderF TM.Term]
chainOfVar tenv m x = do
  let t = (IntMap.!) tenv (Ident.toInt x)
  let xts = chainOf' tenv t
  xts ++ [(m, x, t)]
