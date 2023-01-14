module Entity.Term.Chain
  ( chainOf,
    chainOfClauseList,
  )
where

import Control.Comonad.Cofree
import qualified Data.IntMap as IntMap
import Data.List
import Data.Maybe
import Entity.Binder
import qualified Entity.DecisionTree as DT
import Entity.Hint
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import qualified Entity.LamKind as LK
import qualified Entity.Term as TM

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
    -- let t = (IntMap.!) tenv (Ident.toInt x)
    -- let xts = chainOf' tenv t
    -- xts ++ [(m, x, t)]
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
    _ :< TM.Data _ es ->
      concatMap (chainOf' tenv) es
    _ :< TM.DataIntro _ _ _ dataArgs consArgs ->
      concatMap (chainOf' tenv) $ dataArgs ++ consArgs
    m :< TM.DataElim _ xets tree -> do
      let (xs, es, ts) = unzip3 xets
      let xs1 = concatMap (chainOf' tenv) es
      let mxts = zipWith (\x t -> (m, x, t)) xs ts
      let xs2 = chainOfDecisionTree' tenv m mxts tree
      xs1 ++ xs2
    _ :< TM.Noema t ->
      chainOf' tenv t
    _ :< TM.Prim _ ->
      []
    _ :< TM.Magic der ->
      foldMap (chainOf' tenv) der

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
chainOfCase tenv m (DT.Cons _ _ dataArgs consArgs tree) = do
  let (dataTerms, dataTypes) = unzip dataArgs
  let xs1 = concatMap (chainOf' tenv) dataTerms
  let xs2 = concatMap (chainOf' tenv) dataTypes
  let xs3 = chainOfDecisionTree' tenv m consArgs tree
  xs1 ++ xs2 ++ xs3

nubFreeVariables :: [BinderF TM.Term] -> [BinderF TM.Term]
nubFreeVariables =
  nubBy (\(_, x, _) (_, y, _) -> x == y)

chainOfVar :: TM.TypeEnv -> Hint -> Ident -> [BinderF TM.Term]
chainOfVar tenv m x = do
  let t = (IntMap.!) tenv (Ident.toInt x)
  let xts = chainOf' tenv t
  xts ++ [(m, x, t)]
