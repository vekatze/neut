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
import qualified Entity.Ident.Reify as Ident
import qualified Entity.LamKind as LK
import qualified Entity.Term as TM

chainOf :: TM.TypeEnv -> [TM.Term] -> [BinderF TM.Term]
chainOf tenv term =
  nubFreeVariables $ concatMap (chainOf' tenv) term

chainOfClauseList :: TM.TypeEnv -> DT.CaseList TM.Term -> [BinderF TM.Term]
chainOfClauseList tenv clauseList =
  nubFreeVariables $ chainOfCaseList tenv clauseList

chainOf' :: TM.TypeEnv -> TM.Term -> [BinderF TM.Term]
chainOf' tenv term =
  case term of
    _ :< TM.Tau ->
      []
    m :< TM.Var x -> do
      let t = (IntMap.!) tenv (Ident.toInt x)
      let xts = chainOf' tenv t
      xts ++ [(m, x, t)]
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
    m :< TM.DataElim xets tree -> do
      let (xs, es, ts) = unzip3 xets
      let xs1 = concatMap (chainOf' tenv) es
      let mxts = zipWith (\x t -> (m, x, t)) xs ts
      let xs2 = chainOfDecisionTree' tenv mxts tree
      xs1 ++ xs2
    _ :< TM.Sigma xts ->
      chainOfBinder tenv xts []
    _ :< TM.SigmaIntro es ->
      concatMap (chainOf' tenv) es
    _ :< TM.SigmaElim xts e1 e2 -> do
      let xs1 = chainOf' tenv e1
      let xs2 = chainOfBinder tenv xts [e2]
      xs1 ++ xs2
    _ :< TM.Let mxt e1 e2 -> do
      let xs1 = chainOf' tenv e1
      let xs2 = chainOfBinder tenv [mxt] [e2]
      xs1 ++ xs2
    _ :< TM.Prim _ ->
      []
    _ :< TM.Enum {} ->
      []
    _ :< TM.EnumIntro {} ->
      []
    _ :< TM.EnumElim (e, t) les -> do
      let xs0 = chainOf' tenv t
      let xs1 = chainOf' tenv e
      let es = map snd les
      let xs2 = concatMap (chainOf' tenv) es
      xs0 ++ xs1 ++ xs2
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
    ((_, x, t) : xts) -> do
      let hs1 = chainOf' tenv t
      let hs2 = chainOfBinder' tenv xts f
      hs1 ++ filter (\(_, y, _) -> y /= x) hs2

-- S.union hs1 $ S.filter (/= x) hs2

-- insTypeEnv :: [BinderF TM.Term] -> TM.TypeEnv -> TM.TypeEnv
-- insTypeEnv xts tenv =
--   case xts of
--     [] ->
--       tenv
--     (_, x, t) : rest ->
--       insTypeEnv rest $ IntMap.insert (Ident.toInt x) t tenv

chainOfDecisionTree :: TM.TypeEnv -> DT.DecisionTree TM.Term -> [BinderF TM.Term]
chainOfDecisionTree tenv tree =
  case tree of
    DT.Leaf _ e ->
      chainOf' tenv e
    DT.Unreachable ->
      []
    DT.Switch (_, cursor) caseList ->
      chainOf' tenv cursor ++ chainOfCaseList tenv caseList

chainOfDecisionTree' :: TM.TypeEnv -> [BinderF TM.Term] -> DT.DecisionTree TM.Term -> [BinderF TM.Term]
chainOfDecisionTree' tenv xts tree =
  chainOfBinder' tenv xts $ \tenv' -> chainOfDecisionTree tenv' tree

chainOfCaseList :: TM.TypeEnv -> DT.CaseList TM.Term -> [BinderF TM.Term]
chainOfCaseList tenv (fallbackClause, clauseList) = do
  let xs1 = chainOfDecisionTree tenv fallbackClause
  let xs2 = concatMap (chainOfCase tenv) clauseList
  xs1 ++ xs2

chainOfCase :: TM.TypeEnv -> DT.Case TM.Term -> [BinderF TM.Term]
chainOfCase tenv (DT.Cons _ _ dataArgs consArgs tree) =
  concatMap (chainOf' tenv) dataArgs ++ chainOfDecisionTree' tenv consArgs tree

nubFreeVariables :: [BinderF TM.Term] -> [BinderF TM.Term]
nubFreeVariables =
  nubBy (\(_, x, _) (_, y, _) -> x == y)
