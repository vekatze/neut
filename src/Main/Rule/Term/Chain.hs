module Main.Rule.Term.Chain
  ( chainOf,
    chainOfDecisionTree,
    chainOfCaseWithoutCont,
    nubFreeVariables,
  )
where

import Control.Comonad.Cofree
import Data.Containers.ListUtils qualified as ListUtils
import Data.IntMap qualified as IntMap
import Data.Maybe
import Data.Text qualified as T
import Language.Common.Rule.Attr.Lam qualified as AttrL
import Language.Common.Rule.Binder
import Language.Common.Rule.DecisionTree qualified as DT
import Language.Common.Rule.Hint
import Language.Common.Rule.Hint.Reify (toString)
import Language.Common.Rule.Ident
import Language.Common.Rule.Ident.Reify qualified as Ident
import Language.Term.Rule.Term qualified as TM

chainOf :: TM.TypeEnv -> [TM.Term] -> [BinderF TM.Term]
chainOf tenv term =
  nubFreeVariables $ concatMap (chainOf' tenv) term

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
    _ :< TM.PiIntro attr impArgs expArgs e ->
      chainOfBinder tenv (impArgs ++ expArgs ++ catMaybes [AttrL.fromAttr attr]) [e]
    _ :< TM.PiElim e es -> do
      let xs1 = chainOf' tenv e
      let xs2 = concatMap (chainOf' tenv) es
      xs1 ++ xs2
    _ :< TM.Data _ _ es ->
      concatMap (chainOf' tenv) es
    _ :< TM.DataIntro _ _ dataArgs consArgs ->
      concatMap (chainOf' tenv) $ dataArgs ++ consArgs
    m :< TM.DataElim _ xets tree -> do
      let (xs, es, ts) = unzip3 xets
      let xs1 = concatMap (chainOf' tenv) es
      let mxts = zipWith (\x t -> (m, x, t)) xs ts
      let xs2 = chainOfDecisionTree' tenv m mxts tree
      xs1 ++ xs2
    _ :< TM.Box t ->
      chainOf' tenv t
    _ :< TM.BoxNoema t ->
      chainOf' tenv t
    _ :< TM.BoxIntro letSeq e -> do
      let (xts, es) = unzip letSeq
      chainOfBinder tenv xts (e : es)
    _ :< TM.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      let (xts, es) = unzip $ castSeq ++ [(mxt, e1)] ++ uncastSeq
      chainOfBinder tenv xts (es ++ [e2])
    _ :< TM.Let _ mxt e1 e2 -> do
      let xs1 = chainOf' tenv e1
      let xs2 = chainOfBinder tenv [mxt] [e2]
      xs1 ++ xs2
    _ :< TM.Prim _ ->
      []
    _ :< TM.Magic der ->
      foldMap (chainOf' tenv) der
    _ :< TM.Resource _ _ unitType discarder copier -> do
      let xs1 = chainOf' tenv unitType
      let xs2 = chainOf' tenv discarder
      let xs3 = chainOf' tenv copier
      xs1 ++ xs2 ++ xs3
    _ :< TM.Void ->
      []

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
    DT.Leaf xs letSeq e -> do
      concatMap (chainOfVar tenv m) xs ++ chainOf' tenv (TM.fromLetSeq letSeq e)
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
  case decisionCase of
    DT.LiteralCase _ _ cont -> do
      chainOfDecisionTree' tenv m [] cont
    DT.ConsCase (DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      let xs1 = concatMap (chainOf' tenv) dataTerms
      let xs2 = concatMap (chainOf' tenv) dataTypes
      let xs3 = chainOfDecisionTree' tenv m consArgs cont
      xs1 ++ xs2 ++ xs3

chainOfCaseWithoutCont :: TM.TypeEnv -> DT.Case TM.Term -> [BinderF TM.Term]
chainOfCaseWithoutCont tenv decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat _ cont -> do
      chainOfDecisionTree' tenv mPat [] cont
    DT.ConsCase (DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      let xs1 = concatMap (chainOf' tenv) dataTerms
      let xs2 = concatMap (chainOf' tenv) dataTypes
      xs1 ++ xs2

nubFreeVariables :: [BinderF TM.Term] -> [BinderF TM.Term]
nubFreeVariables =
  ListUtils.nubOrdOn (\(_, x, _) -> x)

chainOfVar :: TM.TypeEnv -> Hint -> Ident -> [BinderF TM.Term]
chainOfVar tenv m x = do
  case IntMap.lookup (Ident.toInt x) tenv of
    Just t -> do
      let xts = chainOf' tenv t
      xts ++ [(m, x, t)]
    _ ->
      error $ T.unpack $ "[critical] chainOfVar: " <> Ident.toText' x <> "\n" <> T.pack (toString m)
