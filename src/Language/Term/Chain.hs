module Language.Term.Chain
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
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.Binder
import Language.Common.DecisionTree qualified as DT
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.Term.Term qualified as TM
import Logger.Hint
import Logger.Hint.Reify (toString)

chainOf :: TM.TypeEnv -> [TM.Term] -> [BinderF TM.Type]
chainOf tenv term =
  nubFreeVariables $ concatMap (chainOf' tenv) term

chainOf' :: TM.TypeEnv -> TM.Term -> [BinderF TM.Type]
chainOf' tenv term =
  case term of
    m :< TM.Var x -> do
      chainOfVar tenv m x
    _ :< TM.VarGlobal {} ->
      []
    _ :< TM.PiIntro attr impArgs expArgs defaultArgs e ->
      chainOfBinder tenv (impArgs ++ expArgs ++ map fst defaultArgs ++ catMaybes [AttrL.fromAttr attr]) [e]
    _ :< TM.PiElim _ e impArgs expArgs -> do
      let xs1 = chainOf' tenv e
      let xs2 = concatMap (chainOfType tenv) impArgs
      let xs3 = concatMap (chainOf' tenv) expArgs
      xs1 ++ xs2 ++ xs3
    _ :< TM.DataIntro _ _ dataArgs consArgs -> do
      concatMap (chainOfType tenv) dataArgs ++ concatMap (chainOf' tenv) consArgs
    m :< TM.DataElim _ xets tree -> do
      let (xs, es, ts) = unzip3 xets
      let xs1 = concatMap (chainOf' tenv) es
      let mxts = zipWith (\x t -> (m, x, t)) xs ts
      let xs2 = chainOfDecisionTree' tenv m mxts tree
      xs1 ++ xs2
    _ :< TM.BoxIntro letSeq e -> do
      let (xts, es) = unzip letSeq
      chainOfBinder tenv xts (e : es)
    _ :< TM.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      let (xts, es) = unzip $ castSeq ++ [(mxt, e1)] ++ uncastSeq
      chainOfBinder tenv xts (es ++ [e2])
    _ :< TM.CodeIntro e ->
      chainOf' tenv e
    _ :< TM.CodeElim e ->
      chainOf' tenv e
    _ :< TM.TauIntro ty ->
      chainOfType tenv ty
    _ :< TM.TauElim (mx, x) e1 e2 -> do
      let xs1 = chainOf' tenv e1
      let mxt = (mx, x, mx :< TM.Tau)
      let xs2 = chainOfBinder tenv [mxt] [e2]
      xs1 ++ xs2
    _ :< TM.Let _ mxt e1 e2 -> do
      let xs1 = chainOf' tenv e1
      let xs2 = chainOfBinder tenv [mxt] [e2]
      xs1 ++ xs2
    _ :< TM.Prim _ ->
      []
    _ :< TM.Magic der ->
      foldMap (chainOf' tenv) der

chainOfType :: TM.TypeEnv -> TM.Type -> [BinderF TM.Type]
chainOfType tenv ty =
  case ty of
    _ :< TM.Tau ->
      []
    m :< TM.TVar x -> do
      chainOfVar tenv m x
    _ :< TM.TVarGlobal {} ->
      []
    _ :< TM.TyApp t args ->
      concatMap (chainOfType tenv) (t : args)
    _ :< TM.Pi _ impArgs expArgs defaultArgs t -> do
      chainOfTypeBinder tenv (impArgs ++ expArgs ++ map fst defaultArgs) [t]
    _ :< TM.Data attr _ es -> do
      let xs1 = concatMap (chainOfType tenv) es
      let xs2 = chainOfAttrData tenv attr
      xs1 ++ xs2
    _ :< TM.Box t ->
      chainOfType tenv t
    _ :< TM.BoxNoema t ->
      chainOfType tenv t
    _ :< TM.Code t ->
      chainOfType tenv t
    _ :< TM.PrimType {} ->
      []
    _ :< TM.Void ->
      []
    _ :< TM.Resource _ _ unitType discarder copier typeTag -> do
      let xs1 = chainOfType tenv unitType
      let xs2 = chainOf' tenv discarder
      let xs3 = chainOf' tenv copier
      let xs4 = chainOf' tenv typeTag
      xs1 ++ xs2 ++ xs3 ++ xs4

chainOfBinder :: TM.TypeEnv -> [BinderF TM.Type] -> [TM.Term] -> [BinderF TM.Type]
chainOfBinder tenv binder es =
  chainOfBinder' tenv binder $ \tenv' -> concatMap (chainOf' tenv') es

chainOfBinder' :: TM.TypeEnv -> [BinderF TM.Type] -> (TM.TypeEnv -> [BinderF TM.Type]) -> [BinderF TM.Type]
chainOfBinder' tenv mxts f =
  case mxts of
    [] ->
      f tenv
    (mxt@(_, x, t) : xts) -> do
      let hs1 = chainOfType tenv t
      let hs2 = chainOfBinder' (TM.insTypeEnv [mxt] tenv) xts f
      hs1 ++ filter (\(_, y, _) -> y /= x) hs2

chainOfTypeBinder :: TM.TypeEnv -> [BinderF TM.Type] -> [TM.Type] -> [BinderF TM.Type]
chainOfTypeBinder tenv binder ts =
  chainOfBinder' tenv binder $ \tenv' -> concatMap (chainOfType tenv') ts

chainOfDecisionTree :: TM.TypeEnv -> Hint -> DT.DecisionTree TM.Type TM.Term -> [BinderF TM.Type]
chainOfDecisionTree tenv m tree =
  case tree of
    DT.Leaf xs letSeq e -> do
      concatMap (chainOfVar tenv m) xs ++ chainOf' tenv (TM.fromLetSeq letSeq e)
    DT.Unreachable ->
      []
    DT.Switch (cursor, _) caseList ->
      -- the cursor must be treated as an immediate
      (m, cursor, m :< TM.Tau) : chainOfCaseList tenv m caseList

chainOfDecisionTree' :: TM.TypeEnv -> Hint -> [BinderF TM.Type] -> DT.DecisionTree TM.Type TM.Term -> [BinderF TM.Type]
chainOfDecisionTree' tenv m xts tree =
  chainOfBinder' tenv xts $ \tenv' -> chainOfDecisionTree tenv' m tree

chainOfCaseList :: TM.TypeEnv -> Hint -> DT.CaseList TM.Type TM.Term -> [BinderF TM.Type]
chainOfCaseList tenv m (fallbackClause, clauseList) = do
  let xs1 = chainOfDecisionTree tenv m fallbackClause
  let xs2 = concatMap (chainOfCase tenv m) clauseList
  xs1 ++ xs2

chainOfCase :: TM.TypeEnv -> Hint -> DT.Case TM.Type TM.Term -> [BinderF TM.Type]
chainOfCase tenv m decisionCase = do
  case decisionCase of
    DT.LiteralCase _ _ cont -> do
      chainOfDecisionTree' tenv m [] cont
    DT.ConsCase (DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      let xs1 = concatMap (chainOfType tenv) dataTerms
      let xs2 = concatMap (chainOfType tenv) dataTypes
      let xs3 = chainOfDecisionTree' tenv m consArgs cont
      xs1 ++ xs2 ++ xs3

chainOfCaseWithoutCont :: TM.TypeEnv -> DT.Case TM.Type TM.Term -> [BinderF TM.Type]
chainOfCaseWithoutCont tenv decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat _ cont -> do
      chainOfDecisionTree' tenv mPat [] cont
    DT.ConsCase (DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      let xs1 = concatMap (chainOfType tenv) dataTerms
      let xs2 = concatMap (chainOfType tenv) dataTypes
      xs1 ++ xs2

nubFreeVariables :: [BinderF TM.Type] -> [BinderF TM.Type]
nubFreeVariables =
  ListUtils.nubOrdOn (\(_, x, _) -> x)

chainOfVar :: TM.TypeEnv -> Hint -> Ident -> [BinderF TM.Type]
chainOfVar tenv m x = do
  case IntMap.lookup (Ident.toInt x) tenv of
    Just t -> do
      let xts = chainOfType tenv t
      xts ++ [(m, x, t)]
    _ ->
      error $ T.unpack $ "[critical] chainOfVar: " <> Ident.toText' x <> "\n" <> T.pack (toString m)

chainOfAttrData :: TM.TypeEnv -> AttrD.Attr name (BinderF TM.Type) -> [BinderF TM.Type]
chainOfAttrData tenv attr = do
  let consNameList = AttrD.consNameList attr
  concatMap (\(_, binders, _) -> concatMap (\(_, _, t) -> chainOfType tenv t) binders) consNameList
