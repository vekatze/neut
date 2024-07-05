module Entity.Term.TypedFreeVars (typedFreeVars) where

import Control.Comonad.Cofree
import Data.Containers.ListUtils qualified as ListUtils
import Data.IntMap qualified as IntMap
import Data.Maybe
import Data.Text qualified as T
import Entity.Attr.Lam qualified as AttrL
import Entity.Binder
import Entity.DecisionTree qualified as DT
import Entity.Hint
import Entity.Hint.Reify (toString)
import Entity.Ident
import Entity.Ident.Reify qualified as Ident
import Entity.Term qualified as TM

typedFreeVars :: TM.TypeEnv -> TM.Term -> [BinderF TM.Term]
typedFreeVars tenv term =
  nubFreeVariables $ freeVars tenv term

freeVars :: TM.TypeEnv -> TM.Term -> [BinderF TM.Term]
freeVars tenv term =
  case term of
    _ :< TM.Tau ->
      []
    m :< TM.Var x -> do
      attachType tenv m x
    _ :< TM.VarGlobal {} ->
      []
    _ :< TM.Pi {} ->
      []
    _ :< TM.PiIntro attr impArgs expArgs e ->
      freeVars' tenv (impArgs ++ expArgs ++ catMaybes [AttrL.fromAttr attr]) [e]
    _ :< TM.PiElim e es -> do
      let xs1 = freeVars tenv e
      let xs2 = concatMap (freeVars tenv) es
      xs1 ++ xs2
    _ :< TM.Data _ _ es ->
      concatMap (freeVars tenv) es
    _ :< TM.DataIntro _ _ dataArgs consArgs ->
      concatMap (freeVars tenv) $ dataArgs ++ consArgs
    m :< TM.DataElim _ xets tree -> do
      let (xs, es, ts) = unzip3 xets
      let xs1 = concatMap (freeVars tenv) es
      let mxts = zipWith (\x t -> (m, x, t)) xs ts
      let xs2 = freeVarsDecisionTree' tenv m mxts tree
      xs1 ++ xs2
    _ :< TM.Box t ->
      freeVars tenv t
    m :< TM.BoxIntro xets e -> do
      let (xs, es, ts) = unzip3 xets
      let vs = concatMap (freeVars tenv) es
      let mxts = zipWith (\x t -> (m, x, t)) xs ts
      vs ++ freeVars' tenv mxts [e]
    _ :< TM.Noema t ->
      freeVars tenv t
    _ :< TM.Embody t e -> do
      let xs1 = freeVars tenv t
      let xs2 = freeVars tenv e
      xs1 ++ xs2
    _ :< TM.Let _ mxt e1 e2 -> do
      let xs1 = freeVars tenv e1
      let xs2 = freeVars' tenv [mxt] [e2]
      xs1 ++ xs2
    _ :< TM.Prim _ ->
      []
    _ :< TM.Magic der ->
      foldMap (freeVars tenv) der
    _ :< TM.Resource _ discarder copier -> do
      let xs1 = freeVars tenv discarder
      let xs2 = freeVars tenv copier
      xs1 ++ xs2

freeVars' :: TM.TypeEnv -> [BinderF TM.Term] -> [TM.Term] -> [BinderF TM.Term]
freeVars' tenv binder es =
  freeVars'' tenv binder $ \tenv' -> concatMap (freeVars tenv') es

freeVars'' :: TM.TypeEnv -> [BinderF TM.Term] -> (TM.TypeEnv -> [BinderF TM.Term]) -> [BinderF TM.Term]
freeVars'' tenv mxts f =
  case mxts of
    [] ->
      f tenv
    (mxt@(_, x, t) : xts) -> do
      let hs1 = freeVars tenv t
      let hs2 = freeVars'' (TM.insTypeEnv [mxt] tenv) xts f
      hs1 ++ filter (\(_, y, _) -> y /= x) hs2

freeVarsDecisionTree :: TM.TypeEnv -> Hint -> DT.DecisionTree TM.Term -> [BinderF TM.Term]
freeVarsDecisionTree tenv m tree =
  case tree of
    DT.Leaf xs letSeq e -> do
      concatMap (attachType tenv m) xs ++ freeVars tenv (TM.fromLetSeq letSeq e)
    DT.Unreachable ->
      []
    DT.Switch (cursor, _) caseList ->
      -- the cursor must be treated as an immediate
      (m, cursor, m :< TM.Tau) : freeVarsCaseList tenv m caseList

freeVarsDecisionTree' :: TM.TypeEnv -> Hint -> [BinderF TM.Term] -> DT.DecisionTree TM.Term -> [BinderF TM.Term]
freeVarsDecisionTree' tenv m xts tree =
  freeVars'' tenv xts $ \tenv' -> freeVarsDecisionTree tenv' m tree

freeVarsCaseList :: TM.TypeEnv -> Hint -> DT.CaseList TM.Term -> [BinderF TM.Term]
freeVarsCaseList tenv m (fallbackClause, clauseList) = do
  let xs1 = freeVarsDecisionTree tenv m fallbackClause
  let xs2 = concatMap (freeVarsCase tenv m) clauseList
  xs1 ++ xs2

freeVarsCase :: TM.TypeEnv -> Hint -> DT.Case TM.Term -> [BinderF TM.Term]
freeVarsCase tenv m decisionCase = do
  case decisionCase of
    DT.LiteralIntCase _ _ cont -> do
      freeVarsDecisionTree' tenv m [] cont
    DT.ConsCase {..} -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      let xs1 = concatMap (freeVars tenv) dataTerms
      let xs2 = concatMap (freeVars tenv) dataTypes
      let xs3 = freeVarsDecisionTree' tenv m consArgs cont
      xs1 ++ xs2 ++ xs3

nubFreeVariables :: [BinderF TM.Term] -> [BinderF TM.Term]
nubFreeVariables =
  ListUtils.nubOrdOn (\(_, x, _) -> x)

attachType :: TM.TypeEnv -> Hint -> Ident -> [BinderF TM.Term]
attachType tenv m x = do
  case IntMap.lookup (Ident.toInt x) tenv of
    Just t -> do
      [(m, x, t)]
    _ ->
      error $ T.unpack $ "[critical] attachType: " <> Ident.toText' x <> "\n" <> T.pack (toString m)
