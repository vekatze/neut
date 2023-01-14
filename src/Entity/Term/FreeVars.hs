module Entity.Term.FreeVars (freeVars, matchFreeVarSets) where

import Control.Comonad.Cofree
import Data.Maybe
import qualified Data.Set as S
import Entity.Binder
import qualified Entity.DecisionTree as DT
import Entity.Ident
import Entity.LamKind
import qualified Entity.Term as TM

matchFreeVarSets :: DT.CaseList TM.Term -> (S.Set Ident, [S.Set Ident])
matchFreeVarSets (fallbackClause, clauseList) = do
  let fallbackFreeVarSet = freeVarsDecisionTree fallbackClause
  let clauseFreeVarSetList = map freeVarsCase clauseList
  (fallbackFreeVarSet, clauseFreeVarSetList)

freeVarsClauseList :: DT.CaseList TM.Term -> [S.Set Ident]
freeVarsClauseList (fallbackClause, clauseList) = do
  freeVarsDecisionTree fallbackClause : map freeVarsCase clauseList

freeVars :: TM.Term -> S.Set Ident
freeVars term =
  case term of
    _ :< TM.Tau ->
      S.empty
    _ :< TM.Var x ->
      S.singleton x
    _ :< TM.VarGlobal {} ->
      S.empty
    _ :< TM.Pi xts t ->
      freeVars' xts (freeVars t)
    _ :< TM.PiIntro k xts e ->
      freeVars' (catMaybes [fromLamKind k] ++ xts) (freeVars e)
    _ :< TM.PiElim e es -> do
      let xs = freeVars e
      let ys = S.unions $ map freeVars es
      S.union xs ys
    _ :< TM.Data _ es ->
      S.unions $ map freeVars es
    _ :< TM.DataIntro _ _ _ dataArgs consArgs -> do
      S.unions $ map freeVars $ dataArgs ++ consArgs
    m :< TM.DataElim _ oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      let xs1 = S.unions $ map freeVars es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      let xs2 = freeVars' binder (freeVarsDecisionTree decisionTree)
      S.union xs1 xs2
    _ :< TM.Noema t ->
      freeVars t
    _ :< TM.Prim _ ->
      S.empty
    _ :< TM.Magic der ->
      foldMap freeVars der

freeVars' :: [BinderF TM.Term] -> S.Set Ident -> S.Set Ident
freeVars' binder zs =
  case binder of
    [] ->
      zs
    ((_, x, t) : xts) -> do
      let hs1 = freeVars t
      let hs2 = freeVars' xts zs
      S.union hs1 $ S.filter (/= x) hs2

freeVarsDecisionTree :: DT.DecisionTree TM.Term -> S.Set Ident
freeVarsDecisionTree tree =
  case tree of
    DT.Leaf _ e ->
      freeVars e
    DT.Unreachable ->
      S.empty
    DT.Switch (_, cursor) caseList ->
      S.union (freeVars cursor) (freeVarsClauseList' caseList)

freeVarsClauseList' :: DT.CaseList TM.Term -> S.Set Ident
freeVarsClauseList' clauses = do
  S.unions $ freeVarsClauseList clauses

freeVarsCase :: DT.Case TM.Term -> S.Set Ident
freeVarsCase (DT.Cons _ _ dataArgs consArgs tree) = do
  let (dataTerms, dataTypes) = unzip dataArgs
  S.unions $ freeVars' consArgs (freeVarsDecisionTree tree) : map freeVars dataTerms ++ map freeVars dataTypes
