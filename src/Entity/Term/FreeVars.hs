module Entity.Term.FreeVars (freeVars) where

import Control.Comonad.Cofree
import Data.Maybe
import Data.Set qualified as S
import Entity.Attr.Lam qualified as AttrL
import Entity.Binder
import Entity.DecisionTree qualified as DT
import Entity.Ident
import Entity.Prim qualified as P
import Entity.PrimValue qualified as PV
import Entity.Term qualified as TM

freeVars :: TM.Term -> S.Set Ident
freeVars term =
  case term of
    _ :< TM.Tau ->
      S.empty
    _ :< TM.Var x ->
      S.singleton x
    _ :< TM.VarGlobal {} ->
      S.empty
    _ :< TM.Pi impArgs expArgs t ->
      freeVars' (impArgs ++ expArgs) (freeVars t)
    _ :< TM.PiIntro k impArgs expArgs e ->
      freeVars' (impArgs ++ expArgs ++ catMaybes [AttrL.fromAttr k]) (freeVars e)
    _ :< TM.PiElim e es -> do
      let xs = freeVars e
      let ys = S.unions $ map freeVars es
      S.union xs ys
    _ :< TM.Data _ _ es ->
      S.unions $ map freeVars es
    _ :< TM.DataIntro _ _ dataArgs consArgs -> do
      S.unions $ map freeVars $ dataArgs ++ consArgs
    m :< TM.DataElim _ oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      let xs1 = S.unions $ map freeVars es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      let xs2 = freeVars' binder (freeVarsDecisionTree decisionTree)
      S.union xs1 xs2
    _ :< TM.Noema t ->
      freeVars t
    _ :< TM.Embody t e ->
      S.union (freeVars t) (freeVars e)
    _ :< TM.Let _ mxt e1 e2 -> do
      let set1 = freeVars e1
      let set2 = freeVars' [mxt] (freeVars e2)
      S.union set1 set2
    _ :< TM.Prim prim ->
      case prim of
        P.Value (PV.StaticText t _) ->
          freeVars t
        _ ->
          S.empty
    _ :< TM.Magic der ->
      foldMap freeVars der
    _ :< TM.Resource _ discarder copier -> do
      let xs1 = freeVars discarder
      let xs2 = freeVars copier
      S.union xs1 xs2

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
    DT.Leaf _ letSeq e ->
      freeVars (TM.fromLetSeq letSeq e)
    DT.Unreachable ->
      S.empty
    DT.Switch (_, cursor) caseList ->
      S.union (freeVars cursor) (freeVarsCaseList caseList)

freeVarsCaseList :: DT.CaseList TM.Term -> S.Set Ident
freeVarsCaseList (fallbackClause, clauseList) = do
  let xs1 = freeVarsDecisionTree fallbackClause
  let xs2 = S.unions $ map freeVarsCase clauseList
  S.union xs1 xs2

freeVarsCase :: DT.Case TM.Term -> S.Set Ident
freeVarsCase decisionCase = do
  case decisionCase of
    DT.LiteralIntCase _ _ cont -> do
      freeVarsDecisionTree cont
    DT.ConsCase {..} -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      S.unions $ freeVars' consArgs (freeVarsDecisionTree cont) : map freeVars dataTerms ++ map freeVars dataTypes
