module Entity.WeakTerm.LayeredFreeVars (freeVarsAtCurrentLayer) where

import Control.Comonad.Cofree
import Data.Maybe
import Data.Set qualified as S
import Entity.Annotation qualified as AN
import Entity.Attr.Lam qualified as AttrL
import Entity.Binder
import Entity.DecisionTree qualified as DT
import Entity.Hint
import Entity.Ident
import Entity.Layer
import Entity.WeakTerm qualified as WT

freeVarsAtCurrentLayer :: WT.WeakTerm -> S.Set (Hint, Ident)
freeVarsAtCurrentLayer =
  freeVars' 0

freeVars' :: Layer -> WT.WeakTerm -> S.Set (Hint, Ident)
freeVars' l term =
  case term of
    _ :< WT.Tau ->
      S.empty
    m :< WT.Var x ->
      if l == 0
        then S.singleton (m, x)
        else S.empty
    _ :< WT.VarGlobal {} ->
      S.empty
    _ :< WT.Pi impArgs expArgs t ->
      freeVars'' l (impArgs ++ expArgs) (freeVars' l t)
    _ :< WT.PiIntro k impArgs expArgs e ->
      freeVars'' l (impArgs ++ expArgs ++ catMaybes [AttrL.fromAttr k]) (freeVars' l e)
    _ :< WT.PiElim e es -> do
      let xs = freeVars' l e
      let ys = S.unions $ map (freeVars' l) es
      S.union xs ys
    _ :< WT.PiElimExact e -> do
      freeVars' l e
    _ :< WT.Data _ _ es ->
      S.unions $ map (freeVars' l) es
    _ :< WT.DataIntro _ _ dataArgs consArgs -> do
      S.unions $ map (freeVars' l) $ dataArgs ++ consArgs
    m :< WT.DataElim _ oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      let xs1 = S.unions $ map (freeVars' l) es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      let xs2 = freeVars'' l binder (freeVarsDecisionTree l decisionTree)
      S.union xs1 xs2
    _ :< WT.Box t ->
      freeVars' l t
    _ :< WT.BoxIntro xets e -> do
      let (xs, es, ts) = unzip3 xets
      let outerFreeVars = map (freeVars' l) (es ++ ts)
      let innerFreeVars = S.filter (\(_, y) -> y `notElem` xs) $ freeVars' (l - 1) e
      S.unions $ innerFreeVars : outerFreeVars
    _ :< WT.Noema t ->
      freeVars' l t
    _ :< WT.Embody t e ->
      S.union (freeVars' l t) (freeVars' l e)
    _ :< WT.Actual e ->
      freeVars' l e
    _ :< WT.Let _ mxt e1 e2 -> do
      let set1 = freeVars' l e1
      let set2 = freeVars'' l [mxt] (freeVars' l e2)
      S.union set1 set2
    _ :< WT.Prim prim ->
      foldMap (freeVars' l) prim
    _ :< WT.Hole _ es ->
      S.unions $ map (freeVars' l) es
    _ :< WT.Magic der ->
      foldMap (freeVars' l) der
    _ :< WT.Annotation _ annot e -> do
      let xs1 = freeVars' l e
      case annot of
        AN.Type t -> do
          let xs2 = freeVars' l t
          S.union xs1 xs2
    _ :< WT.Resource _ discarder copier -> do
      let xs1 = freeVars' l discarder
      let xs2 = freeVars' l copier
      S.union xs1 xs2
    _ :< WT.Use e xts cont -> do
      let fvs1 = freeVars' l e
      let fvs2 = freeVars'' l xts (freeVars' l cont)
      S.union fvs1 fvs2

freeVars'' :: Layer -> [BinderF WT.WeakTerm] -> S.Set (Hint, Ident) -> S.Set (Hint, Ident)
freeVars'' l binder zs =
  case binder of
    [] ->
      zs
    ((_, x, t) : xts) -> do
      let hs1 = freeVars' l t
      let hs2 = freeVars'' l xts zs
      S.union hs1 $ S.filter (\(_, y) -> y /= x) hs2

freeVarsDecisionTree :: Layer -> DT.DecisionTree WT.WeakTerm -> S.Set (Hint, Ident)
freeVarsDecisionTree l tree =
  case tree of
    DT.Leaf _ letSeq e ->
      freeVars' l (WT.fromLetSeq letSeq e)
    DT.Unreachable ->
      S.empty
    DT.Switch (_, cursor) caseList ->
      S.union (freeVars' l cursor) (freeVarsCaseList l caseList)

freeVarsCaseList :: Layer -> DT.CaseList WT.WeakTerm -> S.Set (Hint, Ident)
freeVarsCaseList l (fallbackClause, clauseList) = do
  let xs1 = freeVarsDecisionTree l fallbackClause
  let xs2 = S.unions $ map (freeVarsCase l) clauseList
  S.union xs1 xs2

freeVarsCase :: Layer -> DT.Case WT.WeakTerm -> S.Set (Hint, Ident)
freeVarsCase l decisionCase = do
  case decisionCase of
    DT.LiteralIntCase _ _ cont -> do
      freeVarsDecisionTree l cont
    DT.ConsCase {..} -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      S.unions $ freeVars'' l consArgs (freeVarsDecisionTree l cont) : map (freeVars' l) dataTerms ++ map (freeVars' l) dataTypes
