module Language.WeakTerm.FreeVars (freeVars, freeVarsType) where

import Control.Comonad.Cofree
import Data.Foldable (toList)
import Data.Maybe
import Data.Set qualified as S
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.Binder
import Language.Common.DecisionTree qualified as DT
import Language.Common.Ident
import Language.Common.Magic qualified as M
import Language.WeakTerm.WeakTerm qualified as WT

freeVars :: WT.WeakTerm -> S.Set Ident
freeVars term =
  case term of
    _ :< WT.Var x ->
      S.singleton x
    _ :< WT.VarGlobal {} ->
      S.empty
    _ :< WT.PiIntro k _ expParams defaultParams e -> do
      let body = removeBinders (catMaybes [AttrL.fromAttr k]) (freeVars e)
      removeBinders expParams (freeVarsDefaultParams defaultParams body)
    _ :< WT.PiElim _ e _ expArgs defaultArgs ->
      S.unions $ freeVars e : map freeVars (toList defaultArgs ++ expArgs)
    _ :< WT.PiElimExact e ->
      freeVars e
    _ :< WT.DataIntro _ _ _ _ consArgs ->
      S.unions $ map freeVars consArgs
    _ :< WT.DataElim _ oets decisionTree -> do
      let (os, es, _) = unzip3 oets
      let xs1 = S.unions $ map freeVars es
      let xs2 = foldr S.delete (freeVarsDecisionTree decisionTree) os
      S.union xs1 xs2
    _ :< WT.BoxIntro letSeq e -> do
      let (mxts, es) = unzip letSeq
      removeBinders mxts (S.unions $ map freeVars (e : es))
    _ :< WT.BoxIntroLift _ e ->
      freeVars e
    _ :< WT.EmbedIntro e ->
      freeVars e
    _ :< WT.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      let (xts, es) = unzip $ castSeq ++ [(mxt, e1)] ++ uncastSeq
      removeBinders xts (S.unions $ map freeVars $ es ++ [e2])
    _ :< WT.CodeIntro _ e ->
      freeVars e
    _ :< WT.CodeElim e ->
      freeVars e
    _ :< WT.TauIntro _ ->
      S.empty
    _ :< WT.Let mxt e1 e2 ->
      S.union (freeVars e1) (removeBinders [mxt] (freeVars e2))
    _ :< WT.TauElim _ e1 e2 ->
      S.union (freeVars e1) (freeVars e2)
    _ :< WT.LetType _ _ e ->
      freeVars e
    _ :< WT.Invoke _ body ->
      freeVars body
    _ :< WT.Prim _ ->
      S.empty
    _ :< WT.Magic (M.WeakMagic magic) ->
      foldMap freeVars magic
    _ :< WT.Annotation _ _ e ->
      freeVars e

freeVarsDefaultParams :: [(BinderF WT.WeakType, WT.WeakTerm)] -> S.Set Ident -> S.Set Ident
freeVarsDefaultParams defaultParams zs =
  case defaultParams of
    [] ->
      zs
    (binder, value) : rest ->
      S.union (freeVars value) (removeBinders [binder] (freeVarsDefaultParams rest zs))

removeBinders :: [BinderF WT.WeakType] -> S.Set Ident -> S.Set Ident
removeBinders binders zs =
  foldr (S.delete . getBinderIdent) zs binders

freeVarsDecisionTree :: DT.DecisionTree WT.WeakType WT.WeakTerm -> S.Set Ident
freeVarsDecisionTree tree =
  case tree of
    DT.Leaf _ letSeq e ->
      freeVars (WT.fromLetSeq letSeq e)
    DT.Unreachable ->
      S.empty
    DT.Switch _ (fallbackClause, clauseList) ->
      S.unions $ freeVarsDecisionTree fallbackClause : map freeVarsCase clauseList

freeVarsCase :: DT.Case WT.WeakType WT.WeakTerm -> S.Set Ident
freeVarsCase decisionCase = do
  case decisionCase of
    DT.LiteralCase _ _ cont ->
      freeVarsDecisionTree cont
    DT.ConsCase (DT.ConsCaseRecord {consBinders, cont}) ->
      removeBinders consBinders (freeVarsDecisionTree cont)

freeVarsType :: WT.WeakType -> S.Set Ident
freeVarsType ty =
  case ty of
    _ :< WT.Tau ->
      S.empty
    _ :< WT.TVar x ->
      S.singleton x
    _ :< WT.TVarGlobal {} ->
      S.empty
    _ :< WT.TyApp t args -> do
      S.unions $ freeVarsType t : map freeVarsType args
    _ :< WT.Pi _ impParams expParams defaultParams t ->
      freeVarsTypeBinders (impParams ++ expParams ++ defaultParams) (freeVarsType t)
    _ :< WT.Data _ _ es ->
      S.unions $ map freeVarsType es
    _ :< WT.Box t ->
      freeVarsType t
    _ :< WT.BoxNoema t ->
      freeVarsType t
    _ :< WT.Embed t ->
      freeVarsType t
    _ :< WT.Code t ->
      freeVarsType t
    _ :< WT.PrimType {} ->
      S.empty
    _ :< WT.Void ->
      S.empty
    _ :< WT.Resource _ _ -> do
      S.empty
    _ :< WT.TypeHole _ es ->
      S.unions $ map freeVarsType es

freeVarsTypeBinders :: [BinderF WT.WeakType] -> S.Set Ident -> S.Set Ident
freeVarsTypeBinders binders zs =
  case binders of
    [] ->
      zs
    (_, _, x, t) : rest ->
      S.union (freeVarsType t) (S.delete x (freeVarsTypeBinders rest zs))
