module Language.WeakTerm.Holes (holes, holesType) where

import Control.Comonad.Cofree
import Data.Maybe
import Data.Set qualified as S
import Language.Common.Annotation qualified as AN
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.Binder
import Language.Common.DecisionTree qualified as DT
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.HoleID
import Language.Common.DefaultArgs qualified as DefaultArgs
import Language.Common.ImpArgs qualified as ImpArgs
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.WeakTerm.WeakTerm qualified as WT

holes :: WT.WeakTerm -> S.Set HoleID
holes term =
  case term of
    _ :< WT.Var {} ->
      S.empty
    _ :< WT.VarGlobal {} ->
      S.empty
    _ :< WT.PiIntro k impArgs defaultArgs expArgs e -> do
      let impBinders = impArgs ++ map fst defaultArgs
      let defaultHoles = S.unions $ map holes $ map snd defaultArgs
      S.union defaultHoles (holesBindersType (impBinders ++ expArgs ++ catMaybes [AttrL.fromAttr k]) (holes e))
    _ :< WT.PiElim _ e impArgs defaultArgs expArgs ->
      S.unions $
        holes e
          : map holesType (ImpArgs.extract impArgs)
          ++ map holes (DefaultArgs.extract defaultArgs)
          ++ map holes expArgs
    _ :< WT.PiElimExact e ->
      holes e
    _ :< WT.DataIntro attr _ dataArgs consArgs -> do
      let xs1 = S.unions $ map holes consArgs
      let xs2 = S.unions $ map holesType dataArgs
      let xs3 = holesAttrDataIntro attr
      S.unions [xs1, xs2, xs3]
    m :< WT.DataElim _ oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      let xs1 = S.unions $ map holes es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      let xs2 = holesBindersType binder (holesDecisionTree decisionTree)
      S.union xs1 xs2
    _ :< WT.BoxIntro letSeq e -> do
      let (xts, es) = unzip letSeq
      holesBindersType xts (S.unions $ map holes $ e : es)
    _ :< WT.BoxIntroLift e ->
      holes e
    _ :< WT.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      let (xts, es) = unzip $ castSeq ++ [(mxt, e1)] ++ uncastSeq
      holesBindersType xts (S.unions $ map holes $ es ++ [e2])
    _ :< WT.CodeIntro e ->
      holes e
    _ :< WT.CodeElim e ->
      holes e
    _ :< WT.Actual e ->
      holes e
    _ :< WT.Let _ mxt e1 e2 -> do
      let set1 = holes e1
      let set2 = holesBindersType [mxt] (holes e2)
      S.union set1 set2
    _ :< WT.LetType _ e1 e2 ->
      S.union (holes e1) (holes e2)
    _ :< WT.Prim prim ->
      foldMap holesType prim
    _ :< WT.Magic der ->
      holesMagic der
    _ :< WT.Annotation _ annot e -> do
      let xs1 = holes e
      case annot of
        AN.Type t -> do
          let xs2 = holesType t
          S.union xs1 xs2

holesType :: WT.WeakType -> S.Set HoleID
holesType ty =
  case ty of
    _ :< WT.Tau ->
      S.empty
    _ :< WT.TVar {} ->
      S.empty
    _ :< WT.TVarGlobal {} ->
      S.empty
    _ :< WT.TyApp t args ->
      S.unions $ holesType t : map holesType args
    _ :< WT.Pi _ impArgs defaultArgs expArgs t -> do
      let impBinders = impArgs ++ map fst defaultArgs
      let defaultHoles = S.unions $ map holes $ map snd defaultArgs
      S.union defaultHoles (holesBindersType (impBinders ++ expArgs) (holesType t))
    _ :< WT.Data attr _ es -> do
      let xs1 = S.unions $ map holesType es
      let xs2 = holesAttrData attr
      S.union xs1 xs2
    _ :< WT.Box t ->
      holesType t
    _ :< WT.BoxNoema t ->
      holesType t
    _ :< WT.Code t ->
      holesType t
    _ :< WT.PrimType {} ->
      S.empty
    _ :< WT.Void ->
      S.empty
    _ :< WT.Resource _ _ unitType discarder copier typeTag -> do
      S.unions $ [holesType unitType, holes discarder, holes copier, holes typeTag]
    _ :< WT.TypeHole h es ->
      S.insert h $ S.unions $ map holesType es

holesBindersType :: [BinderF WT.WeakType] -> S.Set HoleID -> S.Set HoleID
holesBindersType binder zs =
  case binder of
    [] ->
      zs
    ((_, _, t) : xts) -> do
      let set1 = holesType t
      let set2 = holesBindersType xts zs
      S.union set1 set2

holesDecisionTree :: DT.DecisionTree WT.WeakType WT.WeakTerm -> S.Set HoleID
holesDecisionTree tree =
  case tree of
    DT.Leaf _ letSeq e -> do
      holes $ WT.fromLetSeq letSeq e
    DT.Unreachable ->
      S.empty
    DT.Switch (_, cursor) caseList ->
      S.union (holesType cursor) (holesCaseList caseList)

holesCaseList :: DT.CaseList WT.WeakType WT.WeakTerm -> S.Set HoleID
holesCaseList (fallbackClause, clauseList) = do
  let xs1 = holesDecisionTree fallbackClause
  let xs2 = S.unions $ map holesCase clauseList
  S.union xs1 xs2

holesCase :: DT.Case WT.WeakType WT.WeakTerm -> S.Set HoleID
holesCase decisionCase = do
  case decisionCase of
    DT.LiteralCase _ _ cont -> do
      holesDecisionTree cont
    DT.ConsCase (DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      S.unions $ holesBindersType consArgs (holesDecisionTree cont) : map holesType dataTerms ++ map holesType dataTypes

holesMagic :: M.WeakMagic WT.WeakType WT.WeakType WT.WeakTerm -> S.Set HoleID
holesMagic (M.WeakMagic magic) =
  case magic of
    M.LowMagic lowMagic ->
      holesLowMagic lowMagic
    M.GetTypeTag _ typeTagExpr e ->
      S.union (holesType typeTagExpr) (holesType e)
    M.GetConsSize typeExpr ->
      holesType typeExpr
    M.GetConstructorArgTypes _ listExpr typeExpr index ->
      S.unions [holesType listExpr, holesType typeExpr, holes index]
    M.CompileError _ ->
      S.empty

holesLowMagic :: LM.LowMagic WT.WeakType WT.WeakType WT.WeakTerm -> S.Set HoleID
holesLowMagic lowMagic =
  case lowMagic of
    LM.Cast from to value ->
      S.unions [holesType from, holesType to, holes value]
    LM.Store t unit value pointer ->
      S.unions [holesType t, holesType unit, holes value, holes pointer]
    LM.Load t pointer ->
      S.union (holesType t) (holes pointer)
    LM.Alloca t size ->
      S.union (holesType t) (holes size)
    LM.External domList cod _ args varArgs -> do
      let domHoles = S.unions $ map holesType domList
      let codHoles = case cod of
            FCT.Cod t -> holesType t
            FCT.Void -> S.empty
      let argsHoles = S.unions $ map holes args
      let varArgsHoles = S.unions $ map (\(a, t) -> S.union (holes a) (holesType t)) varArgs
      S.unions [domHoles, codHoles, argsHoles, varArgsHoles]
    LM.Global _ t ->
      holesType t
    LM.OpaqueValue e ->
      holes e
    LM.CallType func arg1 arg2 ->
      S.unions [holes func, holes arg1, holes arg2]
    LM.TermType ty ->
      holesType ty

holesAttrData :: AttrD.Attr name (BinderF WT.WeakType) -> S.Set HoleID
holesAttrData attr = do
  let consNameList = AttrD.consNameList attr
  S.unions $ map (\(_, binders, _) -> S.unions $ map (\(_, _, t) -> holesType t) binders) consNameList

holesAttrDataIntro :: AttrDI.Attr name (BinderF WT.WeakType) -> S.Set HoleID
holesAttrDataIntro attr = do
  let consNameList = AttrDI.consNameList attr
  S.unions $ map (\(_, binders, _) -> S.unions $ map (\(_, _, t) -> holesType t) binders) consNameList
