module Kernel.Elaborate.Internal.WeakTerm.Fill
  ( Handle,
    new,
    fill,
    fillType,
  )
where

import App.App (App)
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class
import Data.Bitraversable (bimapM)
import Data.IntMap qualified as IntMap
import Data.Text qualified as T
import Kernel.Elaborate.TypeHoleSubst qualified as THS
import Language.Common.Annotation qualified as AN
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.Binder
import Language.Common.DecisionTree qualified as DT
import Language.Common.DefaultArgs qualified as DefaultArgs
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.ImpArgs qualified as ImpArgs
import Language.Common.LamKind qualified as LK
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.WeakTerm.Reduce qualified as Reduce
import Language.WeakTerm.Subst (SubstEntry (..))
import Language.WeakTerm.Subst qualified as Subst
import Language.WeakTerm.ToText qualified as WT
import Language.WeakTerm.WeakTerm qualified as WT
import Prelude hiding (lookup)

data Handle = Handle
  { substHandle :: Subst.Handle,
    reduceHandle :: Reduce.Handle
  }

new :: Subst.Handle -> Reduce.Handle -> Handle
new substHandle reduceHandle = do
  Handle {..}

fill :: Handle -> THS.TypeHoleSubst -> WT.WeakTerm -> App WT.WeakTerm
fill h typeSubst term =
  case term of
    _ :< WT.Var {} ->
      return term
    _ :< WT.VarGlobal {} ->
      return term
    m :< WT.PiIntro attr@(AttrL.Attr {lamKind}) impArgs defaultArgs expArgs e -> do
      impArgs' <- fillBinder h typeSubst impArgs
      defaultArgs' <- fillDefaultArgs h typeSubst defaultArgs
      expArgs' <- fillBinder h typeSubst expArgs
      case lamKind of
        LK.Fix opacity xt -> do
          [xt'] <- fillBinder h typeSubst [xt]
          e' <- fill h typeSubst e
          return $ m :< WT.PiIntro (attr {AttrL.lamKind = LK.Fix opacity xt'}) impArgs' defaultArgs' expArgs' e'
        LK.Normal name codType -> do
          codType' <- fillType h typeSubst codType
          e' <- fill h typeSubst e
          return $ m :< WT.PiIntro (attr {AttrL.lamKind = LK.Normal name codType'}) impArgs' defaultArgs' expArgs' e'
    m :< WT.PiElim b e impArgs defaultArgs expArgs -> do
      e' <- fill h typeSubst e
      impArgs' <- ImpArgs.traverseImpArgs (fillType h typeSubst) impArgs
      defaultArgs' <- DefaultArgs.traverseDefaultArgs (fill h typeSubst) defaultArgs
      expArgs' <- mapM (fill h typeSubst) expArgs
      return $ m :< WT.PiElim b e' impArgs' defaultArgs' expArgs'
    m :< WT.PiElimExact e -> do
      e' <- fill h typeSubst e
      return $ m :< WT.PiElimExact e'
    m :< WT.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (fillType h typeSubst) dataArgs
      consArgs' <- mapM (fill h typeSubst) consArgs
      attr' <- fillAttrDataIntro h typeSubst attr
      return $ m :< WT.DataIntro attr' consName dataArgs' consArgs'
    m :< WT.DataElim isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (fill h typeSubst) es
      ts' <- mapM (fillType h typeSubst) ts
      let binder = zipWith (\o t -> (m, o, t)) os ts'
      (binder', decisionTree') <- fill''' h typeSubst binder decisionTree
      let (_, os', ts'') = unzip3 binder'
      return $ m :< WT.DataElim isNoetic (zip3 os' es' ts'') decisionTree'
    m :< WT.BoxIntro letSeq e -> do
      let (xts, es) = unzip letSeq
      xts' <- fillBinder h typeSubst xts
      es' <- mapM (fill h typeSubst) es
      e' <- fill h typeSubst e
      return $ m :< WT.BoxIntro (zip xts' es') e'
    m :< WT.BoxIntroLift e -> do
      e' <- fill h typeSubst e
      return $ m :< WT.BoxIntroLift e'
    m :< WT.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      castSeq' <- fillLetSeq h typeSubst castSeq
      (mxt', e1') <- fillLet h typeSubst (mxt, e1)
      uncastSeq' <- fillLetSeq h typeSubst uncastSeq
      e2' <- fill h typeSubst e2
      return $ m :< WT.BoxElim castSeq' mxt' e1' uncastSeq' e2'
    m :< WT.CodeIntro e -> do
      e' <- fill h typeSubst e
      return $ m :< WT.CodeIntro e'
    m :< WT.CodeElim e -> do
      e' <- fill h typeSubst e
      return $ m :< WT.CodeElim e'
    m :< WT.TauIntro ty -> do
      ty' <- fillType h typeSubst ty
      return $ m :< WT.TauIntro ty'
    m :< WT.TauElim mx e1 e2 -> do
      e1' <- fill h typeSubst e1
      e2' <- fill h typeSubst e2
      return $ m :< WT.TauElim mx e1' e2'
    m :< WT.Actual e -> do
      e' <- fill h typeSubst e
      return $ m :< WT.Actual e'
    m :< WT.Let opacity mxt e1 e2 -> do
      e1' <- fill h typeSubst e1
      (mxt', _, e2') <- fill'' h typeSubst mxt [] e2
      return $ m :< WT.Let opacity mxt' e1' e2'
    m :< WT.Prim prim -> do
      prim' <- mapM (fillType h typeSubst) prim
      return $ m :< WT.Prim prim'
    m :< WT.Magic (M.WeakMagic magic) -> do
      magic' <- fillMagic h typeSubst magic
      return $ m :< WT.Magic (M.WeakMagic magic')
    m :< WT.Annotation logLevel annot e -> do
      e' <- fill h typeSubst e
      case annot of
        AN.Type t -> do
          t' <- fillType h typeSubst t
          return $ m :< WT.Annotation logLevel (AN.Type t') e'

fillType :: Handle -> THS.TypeHoleSubst -> WT.WeakType -> App WT.WeakType
fillType h holeSubst ty =
  case ty of
    _ :< WT.Tau ->
      return ty
    _ :< WT.TVar {} ->
      return ty
    _ :< WT.TVarGlobal {} ->
      return ty
    m :< WT.TyApp t args -> do
      t' <- fillType h holeSubst t
      args' <- mapM (fillType h holeSubst) args
      return $ m :< WT.TyApp t' args'
    m :< WT.Pi piKind impArgs defaultArgs expArgs t -> do
      impArgs' <- fillTypeBinder h holeSubst impArgs
      defaultArgs' <- fillTypeDefaultArgs h holeSubst defaultArgs
      expArgs' <- fillTypeBinder h holeSubst expArgs
      t' <- fillType h holeSubst t
      return $ m :< WT.Pi piKind impArgs' defaultArgs' expArgs' t'
    m :< WT.Data attr name es -> do
      es' <- mapM (fillType h holeSubst) es
      attr' <- fillAttrData h holeSubst attr
      return $ m :< WT.Data attr' name es'
    m :< WT.Box t -> do
      t' <- fillType h holeSubst t
      return $ m :< WT.Box t'
    m :< WT.BoxNoema t -> do
      t' <- fillType h holeSubst t
      return $ m :< WT.BoxNoema t'
    m :< WT.Code t -> do
      t' <- fillType h holeSubst t
      return $ m :< WT.Code t'
    _ :< WT.PrimType {} ->
      return ty
    _ :< WT.Void ->
      return ty
    m :< WT.Resource dd resourceID unitType discarder copier typeTag -> do
      unitType' <- fillType h holeSubst unitType
      discarder' <- fill h holeSubst discarder
      copier' <- fill h holeSubst copier
      typeTag' <- fill h holeSubst typeTag
      return $ m :< WT.Resource dd resourceID unitType' discarder' copier' typeTag'
    m :< WT.TypeHole i es -> do
      es' <- mapM (fillType h holeSubst) es
      case THS.lookup i holeSubst of
        Just (xs, body)
          | length xs == length es' -> do
              let varList = map Ident.toInt xs
              let sub = IntMap.fromList $ zip varList (map Type es')
              liftIO (Subst.substType (substHandle h) sub body) >>= Reduce.reduceType (reduceHandle h)
          | otherwise -> do
              let tyText = WT.toTextType ty
              error $ "Rule.WeakTerm.Fill (assertion failure; arity mismatch)\nhole = ?M" ++ show i ++ "(" ++ show xs ++ ")\nhole id = " ++ show i ++ "\ninput: " <> T.unpack tyText
        Nothing ->
          return $ m :< WT.TypeHole i es'

fillBinder ::
  Handle ->
  THS.TypeHoleSubst ->
  [BinderF WT.WeakType] ->
  App [BinderF WT.WeakType]
fillBinder h holeSubst binder =
  case binder of
    [] -> do
      return []
    (m, x, t) : xts -> do
      t' <- fillType h holeSubst t
      xts' <- fillBinder h holeSubst xts
      return $ (m, x, t') : xts'

fillTypeBinder ::
  Handle ->
  THS.TypeHoleSubst ->
  [BinderF WT.WeakType] ->
  App [BinderF WT.WeakType]
fillTypeBinder h holeSubst binder =
  case binder of
    [] -> do
      return []
    (m, x, t) : xts -> do
      t' <- fillType h holeSubst t
      xts' <- fillTypeBinder h holeSubst xts
      return $ (m, x, t') : xts'

fillDefaultArgs ::
  Handle ->
  THS.TypeHoleSubst ->
  [(BinderF WT.WeakType, WT.WeakTerm)] ->
  App [(BinderF WT.WeakType, WT.WeakTerm)]
fillDefaultArgs h holeSubst binderList =
  case binderList of
    [] -> do
      return []
    (binder, value) : rest -> do
      binder' <- fillSingleBinder h holeSubst binder
      value' <- fill h holeSubst value
      rest' <- fillDefaultArgs h holeSubst rest
      return $ (binder', value') : rest'

fillTypeDefaultArgs ::
  Handle ->
  THS.TypeHoleSubst ->
  [(BinderF WT.WeakType, WT.WeakTerm)] ->
  App [(BinderF WT.WeakType, WT.WeakTerm)]
fillTypeDefaultArgs h holeSubst binderList =
  case binderList of
    [] -> do
      return []
    (binder, value) : rest -> do
      binder' <- fillTypeSingleBinder h holeSubst binder
      value' <- fill h holeSubst value
      rest' <- fillTypeDefaultArgs h holeSubst rest
      return $ (binder', value') : rest'

fillLet ::
  Handle ->
  THS.TypeHoleSubst ->
  (BinderF WT.WeakType, WT.WeakTerm) ->
  App (BinderF WT.WeakType, WT.WeakTerm)
fillLet h holeSubst ((m, x, t), e) = do
  e' <- fill h holeSubst e
  t' <- fillType h holeSubst t
  return ((m, x, t'), e')

fillLetSeq ::
  Handle ->
  THS.TypeHoleSubst ->
  [(BinderF WT.WeakType, WT.WeakTerm)] ->
  App [(BinderF WT.WeakType, WT.WeakTerm)]
fillLetSeq h holeSubst letSeq = do
  case letSeq of
    [] ->
      return []
    letPair : rest -> do
      letPair' <- fillLet h holeSubst letPair
      rest' <- fillLetSeq h holeSubst rest
      return $ letPair' : rest'

fillSingleBinder ::
  Handle ->
  THS.TypeHoleSubst ->
  BinderF WT.WeakType ->
  App (BinderF WT.WeakType)
fillSingleBinder h holeSubst (m, x, t) = do
  t' <- fillType h holeSubst t
  return (m, x, t')

fillTypeSingleBinder ::
  Handle ->
  THS.TypeHoleSubst ->
  BinderF WT.WeakType ->
  App (BinderF WT.WeakType)
fillTypeSingleBinder h holeSubst (m, x, t) = do
  t' <- fillType h holeSubst t
  return (m, x, t')

fill' ::
  Handle ->
  THS.TypeHoleSubst ->
  [BinderF WT.WeakType] ->
  WT.WeakTerm ->
  App ([BinderF WT.WeakType], WT.WeakTerm)
fill' h holeSubst binder e =
  case binder of
    [] -> do
      e' <- fill h holeSubst e
      return ([], e')
    (m, x, t) : xts -> do
      (xts', e') <- fill' h holeSubst xts e
      t' <- fillType h holeSubst t
      return ((m, x, t') : xts', e')

fill'' ::
  Handle ->
  THS.TypeHoleSubst ->
  BinderF WT.WeakType ->
  [BinderF WT.WeakType] ->
  WT.WeakTerm ->
  App (BinderF WT.WeakType, [BinderF WT.WeakType], WT.WeakTerm)
fill'' h holeSubst (m, x, t) binder e = do
  (xts', e') <- fill' h holeSubst binder e
  t' <- fillType h holeSubst t
  return ((m, x, t'), xts', e')

fill''' ::
  Handle ->
  THS.TypeHoleSubst ->
  [BinderF WT.WeakType] ->
  DT.DecisionTree WT.WeakType WT.WeakTerm ->
  App ([BinderF WT.WeakType], DT.DecisionTree WT.WeakType WT.WeakTerm)
fill''' h holeSubst binder decisionTree =
  case binder of
    [] -> do
      decisionTree' <- fillDecisionTree h holeSubst decisionTree
      return ([], decisionTree')
    ((m, x, t) : xts) -> do
      t' <- fillType h holeSubst t
      (xts', e') <- fill''' h holeSubst xts decisionTree
      return ((m, x, t') : xts', e')

fillDecisionTree ::
  Handle ->
  THS.TypeHoleSubst ->
  DT.DecisionTree WT.WeakType WT.WeakTerm ->
  App (DT.DecisionTree WT.WeakType WT.WeakTerm)
fillDecisionTree h holeSubst tree =
  case tree of
    DT.Leaf xs letSeq e -> do
      letSeq' <- mapM (bimapM (fillSingleBinder h holeSubst) (fill h holeSubst)) letSeq
      e' <- fill h holeSubst e
      return $ DT.Leaf xs letSeq' e'
    DT.Unreachable ->
      return tree
    DT.Switch (cursorVar, cursor) caseList -> do
      cursor' <- fillType h holeSubst cursor
      caseList' <- fillCaseList h holeSubst caseList
      return $ DT.Switch (cursorVar, cursor') caseList'

fillCaseList ::
  Handle ->
  THS.TypeHoleSubst ->
  DT.CaseList WT.WeakType WT.WeakTerm ->
  App (DT.CaseList WT.WeakType WT.WeakTerm)
fillCaseList h holeSubst (fallbackClause, clauseList) = do
  fallbackClause' <- fillDecisionTree h holeSubst fallbackClause
  clauseList' <- mapM (fillCase h holeSubst) clauseList
  return (fallbackClause', clauseList')

fillCase ::
  Handle ->
  THS.TypeHoleSubst ->
  DT.Case WT.WeakType WT.WeakTerm ->
  App (DT.Case WT.WeakType WT.WeakTerm)
fillCase h holeSubst decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      cont' <- fillDecisionTree h holeSubst cont
      return $ DT.LiteralCase mPat i cont'
    DT.ConsCase record@(DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      dataTerms' <- mapM (fillType h holeSubst) dataTerms
      dataTypes' <- mapM (fillType h holeSubst) dataTypes
      (consArgs', cont') <- fill''' h holeSubst consArgs cont
      return $
        DT.ConsCase
          record
            { DT.dataArgs = zip dataTerms' dataTypes',
              DT.consArgs = consArgs',
              DT.cont = cont'
            }

fillAttrData :: Handle -> THS.TypeHoleSubst -> AttrD.Attr name (BinderF WT.WeakType) -> App (AttrD.Attr name (BinderF WT.WeakType))
fillAttrData h holeSubst attr = do
  let consNameList = AttrD.consNameList attr
  consNameList' <- forM consNameList $ \(cn, binders, cl) -> do
    binders' <- forM binders $ \(mx, x, t) -> do
      t' <- fillType h holeSubst t
      return (mx, x, t')
    return (cn, binders', cl)
  return $ attr {AttrD.consNameList = consNameList'}

fillAttrDataIntro :: Handle -> THS.TypeHoleSubst -> AttrDI.Attr name (BinderF WT.WeakType) -> App (AttrDI.Attr name (BinderF WT.WeakType))
fillAttrDataIntro h holeSubst attr = do
  let consNameList = AttrDI.consNameList attr
  consNameList' <- forM consNameList $ \(cn, binders, cl) -> do
    binders' <- forM binders $ \(mx, x, t) -> do
      t' <- fillType h holeSubst t
      return (mx, x, t')
    return (cn, binders', cl)
  return $ attr {AttrDI.consNameList = consNameList'}

fillMagic :: Handle -> THS.TypeHoleSubst -> M.Magic WT.WeakType WT.WeakType WT.WeakTerm -> App (M.Magic WT.WeakType WT.WeakType WT.WeakTerm)
fillMagic h holeSubst magic =
  case magic of
    M.LowMagic lowMagic -> do
      lowMagic' <- fillLowMagic h holeSubst lowMagic
      return $ M.LowMagic lowMagic'
    M.GetTypeTag mid typeTagExpr typeExpr -> do
      typeTagExpr' <- fillType h holeSubst typeTagExpr
      typeExpr' <- fillType h holeSubst typeExpr
      return $ M.GetTypeTag mid typeTagExpr' typeExpr'
    M.GetDataArgs sgl listExpr typeExpr -> do
      listExpr' <- fillType h holeSubst listExpr
      typeExpr' <- fillType h holeSubst typeExpr
      return $ M.GetDataArgs sgl listExpr' typeExpr'
    M.GetConsSize typeExpr -> do
      typeExpr' <- fillType h holeSubst typeExpr
      return $ M.GetConsSize typeExpr'
    M.GetWrapperContentType typeExpr -> do
      typeExpr' <- fillType h holeSubst typeExpr
      return $ M.GetWrapperContentType typeExpr'
    M.GetVectorContentType sgl typeExpr -> do
      typeExpr' <- fillType h holeSubst typeExpr
      return $ M.GetVectorContentType sgl typeExpr'
    M.GetConstructorArgTypes sgl listExpr typeExpr index -> do
      listExpr' <- fillType h holeSubst listExpr
      typeExpr' <- fillType h holeSubst typeExpr
      index' <- fill h holeSubst index
      return $ M.GetConstructorArgTypes sgl listExpr' typeExpr' index'
    M.CompileError {} ->
      return magic

fillLowMagic :: Handle -> THS.TypeHoleSubst -> LM.LowMagic WT.WeakType WT.WeakType WT.WeakTerm -> App (LM.LowMagic WT.WeakType WT.WeakType WT.WeakTerm)
fillLowMagic h holeSubst lowMagic =
  case lowMagic of
    LM.Cast from to value -> do
      from' <- fillType h holeSubst from
      to' <- fillType h holeSubst to
      value' <- fill h holeSubst value
      return $ LM.Cast from' to' value'
    LM.Store t unit value pointer -> do
      t' <- fillType h holeSubst t
      unit' <- fillType h holeSubst unit
      value' <- fill h holeSubst value
      pointer' <- fill h holeSubst pointer
      return $ LM.Store t' unit' value' pointer'
    LM.Load t pointer -> do
      t' <- fillType h holeSubst t
      pointer' <- fill h holeSubst pointer
      return $ LM.Load t' pointer'
    LM.Alloca t size -> do
      t' <- fillType h holeSubst t
      size' <- fill h holeSubst size
      return $ LM.Alloca t' size'
    LM.External domList cod extFunName args varArgs -> do
      domList' <- mapM (fillType h holeSubst) domList
      cod' <- case cod of
        FCT.Cod t -> do
          t' <- fillType h holeSubst t
          return $ FCT.Cod t'
        FCT.Void ->
          return FCT.Void
      args' <- mapM (fill h holeSubst) args
      varArgs' <- forM varArgs $ \(arg, t) -> do
        arg' <- fill h holeSubst arg
        t' <- fillType h holeSubst t
        return (arg', t')
      return $ LM.External domList' cod' extFunName args' varArgs'
    LM.Global name t -> do
      t' <- fillType h holeSubst t
      return $ LM.Global name t'
    LM.OpaqueValue value -> do
      value' <- fill h holeSubst value
      return $ LM.OpaqueValue value'
    LM.CallType func arg1 arg2 -> do
      func' <- fill h holeSubst func
      arg1' <- fill h holeSubst arg1
      arg2' <- fill h holeSubst arg2
      return $ LM.CallType func' arg1' arg2'
