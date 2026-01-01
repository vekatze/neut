module Language.WeakTerm.Subst
  ( Handle,
    new,
    SubstType,
    subst,
    substType,
    substTypeWith,
    substTypeInTerm,
    subst',
    substDecisionTree,
  )
where

import Control.Comonad.Cofree
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IntMap qualified as IntMap
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Gensym.Gensym qualified as Gensym
import Gensym.Handle qualified as Gensym
import Language.Common.Annotation qualified as AN
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.Binder
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.DecisionTree qualified as DT
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.ImpArgs qualified as ImpArgs
import Language.Common.LamKind qualified as LK
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.WeakTerm.FreeVars qualified as WT
import Language.WeakTerm.WeakTerm qualified as WT

type SubstType =
  WT.SubstWeakType

newtype Handle = Handle
  { gensymHandle :: Gensym.Handle
  }

new :: Gensym.Handle -> Handle
new gensymHandle = do
  Handle {..}

subst :: Handle -> WT.SubstWeakTerm -> WT.WeakTerm -> IO WT.WeakTerm
subst h sub term =
  case term of
    m :< WT.Var x
      | Just varOrTerm <- IntMap.lookup (Ident.toInt x) sub ->
          case varOrTerm of
            Left x' ->
              return $ m :< WT.Var x'
            Right e ->
              return e
      | otherwise ->
          return term
    _ :< WT.VarGlobal {} ->
      return term
    m :< WT.PiIntro (AttrL.Attr {lamKind}) impArgs defaultArgs expArgs e -> do
      let fvs = S.map Ident.toInt $ WT.freeVars term
      let subDomSet = S.fromList $ IntMap.keys sub
      if S.intersection fvs subDomSet == S.empty
        then return term
        else do
          newLamID <- liftIO $ Gensym.newCount (gensymHandle h)
          case lamKind of
            LK.Fix xt -> do
              (impArgs', sub') <- subst' h sub impArgs
              (defaultArgs', sub'') <- substDefaultArgs h sub' defaultArgs
              (expArgs', sub''') <- subst' h sub'' expArgs
              ([xt'], sub'''') <- subst' h sub''' [xt]
              e' <- subst h sub'''' e
              let fixAttr = AttrL.Attr {lamKind = LK.Fix xt', identity = newLamID}
              return (m :< WT.PiIntro fixAttr impArgs' defaultArgs' expArgs' e')
            LK.Normal mName codType -> do
              (impArgs', sub') <- subst' h sub impArgs
              (defaultArgs', sub'') <- substDefaultArgs h sub' defaultArgs
              (expArgs', sub''') <- subst' h sub'' expArgs
              codType' <- substType h sub''' codType
              e' <- subst h sub''' e
              let lamAttr = AttrL.Attr {lamKind = LK.Normal mName codType', identity = newLamID}
              return (m :< WT.PiIntro lamAttr impArgs' defaultArgs' expArgs' e')
    m :< WT.PiElim b e impArgs expArgs -> do
      e' <- subst h sub e
      impArgs' <- ImpArgs.traverseImpArgs (substType h sub) impArgs
      expArgs' <- mapM (subst h sub) expArgs
      return $ m :< WT.PiElim b e' impArgs' expArgs'
    m :< WT.PiElimExact e -> do
      e' <- subst h sub e
      return $ m :< WT.PiElimExact e'
    m :< WT.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (substType h sub) dataArgs
      consArgs' <- mapM (subst h sub) consArgs
      attr' <- substAttrDataIntro h sub attr
      return $ m :< WT.DataIntro attr' consName dataArgs' consArgs'
    m :< WT.DataElim isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (subst h sub) es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      (binder', decisionTree') <- subst''' h sub binder decisionTree
      let (_, os', ts') = unzip3 binder'
      return $ m :< WT.DataElim isNoetic (zip3 os' es' ts') decisionTree'
    m :< WT.BoxIntro letSeq e -> do
      (letSeq', sub') <- substLetSeq h sub letSeq
      e' <- subst h sub' e
      return $ m :< WT.BoxIntro letSeq' e'
    m :< WT.BoxIntroLift e -> do
      e' <- subst h sub e
      return $ m :< WT.BoxIntroLift e'
    m :< WT.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      (castSeq', sub1) <- substLetSeq h sub castSeq
      ((mxt', e1'), sub2) <- substLet h sub1 (mxt, e1)
      (uncastSeq', sub3) <- substLetSeq h sub2 uncastSeq
      e2' <- subst h sub3 e2
      return $ m :< WT.BoxElim castSeq' mxt' e1' uncastSeq' e2'
    m :< WT.CodeIntro e -> do
      e' <- subst h sub e
      return $ m :< WT.CodeIntro e'
    m :< WT.CodeElim e -> do
      e' <- subst h sub e
      return $ m :< WT.CodeElim e'
    m :< WT.Actual e -> do
      e' <- subst h sub e
      return $ m :< WT.Actual e'
    m :< WT.Let opacity mxt e1 e2 -> do
      e1' <- subst h sub e1
      (mxt', _, e2') <- subst'' h sub mxt [] e2
      return $ m :< WT.Let opacity mxt' e1' e2'
    m :< WT.Prim prim -> do
      prim' <- mapM (substType h sub) prim
      return $ m :< WT.Prim prim'
    m :< WT.Magic der -> do
      der' <- substMagic h sub der
      return $ m :< WT.Magic der'
    m :< WT.Annotation logLevel annot e -> do
      e' <- subst h sub e
      case annot of
        AN.Type t -> do
          t' <- substType h sub t
          return $ m :< WT.Annotation logLevel (AN.Type t') e'

substType :: Handle -> WT.SubstWeakTerm -> WT.WeakType -> IO WT.WeakType
substType h sub ty =
  case ty of
    _ :< WT.Tau ->
      return ty
    m :< WT.TVar x
      | Just varOrTerm <- IntMap.lookup (Ident.toInt x) sub ->
          case varOrTerm of
            Left x' ->
              return $ m :< WT.TVar x'
            Right _ ->
              -- If we have a term substitution for a type variable, keep original
              -- (this shouldn't happen in well-typed code after separation)
              return ty
      | otherwise ->
          return ty
    _ :< WT.TVarGlobal {} ->
      return ty
    m :< WT.TyApp t args -> do
      t' <- substType h sub t
      args' <- mapM (substType h sub) args
      return $ m :< WT.TyApp t' args'
    m :< WT.Pi piKind impArgs defaultArgs expArgs t -> do
      (impArgs', sub') <- subst' h sub impArgs
      (defaultArgs', sub'') <- substDefaultArgs h sub' defaultArgs
      (expArgs', sub''') <- subst' h sub'' expArgs
      t' <- substType h sub''' t
      return $ m :< WT.Pi piKind impArgs' defaultArgs' expArgs' t'
    m :< WT.Data attr name es -> do
      es' <- mapM (substType h sub) es
      attr' <- substAttrData h sub attr
      return $ m :< WT.Data attr' name es'
    m :< WT.Box t -> do
      t' <- substType h sub t
      return $ m :< WT.Box t'
    m :< WT.BoxNoema t -> do
      t' <- substType h sub t
      return $ m :< WT.BoxNoema t'
    m :< WT.Code t -> do
      t' <- substType h sub t
      return $ m :< WT.Code t'
    _ :< WT.PrimType {} ->
      return ty
    _ :< WT.Void ->
      return ty
    m :< WT.Resource dd resourceID unitType discarder copier typeTag -> do
      unitType' <- substType h sub unitType
      discarder' <- subst h sub discarder
      copier' <- subst h sub copier
      typeTag' <- subst h sub typeTag
      return $ m :< WT.Resource dd resourceID unitType' discarder' copier' typeTag'
    m :< WT.TypeHole holeID es -> do
      es' <- mapM (substType h sub) es
      return $ m :< WT.TypeHole holeID es'

substBinder ::
  Handle ->
  WT.SubstWeakTerm ->
  [BinderF WT.WeakType] ->
  WT.WeakTerm ->
  IO ([BinderF WT.WeakType], WT.WeakTerm)
substBinder h sub binder e =
  case binder of
    [] -> do
      e' <- subst h sub e
      return ([], e')
    ((m, x, t) : xts) -> do
      t' <- substType h sub t
      x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
      let sub' = IntMap.insert (Ident.toInt x) (Left x') sub
      (xts', e') <- substBinder h sub' xts e
      return ((m, x', t') : xts', e')

subst' ::
  Handle ->
  WT.SubstWeakTerm ->
  [BinderF WT.WeakType] ->
  IO ([BinderF WT.WeakType], WT.SubstWeakTerm)
subst' h sub binder =
  case binder of
    [] -> do
      return ([], sub)
    ((m, x, t) : xts) -> do
      t' <- substType h sub t
      x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
      let sub' = IntMap.insert (Ident.toInt x) (Left x') sub
      (xts', sub'') <- subst' h sub' xts
      return ((m, x', t') : xts', sub'')

substDefaultArgs ::
  Handle ->
  WT.SubstWeakTerm ->
  [(BinderF WT.WeakType, WT.WeakTerm)] ->
  IO ([(BinderF WT.WeakType, WT.WeakTerm)], WT.SubstWeakTerm)
substDefaultArgs h sub binderList =
  case binderList of
    [] -> do
      return ([], sub)
    ((m, x, t), defaultValue) : rest -> do
      t' <- substType h sub t
      defaultValue' <- subst h sub defaultValue
      x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
      let sub' = IntMap.insert (Ident.toInt x) (Left x') sub
      (rest', sub'') <- substDefaultArgs h sub' rest
      return (((m, x', t'), defaultValue') : rest', sub'')

subst'' ::
  Handle ->
  WT.SubstWeakTerm ->
  BinderF WT.WeakType ->
  [BinderF WT.WeakType] ->
  WT.WeakTerm ->
  IO (BinderF WT.WeakType, [BinderF WT.WeakType], WT.WeakTerm)
subst'' h sub (m, x, t) binder e = do
  t' <- substType h sub t
  x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
  let sub' = IntMap.insert (Ident.toInt x) (Left x') sub
  (xts', e') <- substBinder h sub' binder e
  return ((m, x', t'), xts', e')

subst''' ::
  Handle ->
  WT.SubstWeakTerm ->
  [BinderF WT.WeakType] ->
  DT.DecisionTree WT.WeakType WT.WeakTerm ->
  IO ([BinderF WT.WeakType], DT.DecisionTree WT.WeakType WT.WeakTerm)
subst''' h sub binder decisionTree =
  case binder of
    [] -> do
      decisionTree' <- substDecisionTree h sub decisionTree
      return ([], decisionTree')
    ((m, x, t) : xts) -> do
      t' <- substType h sub t
      x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
      let sub' = IntMap.insert (Ident.toInt x) (Left x') sub
      (xts', e') <- subst''' h sub' xts decisionTree
      return ((m, x', t') : xts', e')

substLet ::
  Handle ->
  WT.SubstWeakTerm ->
  (BinderF WT.WeakType, WT.WeakTerm) ->
  IO ((BinderF WT.WeakType, WT.WeakTerm), WT.SubstWeakTerm)
substLet h sub ((m, x, t), e) = do
  e' <- subst h sub e
  t' <- substType h sub t
  x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
  let sub' = IntMap.insert (Ident.toInt x) (Left x') sub
  return (((m, x', t'), e'), sub')

substLetSeq ::
  Handle ->
  WT.SubstWeakTerm ->
  [(BinderF WT.WeakType, WT.WeakTerm)] ->
  IO ([(BinderF WT.WeakType, WT.WeakTerm)], WT.SubstWeakTerm)
substLetSeq h sub letSeq = do
  case letSeq of
    [] ->
      return ([], sub)
    letPair : rest -> do
      (letPair', sub') <- substLet h sub letPair
      (rest', sub'') <- substLetSeq h sub' rest
      return (letPair' : rest', sub'')

substDecisionTree ::
  Handle ->
  WT.SubstWeakTerm ->
  DT.DecisionTree WT.WeakType WT.WeakTerm ->
  IO (DT.DecisionTree WT.WeakType WT.WeakTerm)
substDecisionTree h sub tree =
  case tree of
    DT.Leaf xs letSeq e -> do
      let xs' = mapMaybe (substLeafVar sub) xs
      (letSeq', sub') <- substLetSeq h sub letSeq
      e' <- subst h sub' e
      return $ DT.Leaf xs' letSeq' e'
    DT.Unreachable ->
      return tree
    DT.Switch (cursorVar, cursor) caseList -> do
      let cursorVar' = substVar sub cursorVar
      cursor' <- substType h sub cursor
      caseList' <- substCaseList h sub caseList
      return $ DT.Switch (cursorVar', cursor') caseList'

substCaseList ::
  Handle ->
  WT.SubstWeakTerm ->
  DT.CaseList WT.WeakType WT.WeakTerm ->
  IO (DT.CaseList WT.WeakType WT.WeakTerm)
substCaseList h sub (fallbackClause, clauseList) = do
  fallbackClause' <- substDecisionTree h sub fallbackClause
  clauseList' <- mapM (substCase h sub) clauseList
  return (fallbackClause', clauseList')

substCase ::
  Handle ->
  WT.SubstWeakTerm ->
  DT.Case WT.WeakType WT.WeakTerm ->
  IO (DT.Case WT.WeakType WT.WeakTerm)
substCase h sub decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      cont' <- substDecisionTree h sub cont
      return $ DT.LiteralCase mPat i cont'
    DT.ConsCase record@(DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      dataTerms' <- mapM (substType h sub) dataTerms
      dataTypes' <- mapM (substType h sub) dataTypes
      (consArgs', cont') <- subst''' h sub consArgs cont
      return $
        DT.ConsCase
          record
            { DT.dataArgs = zip dataTerms' dataTypes',
              DT.consArgs = consArgs',
              DT.cont = cont'
            }

substLeafVar :: WT.SubstWeakTerm -> Ident -> Maybe Ident
substLeafVar sub leafVar =
  case IntMap.lookup (Ident.toInt leafVar) sub of
    Just (Left leafVar') ->
      return leafVar'
    Just (Right _) ->
      Nothing
    Nothing ->
      return leafVar


substVar :: WT.SubstWeakTerm -> Ident -> Ident
substVar sub x =
  case IntMap.lookup (Ident.toInt x) sub of
    Just (Left x') ->
      x'
    _ ->
      x

substAttrData :: Handle -> WT.SubstWeakTerm -> AttrD.Attr name (BinderF WT.WeakType) -> IO (AttrD.Attr name (BinderF WT.WeakType))
substAttrData h sub attr = do
  let consNameList = AttrD.consNameList attr
  consNameList' <- mapM (\(cn, binders, cl) -> do
    binders' <- mapM (\(mx, x, t) -> do
      t' <- substType h sub t
      return (mx, x, t')) binders
    return (cn, binders', cl)) consNameList
  return $ attr {AttrD.consNameList = consNameList'}

substAttrDataIntro :: Handle -> WT.SubstWeakTerm -> AttrDI.Attr name (BinderF WT.WeakType) -> IO (AttrDI.Attr name (BinderF WT.WeakType))
substAttrDataIntro h sub attr = do
  let consNameList = AttrDI.consNameList attr
  consNameList' <- mapM (\(cn, binders, cl) -> do
    binders' <- mapM (\(mx, x, t) -> do
      t' <- substType h sub t
      return (mx, x, t')) binders
    return (cn, binders', cl)) consNameList
  return $ attr {AttrDI.consNameList = consNameList'}

substMagic :: Handle -> WT.SubstWeakTerm -> WT.WeakMagic WT.WeakType WT.WeakType WT.WeakTerm -> IO (WT.WeakMagic WT.WeakType WT.WeakType WT.WeakTerm)
substMagic h sub (WT.WeakMagic magic) = do
  magic' <- case magic of
    M.LowMagic lowMagic -> do
      lowMagic' <- substLowMagic h sub lowMagic
      return $ M.LowMagic lowMagic'
    M.GetTypeTag sgl typeTagExpr e -> do
      typeTagExpr' <- substType h sub typeTagExpr
      e' <- substType h sub e
      return $ M.GetTypeTag sgl typeTagExpr' e'
    M.GetConsSize typeExpr -> do
      typeExpr' <- substType h sub typeExpr
      return $ M.GetConsSize typeExpr'
    M.GetConstructorArgTypes sgl listExpr typeExpr index -> do
      listExpr' <- substType h sub listExpr
      typeExpr' <- substType h sub typeExpr
      index' <- subst h sub index
      return $ M.GetConstructorArgTypes sgl listExpr' typeExpr' index'
    M.CompileError msg ->
      return $ M.CompileError msg
  return $ WT.WeakMagic magic'

substLowMagic :: Handle -> WT.SubstWeakTerm -> LM.LowMagic WT.WeakType WT.WeakType WT.WeakTerm -> IO (LM.LowMagic WT.WeakType WT.WeakType WT.WeakTerm)
substLowMagic h sub lowMagic =
  case lowMagic of
    LM.Cast from to value -> do
      from' <- substType h sub from
      to' <- substType h sub to
      value' <- subst h sub value
      return $ LM.Cast from' to' value'
    LM.Store t unit value pointer -> do
      t' <- substType h sub t
      unit' <- substType h sub unit
      value' <- subst h sub value
      pointer' <- subst h sub pointer
      return $ LM.Store t' unit' value' pointer'
    LM.Load t pointer -> do
      t' <- substType h sub t
      pointer' <- subst h sub pointer
      return $ LM.Load t' pointer'
    LM.Alloca t size -> do
      t' <- substType h sub t
      size' <- subst h sub size
      return $ LM.Alloca t' size'
    LM.External domList cod extFunName args varArgs -> do
      domList' <- mapM (substType h sub) domList
      cod' <- case cod of
        FCT.Cod t -> do
          t' <- substType h sub t
          return $ FCT.Cod t'
        FCT.Void ->
          return FCT.Void
      args' <- mapM (subst h sub) args
      varArgs' <- mapM (\(a, t) -> do
        a' <- subst h sub a
        t' <- substType h sub t
        return (a', t')) varArgs
      return $ LM.External domList' cod' extFunName args' varArgs'
    LM.Global name t -> do
      t' <- substType h sub t
      return $ LM.Global name t'
    LM.OpaqueValue e -> do
      e' <- subst h sub e
      return $ LM.OpaqueValue e'
    LM.CallType func arg1 arg2 -> do
      func' <- substType h sub func
      arg1' <- subst h sub arg1
      arg2' <- subst h sub arg2
      return $ LM.CallType func' arg1' arg2'
    LM.TermType ty -> do
      ty' <- substType h sub ty
      return $ LM.TermType ty'

substTypeWith :: SubstType -> WT.WeakType -> IO WT.WeakType
substTypeWith sub ty =
  case ty of
    _ :< WT.Tau ->
      return ty
    m :< WT.TVar x ->
      case IntMap.lookup (Ident.toInt x) sub of
        Just (Left x') ->
          return $ m :< WT.TVar x'
        Just (Right t) ->
          return t
        Nothing ->
          return ty
    _ :< WT.TVarGlobal {} ->
      return ty
    m :< WT.TyApp t args -> do
      t' <- substTypeWith sub t
      args' <- mapM (substTypeWith sub) args
      return $ m :< WT.TyApp t' args'
    m :< WT.Pi piKind impArgs defaultArgs expArgs t -> do
      impArgs' <- mapM (substTypeBinder sub) impArgs
      defaultArgs' <- mapM (substTypeDefaultArg sub) defaultArgs
      expArgs' <- mapM (substTypeBinder sub) expArgs
      let sub' = deleteBound sub (impArgs ++ map fst defaultArgs ++ expArgs)
      t' <- substTypeWith sub' t
      return $ m :< WT.Pi piKind impArgs' defaultArgs' expArgs' t'
    m :< WT.Data attr name es -> do
      es' <- mapM (substTypeWith sub) es
      attr' <- substTypeAttrData sub attr
      return $ m :< WT.Data attr' name es'
    m :< WT.Box t -> do
      t' <- substTypeWith sub t
      return $ m :< WT.Box t'
    m :< WT.BoxNoema t -> do
      t' <- substTypeWith sub t
      return $ m :< WT.BoxNoema t'
    m :< WT.Code t -> do
      t' <- substTypeWith sub t
      return $ m :< WT.Code t'
    _ :< WT.PrimType {} ->
      return ty
    _ :< WT.Void ->
      return ty
    m :< WT.Resource dd resourceID unitType discarder copier typeTag -> do
      unitType' <- substTypeWith sub unitType
      discarder' <- substTypeInTerm sub discarder
      copier' <- substTypeInTerm sub copier
      typeTag' <- substTypeInTerm sub typeTag
      return $ m :< WT.Resource dd resourceID unitType' discarder' copier' typeTag'
    m :< WT.TypeHole holeID es -> do
      es' <- mapM (substTypeWith sub) es
      return $ m :< WT.TypeHole holeID es'

substTypeInTerm :: SubstType -> WT.WeakTerm -> IO WT.WeakTerm
substTypeInTerm sub term =
  case term of
    _ :< WT.Var {} ->
      return term
    _ :< WT.VarGlobal {} ->
      return term
    m :< WT.PiIntro attr@(AttrL.Attr {lamKind}) impArgs defaultArgs expArgs e -> do
      impArgs' <- mapM (substTypeBinder sub) impArgs
      let sub' = deleteBound sub impArgs
      defaultArgs' <- mapM (substTypeDefaultArg sub') defaultArgs
      expArgs' <- mapM (substTypeBinder sub') expArgs
      e' <- substTypeInTerm sub' e
      case lamKind of
        LK.Fix xt -> do
          xt' <- substTypeBinder sub' xt
          return $ m :< WT.PiIntro (attr {AttrL.lamKind = LK.Fix xt'}) impArgs' defaultArgs' expArgs' e'
        LK.Normal name codType -> do
          codType' <- substTypeWith sub' codType
          return $ m :< WT.PiIntro (attr {AttrL.lamKind = LK.Normal name codType'}) impArgs' defaultArgs' expArgs' e'
    m :< WT.PiElim b e impArgs expArgs -> do
      e' <- substTypeInTerm sub e
      impArgs' <- mapM (substTypeWith sub) impArgs
      expArgs' <- mapM (substTypeInTerm sub) expArgs
      return $ m :< WT.PiElim b e' impArgs' expArgs'
    m :< WT.PiElimExact e -> do
      e' <- substTypeInTerm sub e
      return $ m :< WT.PiElimExact e'
    m :< WT.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (substTypeWith sub) dataArgs
      consArgs' <- mapM (substTypeInTerm sub) consArgs
      attr' <- substTypeAttrDataIntro sub attr
      return $ m :< WT.DataIntro attr' consName dataArgs' consArgs'
    m :< WT.DataElim isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (substTypeInTerm sub) es
      ts' <- mapM (substTypeWith sub) ts
      decisionTree' <- substTypeDecisionTree sub decisionTree
      return $ m :< WT.DataElim isNoetic (zip3 os es' ts') decisionTree'
    m :< WT.BoxIntro letSeq e -> do
      letSeq' <- mapM (substTypeLet sub) letSeq
      e' <- substTypeInTerm sub e
      return $ m :< WT.BoxIntro letSeq' e'
    m :< WT.BoxIntroLift e -> do
      e' <- substTypeInTerm sub e
      return $ m :< WT.BoxIntroLift e'
    m :< WT.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      castSeq' <- mapM (substTypeLet sub) castSeq
      (mxt', e1') <- substTypeLet sub (mxt, e1)
      uncastSeq' <- mapM (substTypeLet sub) uncastSeq
      e2' <- substTypeInTerm sub e2
      return $ m :< WT.BoxElim castSeq' mxt' e1' uncastSeq' e2'
    m :< WT.CodeIntro e -> do
      e' <- substTypeInTerm sub e
      return $ m :< WT.CodeIntro e'
    m :< WT.CodeElim e -> do
      e' <- substTypeInTerm sub e
      return $ m :< WT.CodeElim e'
    m :< WT.Actual e -> do
      e' <- substTypeInTerm sub e
      return $ m :< WT.Actual e'
    m :< WT.Let opacity mxt e1 e2 -> do
      (mxt', e1') <- substTypeLet sub (mxt, e1)
      e2' <- substTypeInTerm sub e2
      return $ m :< WT.Let opacity mxt' e1' e2'
    _ :< WT.Prim _ ->
      return term
    m :< WT.Magic der -> do
      der' <- substTypeMagic sub der
      return $ m :< WT.Magic der'
    m :< WT.Annotation logLevel annot e -> do
      e' <- substTypeInTerm sub e
      case annot of
        AN.Type t -> do
          t' <- substTypeWith sub t
          return $ m :< WT.Annotation logLevel (AN.Type t') e'

deleteBound :: SubstType -> [BinderF WT.WeakType] -> SubstType
deleteBound sub binders =
  foldr (IntMap.delete . Ident.toInt . (\(_, x, _) -> x)) sub binders

substTypeBinder :: SubstType -> BinderF WT.WeakType -> IO (BinderF WT.WeakType)
substTypeBinder sub (m, x, t) = do
  t' <- substTypeWith sub t
  return (m, x, t')

substTypeDefaultArg :: SubstType -> (BinderF WT.WeakType, WT.WeakTerm) -> IO (BinderF WT.WeakType, WT.WeakTerm)
substTypeDefaultArg sub (binder, value) = do
  binder' <- substTypeBinder sub binder
  value' <- substTypeInTerm sub value
  return (binder', value')

substTypeLet :: SubstType -> (BinderF WT.WeakType, WT.WeakTerm) -> IO (BinderF WT.WeakType, WT.WeakTerm)
substTypeLet sub (binder, value) = do
  binder' <- substTypeBinder sub binder
  value' <- substTypeInTerm sub value
  return (binder', value')

substTypeDecisionTree :: SubstType -> DT.DecisionTree WT.WeakType WT.WeakTerm -> IO (DT.DecisionTree WT.WeakType WT.WeakTerm)
substTypeDecisionTree sub tree =
  case tree of
    DT.Leaf xs letSeq e -> do
      letSeq' <- mapM (substTypeLet sub) letSeq
      e' <- substTypeInTerm sub e
      return $ DT.Leaf xs letSeq' e'
    DT.Unreachable ->
      return DT.Unreachable
    DT.Switch (cursorVar, cursor) caseList -> do
      cursor' <- substTypeWith sub cursor
      caseList' <- substTypeCaseList sub caseList
      return $ DT.Switch (cursorVar, cursor') caseList'

substTypeCaseList :: SubstType -> DT.CaseList WT.WeakType WT.WeakTerm -> IO (DT.CaseList WT.WeakType WT.WeakTerm)
substTypeCaseList sub (fallbackClause, clauseList) = do
  fallbackClause' <- substTypeDecisionTree sub fallbackClause
  clauseList' <- mapM (substTypeCase sub) clauseList
  return (fallbackClause', clauseList')

substTypeCase :: SubstType -> DT.Case WT.WeakType WT.WeakTerm -> IO (DT.Case WT.WeakType WT.WeakTerm)
substTypeCase sub decisionCase =
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      cont' <- substTypeDecisionTree sub cont
      return $ DT.LiteralCase mPat i cont'
    DT.ConsCase record@(DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      dataTerms' <- mapM (substTypeWith sub) dataTerms
      dataTypes' <- mapM (substTypeWith sub) dataTypes
      consArgs' <- mapM (substTypeBinder sub) consArgs
      cont' <- substTypeDecisionTree sub cont
      return $
        DT.ConsCase
          record
            { DT.dataArgs = zip dataTerms' dataTypes',
              DT.consArgs = consArgs',
              DT.cont = cont'
            }

substTypeAttrData :: SubstType -> AttrD.Attr name (BinderF WT.WeakType) -> IO (AttrD.Attr name (BinderF WT.WeakType))
substTypeAttrData sub attr = do
  let consNameList = AttrD.consNameList attr
  consNameList' <- mapM (\(cn, binders, cl) -> do
    binders' <- mapM (substTypeBinder sub) binders
    return (cn, binders', cl)) consNameList
  return $ attr {AttrD.consNameList = consNameList'}

substTypeAttrDataIntro :: SubstType -> AttrDI.Attr name (BinderF WT.WeakType) -> IO (AttrDI.Attr name (BinderF WT.WeakType))
substTypeAttrDataIntro sub attr = do
  let consNameList = AttrDI.consNameList attr
  consNameList' <- mapM (\(cn, binders, cl) -> do
    binders' <- mapM (substTypeBinder sub) binders
    return (cn, binders', cl)) consNameList
  return $ attr {AttrDI.consNameList = consNameList'}

substTypeMagic :: SubstType -> WT.WeakMagic WT.WeakType WT.WeakType WT.WeakTerm -> IO (WT.WeakMagic WT.WeakType WT.WeakType WT.WeakTerm)
substTypeMagic sub (WT.WeakMagic magic) = do
  magic' <- case magic of
    M.LowMagic lowMagic -> do
      lowMagic' <- substTypeLowMagic sub lowMagic
      return $ M.LowMagic lowMagic'
    M.GetTypeTag sgl typeTagExpr e -> do
      typeTagExpr' <- substTypeWith sub typeTagExpr
      e' <- substTypeWith sub e
      return $ M.GetTypeTag sgl typeTagExpr' e'
    M.GetConsSize typeExpr -> do
      M.GetConsSize <$> substTypeWith sub typeExpr
    M.GetConstructorArgTypes sgl listExpr typeExpr index -> do
      listExpr' <- substTypeWith sub listExpr
      typeExpr' <- substTypeWith sub typeExpr
      index' <- substTypeInTerm sub index
      return $ M.GetConstructorArgTypes sgl listExpr' typeExpr' index'
    M.CompileError msg ->
      return $ M.CompileError msg
  return $ WT.WeakMagic magic'

substTypeLowMagic :: SubstType -> LM.LowMagic WT.WeakType WT.WeakType WT.WeakTerm -> IO (LM.LowMagic WT.WeakType WT.WeakType WT.WeakTerm)
substTypeLowMagic sub lowMagic =
  case lowMagic of
    LM.Cast from to value -> do
      from' <- substTypeWith sub from
      to' <- substTypeWith sub to
      value' <- substTypeInTerm sub value
      return $ LM.Cast from' to' value'
    LM.Store t unit value pointer -> do
      t' <- substTypeWith sub t
      unit' <- substTypeWith sub unit
      value' <- substTypeInTerm sub value
      pointer' <- substTypeInTerm sub pointer
      return $ LM.Store t' unit' value' pointer'
    LM.Load t pointer -> do
      t' <- substTypeWith sub t
      pointer' <- substTypeInTerm sub pointer
      return $ LM.Load t' pointer'
    LM.Alloca t size -> do
      t' <- substTypeWith sub t
      size' <- substTypeInTerm sub size
      return $ LM.Alloca t' size'
    LM.External domList cod extFunName args varArgs -> do
      domList' <- mapM (substTypeWith sub) domList
      cod' <- traverse (substTypeWith sub) cod
      args' <- mapM (substTypeInTerm sub) args
      varArgs' <- mapM (\(a, t) -> do
        a' <- substTypeInTerm sub a
        t' <- substTypeWith sub t
        return (a', t')) varArgs
      return $ LM.External domList' cod' extFunName args' varArgs'
    LM.Global name t -> do
      t' <- substTypeWith sub t
      return $ LM.Global name t'
    LM.OpaqueValue e -> do
      e' <- substTypeInTerm sub e
      return $ LM.OpaqueValue e'
    LM.CallType func arg1 arg2 -> do
      func' <- substTypeWith sub func
      arg1' <- substTypeInTerm sub arg1
      arg2' <- substTypeInTerm sub arg2
      return $ LM.CallType func' arg1' arg2'
    LM.TermType ty -> do
      ty' <- substTypeWith sub ty
      return $ LM.TermType ty'
