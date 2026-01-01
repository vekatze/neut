module Language.Term.Subst
  ( Handle,
    new,
    SubstTerm,
    SubstType,
    subst,
    substWithType,
    substType,
    substTypeWith,
    subst',
    subst'',
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
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.BaseLowType qualified as BLT
import Language.Common.Binder
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.DecisionTree qualified as DT
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.LamKind qualified as LK
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.Term.FreeVars qualified as TM
import Language.Term.Term qualified as TM

type SubstTerm =
  IntMap.IntMap (Either Ident TM.Term)

type SubstType =
  IntMap.IntMap (Either Ident TM.Type)

newtype Handle = Handle
  { gensymHandle :: Gensym.Handle
  }

new :: Gensym.Handle -> Handle
new gensymHandle = do
  Handle {..}

subst :: Handle -> SubstTerm -> TM.Term -> IO TM.Term
subst h sub term =
  case term of
    m :< TM.Var x
      | Just varOrTerm <- IntMap.lookup (Ident.toInt x) sub ->
          case varOrTerm of
            Left x' ->
              return $ m :< TM.Var x'
            Right e ->
              return e
      | otherwise ->
          return term
    _ :< TM.VarGlobal {} ->
      return term
    m :< TM.PiIntro (AttrL.Attr {lamKind}) impArgs defaultArgs expArgs e -> do
      let fvs = S.map Ident.toInt $ TM.freeVars term
      let subDomSet = S.fromList $ IntMap.keys sub
      if S.intersection fvs subDomSet == S.empty
        then return term
        else do
          newLamID <- liftIO $ Gensym.newCount (gensymHandle h)
          case lamKind of
            LK.Fix xt -> do
              (impArgs', sub') <- substBinder h sub impArgs
              (defaultArgs', sub'') <- substDefaultArgs h sub' defaultArgs
              (expArgs', sub''') <- substBinder h sub'' expArgs
              ([xt'], sub'''') <- substBinder h sub''' [xt]
              e' <- subst h sub'''' e
              let fixAttr = AttrL.Attr {lamKind = LK.Fix xt', identity = newLamID}
              return (m :< TM.PiIntro fixAttr impArgs' defaultArgs' expArgs' e')
            LK.Normal name codType -> do
              (impArgs', sub') <- substBinder h sub impArgs
              (defaultArgs', sub'') <- substDefaultArgs h sub' defaultArgs
              (expArgs', sub''') <- substBinder h sub'' expArgs
              codType' <- substType h sub''' codType
              e' <- subst h sub''' e
              let lamAttr = AttrL.Attr {lamKind = LK.Normal name codType', identity = newLamID}
              return (m :< TM.PiIntro lamAttr impArgs' defaultArgs' expArgs' e')
    m :< TM.PiElim b e impArgs expArgs -> do
      e' <- subst h sub e
      impArgs' <- mapM (substType h sub) impArgs
      expArgs' <- mapM (subst h sub) expArgs
      return (m :< TM.PiElim b e' impArgs' expArgs')
    m :< TM.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (substType h sub) dataArgs
      consArgs' <- mapM (subst h sub) consArgs
      attr' <- substAttrDataIntro h sub attr
      return $ m :< TM.DataIntro attr' consName dataArgs' consArgs'
    m :< TM.DataElim isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (subst h sub) es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      (binder', decisionTree') <- subst'' h sub binder decisionTree
      let (_, os', ts') = unzip3 binder'
      return $ m :< TM.DataElim isNoetic (zip3 os' es' ts') decisionTree'
    m :< TM.BoxIntro letSeq e -> do
      (letSeq', sub') <- substLetSeq h sub letSeq
      e' <- subst h sub' e
      return $ m :< TM.BoxIntro letSeq' e'
    m :< TM.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      (castSeq', sub1) <- substLetSeq h sub castSeq
      ((mxt', e1'), sub2) <- substLet h sub1 (mxt, e1)
      (uncastSeq', sub3) <- substLetSeq h sub2 uncastSeq
      e2' <- subst h sub3 e2
      return $ m :< TM.BoxElim castSeq' mxt' e1' uncastSeq' e2'
    m :< TM.CodeIntro e -> do
      e' <- subst h sub e
      return $ m :< TM.CodeIntro e'
    m :< TM.CodeElim e -> do
      e' <- subst h sub e
      return $ m :< TM.CodeElim e'
    m :< TM.Let opacity mxt e1 e2 -> do
      e1' <- subst h sub e1
      ([mxt'], e2') <- subst' h sub [mxt] e2
      return $ m :< TM.Let opacity mxt' e1' e2'
    _ :< TM.Prim _ ->
      return term
    m :< TM.Magic der -> do
      der' <- substMagic h sub der
      return (m :< TM.Magic der')

substType :: Handle -> SubstTerm -> TM.Type -> IO TM.Type
substType h sub ty =
  case ty of
    _ :< TM.Tau ->
      return ty
    m :< TM.TVar x
      | Just varOrTerm <- IntMap.lookup (Ident.toInt x) sub ->
          case varOrTerm of
            Left x' ->
              return $ m :< TM.TVar x'
            Right (_ :< TM.Var x') ->
              return $ m :< TM.TVar x'
            Right _ ->
              return ty
      | otherwise ->
          return ty
    _ :< TM.TVarGlobal {} ->
      return ty
    m :< TM.TyApp t args -> do
      t' <- substType h sub t
      args' <- mapM (substType h sub) args
      return $ m :< TM.TyApp t' args'
    m :< TM.Pi piKind impArgs defaultArgs expArgs t -> do
      (impArgs', sub') <- substBinder h sub impArgs
      (defaultArgs', sub'') <- substDefaultArgs h sub' defaultArgs
      (expArgs', sub''') <- substBinder h sub'' expArgs
      t' <- substType h sub''' t
      return (m :< TM.Pi piKind impArgs' defaultArgs' expArgs' t')
    m :< TM.Data attr name es -> do
      es' <- mapM (substType h sub) es
      attr' <- substAttrData h sub attr
      return $ m :< TM.Data attr' name es'
    m :< TM.Box t -> do
      t' <- substType h sub t
      return $ m :< TM.Box t'
    m :< TM.BoxNoema t -> do
      t' <- substType h sub t
      return $ m :< TM.BoxNoema t'
    m :< TM.Code t -> do
      t' <- substType h sub t
      return $ m :< TM.Code t'
    _ :< TM.PrimType {} ->
      return ty
    _ :< TM.Void ->
      return ty
    m :< TM.Resource dd resourceID unitType discarder copier typeTag -> do
      unitType' <- substType h sub unitType
      discarder' <- subst h sub discarder
      copier' <- subst h sub copier
      typeTag' <- subst h sub typeTag
      return $ m :< TM.Resource dd resourceID unitType' discarder' copier' typeTag'

substBinder ::
  Handle ->
  SubstTerm ->
  [BinderF TM.Type] ->
  IO ([BinderF TM.Type], SubstTerm)
substBinder h sub binder =
  case binder of
    [] -> do
      return ([], sub)
    ((m, x, t) : xts) -> do
      t' <- substType h sub t
      x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
      let sub' = IntMap.insert (Ident.toInt x) (Left x') sub
      (xts', sub'') <- substBinder h sub' xts
      return ((m, x', t') : xts', sub'')

substDefaultArgs ::
  Handle ->
  SubstTerm ->
  [(BinderF TM.Type, TM.Term)] ->
  IO ([(BinderF TM.Type, TM.Term)], SubstTerm)
substDefaultArgs h sub binderList =
  case binderList of
    [] -> do
      return ([], sub)
    ((m, x, t), defaultValue) : xts -> do
      t' <- substType h sub t
      defaultValue' <- subst h sub defaultValue
      x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
      let sub' = IntMap.insert (Ident.toInt x) (Left x') sub
      (xts', sub'') <- substDefaultArgs h sub' xts
      return (((m, x', t'), defaultValue') : xts', sub'')

subst' ::
  Handle ->
  SubstTerm ->
  [BinderF TM.Type] ->
  TM.Term ->
  IO ([BinderF TM.Type], TM.Term)
subst' h sub binder e =
  case binder of
    [] -> do
      e' <- subst h sub e
      return ([], e')
    ((m, x, t) : xts) -> do
      t' <- substType h sub t
      x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
      let sub' = IntMap.insert (Ident.toInt x) (Left x') sub
      (xts', e') <- subst' h sub' xts e
      return ((m, x', t') : xts', e')

subst'' ::
  Handle ->
  SubstTerm ->
  [BinderF TM.Type] ->
  DT.DecisionTree TM.Type TM.Term ->
  IO ([BinderF TM.Type], DT.DecisionTree TM.Type TM.Term)
subst'' h sub binder decisionTree =
  case binder of
    [] -> do
      decisionTree' <- substDecisionTree h sub decisionTree
      return ([], decisionTree')
    ((m, x, t) : xts) -> do
      t' <- substType h sub t
      x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
      let sub' = IntMap.insert (Ident.toInt x) (Left x') sub
      (xts', e') <- subst'' h sub' xts decisionTree
      return ((m, x', t') : xts', e')

substLet ::
  Handle ->
  SubstTerm ->
  (BinderF TM.Type, TM.Term) ->
  IO ((BinderF TM.Type, TM.Term), SubstTerm)
substLet h sub ((m, x, t), e) = do
  e' <- subst h sub e
  t' <- substType h sub t
  x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
  let sub' = IntMap.insert (Ident.toInt x) (Left x') sub
  return (((m, x', t'), e'), sub')

substLetSeq ::
  Handle ->
  SubstTerm ->
  [(BinderF TM.Type, TM.Term)] ->
  IO ([(BinderF TM.Type, TM.Term)], SubstTerm)
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
  SubstTerm ->
  DT.DecisionTree TM.Type TM.Term ->
  IO (DT.DecisionTree TM.Type TM.Term)
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
  SubstTerm ->
  DT.CaseList TM.Type TM.Term ->
  IO (DT.CaseList TM.Type TM.Term)
substCaseList h sub (fallbackClause, clauseList) = do
  fallbackClause' <- substDecisionTree h sub fallbackClause
  clauseList' <- mapM (substCase h sub) clauseList
  return (fallbackClause', clauseList')

substCase ::
  Handle ->
  SubstTerm ->
  DT.Case TM.Type TM.Term ->
  IO (DT.Case TM.Type TM.Term)
substCase h sub decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      cont' <- substDecisionTree h sub cont
      return $ DT.LiteralCase mPat i cont'
    DT.ConsCase record@(DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      dataTerms' <- mapM (substType h sub) dataTerms
      dataTypes' <- mapM (substType h sub) dataTypes
      (consArgs', cont') <- subst'' h sub consArgs cont
      return $
        DT.ConsCase
          record
            { DT.dataArgs = zip dataTerms' dataTypes',
              DT.consArgs = consArgs',
              DT.cont = cont'
            }

substLeafVar :: SubstTerm -> Ident -> Maybe Ident
substLeafVar sub leafVar =
  case IntMap.lookup (Ident.toInt leafVar) sub of
    Just (Left leafVar') ->
      return leafVar'
    Just (Right _) ->
      Nothing
    Nothing ->
      return leafVar

substVar :: SubstTerm -> Ident -> Ident
substVar sub x =
  case IntMap.lookup (Ident.toInt x) sub of
    Just (Left x') ->
      x'
    _ ->
      x

substAttrData :: Handle -> SubstTerm -> AttrD.Attr name (BinderF TM.Type) -> IO (AttrD.Attr name (BinderF TM.Type))
substAttrData h sub attr = do
  let consNameList = AttrD.consNameList attr
  consNameList' <- mapM (\(cn, binders, cl) -> do
    binders' <- mapM (\(mx, x, t) -> do
      t' <- substType h sub t
      return (mx, x, t')) binders
    return (cn, binders', cl)) consNameList
  return $ attr {AttrD.consNameList = consNameList'}

substAttrDataIntro :: Handle -> SubstTerm -> AttrDI.Attr name (BinderF TM.Type) -> IO (AttrDI.Attr name (BinderF TM.Type))
substAttrDataIntro h sub attr = do
  let consNameList = AttrDI.consNameList attr
  consNameList' <- mapM (\(cn, binders, cl) -> do
    binders' <- mapM (\(mx, x, t) -> do
      t' <- substType h sub t
      return (mx, x, t')) binders
    return (cn, binders', cl)) consNameList
  return $ attr {AttrDI.consNameList = consNameList'}

substWithType :: Handle -> SubstTerm -> SubstType -> TM.Term -> IO TM.Term
substWithType h subTerm subType term = do
  term' <- substTypeInTerm subType term
  subst h subTerm term'

substTypeWith :: SubstType -> TM.Type -> IO TM.Type
substTypeWith sub ty =
  case ty of
    _ :< TM.Tau ->
      return ty
    m :< TM.TVar x ->
      case IntMap.lookup (Ident.toInt x) sub of
        Just (Left x') ->
          return $ m :< TM.TVar x'
        Just (Right t) ->
          return t
        Nothing ->
          return ty
    _ :< TM.TVarGlobal {} ->
      return ty
    m :< TM.TyApp t args -> do
      t' <- substTypeWith sub t
      args' <- mapM (substTypeWith sub) args
      return $ m :< TM.TyApp t' args'
    m :< TM.Pi piKind impArgs defaultArgs expArgs t -> do
      impArgs' <- mapM (substTypeBinder sub) impArgs
      defaultArgs' <- mapM (substTypeDefaultArg sub) defaultArgs
      expArgs' <- mapM (substTypeBinder sub) expArgs
      let sub' = deleteBound sub (impArgs ++ map fst defaultArgs ++ expArgs)
      t' <- substTypeWith sub' t
      return $ m :< TM.Pi piKind impArgs' defaultArgs' expArgs' t'
    m :< TM.Data attr name es -> do
      es' <- mapM (substTypeWith sub) es
      attr' <- substTypeAttrData sub attr
      return $ m :< TM.Data attr' name es'
    m :< TM.Box t -> do
      t' <- substTypeWith sub t
      return $ m :< TM.Box t'
    m :< TM.BoxNoema t -> do
      t' <- substTypeWith sub t
      return $ m :< TM.BoxNoema t'
    m :< TM.Code t -> do
      t' <- substTypeWith sub t
      return $ m :< TM.Code t'
    _ :< TM.PrimType {} ->
      return ty
    _ :< TM.Void ->
      return ty
    m :< TM.Resource dd resourceID unitType discarder copier typeTag -> do
      unitType' <- substTypeWith sub unitType
      discarder' <- substTypeInTerm sub discarder
      copier' <- substTypeInTerm sub copier
      typeTag' <- substTypeInTerm sub typeTag
      return $ m :< TM.Resource dd resourceID unitType' discarder' copier' typeTag'

substTypeInTerm :: SubstType -> TM.Term -> IO TM.Term
substTypeInTerm sub term =
  case term of
    _ :< TM.Var {} ->
      return term
    _ :< TM.VarGlobal {} ->
      return term
    m :< TM.PiIntro attr@(AttrL.Attr {lamKind}) impArgs defaultArgs expArgs e -> do
      impArgs' <- mapM (substTypeBinder sub) impArgs
      let sub' = deleteBound sub impArgs
      defaultArgs' <- mapM (substTypeDefaultArg sub') defaultArgs
      expArgs' <- mapM (substTypeBinder sub') expArgs
      e' <- substTypeInTerm sub' e
      case lamKind of
        LK.Fix xt -> do
          xt' <- substTypeBinder sub' xt
          return $ m :< TM.PiIntro (attr {AttrL.lamKind = LK.Fix xt'}) impArgs' defaultArgs' expArgs' e'
        LK.Normal name codType -> do
          codType' <- substTypeWith sub' codType
          return $ m :< TM.PiIntro (attr {AttrL.lamKind = LK.Normal name codType'}) impArgs' defaultArgs' expArgs' e'
    m :< TM.PiElim b e impArgs expArgs -> do
      e' <- substTypeInTerm sub e
      impArgs' <- mapM (substTypeWith sub) impArgs
      expArgs' <- mapM (substTypeInTerm sub) expArgs
      return $ m :< TM.PiElim b e' impArgs' expArgs'
    m :< TM.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (substTypeWith sub) dataArgs
      consArgs' <- mapM (substTypeInTerm sub) consArgs
      attr' <- substTypeAttrDataIntro sub attr
      return $ m :< TM.DataIntro attr' consName dataArgs' consArgs'
    m :< TM.DataElim isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (substTypeInTerm sub) es
      ts' <- mapM (substTypeWith sub) ts
      decisionTree' <- substTypeDecisionTree sub decisionTree
      return $ m :< TM.DataElim isNoetic (zip3 os es' ts') decisionTree'
    m :< TM.BoxIntro letSeq e -> do
      letSeq' <- mapM (substTypeLet sub) letSeq
      e' <- substTypeInTerm sub e
      return $ m :< TM.BoxIntro letSeq' e'
    m :< TM.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      castSeq' <- mapM (substTypeLet sub) castSeq
      (mxt', e1') <- substTypeLet sub (mxt, e1)
      uncastSeq' <- mapM (substTypeLet sub) uncastSeq
      e2' <- substTypeInTerm sub e2
      return $ m :< TM.BoxElim castSeq' mxt' e1' uncastSeq' e2'
    m :< TM.CodeIntro e -> do
      e' <- substTypeInTerm sub e
      return $ m :< TM.CodeIntro e'
    m :< TM.CodeElim e -> do
      e' <- substTypeInTerm sub e
      return $ m :< TM.CodeElim e'
    m :< TM.Let opacity mxt e1 e2 -> do
      (mxt', e1') <- substTypeLet sub (mxt, e1)
      e2' <- substTypeInTerm sub e2
      return $ m :< TM.Let opacity mxt' e1' e2'
    _ :< TM.Prim _ ->
      return term
    m :< TM.Magic der -> do
      der' <- substTypeMagic sub der
      return (m :< TM.Magic der')

deleteBound :: SubstType -> [BinderF TM.Type] -> SubstType
deleteBound sub binders =
  foldr (IntMap.delete . Ident.toInt . (\(_, x, _) -> x)) sub binders

substTypeBinder :: SubstType -> BinderF TM.Type -> IO (BinderF TM.Type)
substTypeBinder sub (m, x, t) = do
  t' <- substTypeWith sub t
  return (m, x, t')

substTypeDefaultArg :: SubstType -> (BinderF TM.Type, TM.Term) -> IO (BinderF TM.Type, TM.Term)
substTypeDefaultArg sub (binder, value) = do
  binder' <- substTypeBinder sub binder
  value' <- substTypeInTerm sub value
  return (binder', value')

substTypeLet :: SubstType -> (BinderF TM.Type, TM.Term) -> IO (BinderF TM.Type, TM.Term)
substTypeLet sub (binder, value) = do
  binder' <- substTypeBinder sub binder
  value' <- substTypeInTerm sub value
  return (binder', value')

substTypeDecisionTree :: SubstType -> DT.DecisionTree TM.Type TM.Term -> IO (DT.DecisionTree TM.Type TM.Term)
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

substTypeCaseList :: SubstType -> DT.CaseList TM.Type TM.Term -> IO (DT.CaseList TM.Type TM.Term)
substTypeCaseList sub (fallbackClause, clauseList) = do
  fallbackClause' <- substTypeDecisionTree sub fallbackClause
  clauseList' <- mapM (substTypeCase sub) clauseList
  return (fallbackClause', clauseList')

substTypeCase :: SubstType -> DT.Case TM.Type TM.Term -> IO (DT.Case TM.Type TM.Term)
substTypeCase sub decisionCase =
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      cont' <- substTypeDecisionTree sub cont
      return $ DT.LiteralCase mPat i cont'
    DT.ConsCase record@DT.ConsCaseRecord {..} -> do
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

substTypeAttrData :: SubstType -> AttrD.Attr name (BinderF TM.Type) -> IO (AttrD.Attr name (BinderF TM.Type))
substTypeAttrData sub attr = do
  let consNameList = AttrD.consNameList attr
  consNameList' <- mapM (\(cn, binders, cl) -> do
    binders' <- mapM (\(mx, x, t) -> do
      t' <- substTypeWith sub t
      return (mx, x, t')) binders
    return (cn, binders', cl)) consNameList
  return $ attr {AttrD.consNameList = consNameList'}

substTypeAttrDataIntro :: SubstType -> AttrDI.Attr name (BinderF TM.Type) -> IO (AttrDI.Attr name (BinderF TM.Type))
substTypeAttrDataIntro sub attr = do
  let consNameList = AttrDI.consNameList attr
  consNameList' <- mapM (\(cn, binders, cl) -> do
    binders' <- mapM (\(mx, x, t) -> do
      t' <- substTypeWith sub t
      return (mx, x, t')) binders
    return (cn, binders', cl)) consNameList
  return $ attr {AttrDI.consNameList = consNameList'}

substMagic :: Handle -> SubstTerm -> M.Magic BLT.BaseLowType TM.Type TM.Term -> IO (M.Magic BLT.BaseLowType TM.Type TM.Term)
substMagic h sub magic =
  case magic of
    M.LowMagic lowMagic ->
      M.LowMagic <$> substLowMagic h sub lowMagic
    M.GetTypeTag mid typeTagExpr e -> do
      typeTagExpr' <- substType h sub typeTagExpr
      e' <- substType h sub e
      return $ M.GetTypeTag mid typeTagExpr' e'
    M.GetConsSize typeExpr ->
      M.GetConsSize <$> substType h sub typeExpr
    M.GetConstructorArgTypes sgl listExpr typeExpr index -> do
      listExpr' <- substType h sub listExpr
      typeExpr' <- substType h sub typeExpr
      index' <- subst h sub index
      return $ M.GetConstructorArgTypes sgl listExpr' typeExpr' index'
    M.CompileError msg ->
      return $ M.CompileError msg

substLowMagic :: Handle -> SubstTerm -> LM.LowMagic BLT.BaseLowType TM.Type TM.Term -> IO (LM.LowMagic BLT.BaseLowType TM.Type TM.Term)
substLowMagic h sub lowMagic =
  case lowMagic of
    LM.Cast from to value -> do
      from' <- substType h sub from
      to' <- substType h sub to
      value' <- subst h sub value
      return $ LM.Cast from' to' value'
    LM.Store t unit value pointer -> do
      value' <- subst h sub value
      pointer' <- subst h sub pointer
      return $ LM.Store t unit value' pointer'
    LM.Load t pointer -> do
      pointer' <- subst h sub pointer
      return $ LM.Load t pointer'
    LM.Alloca t size -> do
      size' <- subst h sub size
      return $ LM.Alloca t size'
    LM.External domList cod extFunName args varArgs -> do
      args' <- mapM (subst h sub) args
      varArgs' <- mapM (\(arg, typ) -> do
        arg' <- subst h sub arg
        return (arg', typ)) varArgs
      return $ LM.External domList cod extFunName args' varArgs'
    LM.Global name t ->
      return $ LM.Global name t
    LM.OpaqueValue e ->
      LM.OpaqueValue <$> subst h sub e
    LM.CallType func arg1 arg2 -> do
      func' <- substType h sub func
      arg1' <- subst h sub arg1
      arg2' <- subst h sub arg2
      return $ LM.CallType func' arg1' arg2'
    LM.TermType ty -> do
      ty' <- substType h sub ty
      return $ LM.TermType ty'

substTypeMagic :: SubstType -> M.Magic BLT.BaseLowType TM.Type TM.Term -> IO (M.Magic BLT.BaseLowType TM.Type TM.Term)
substTypeMagic sub magic =
  case magic of
    M.LowMagic lowMagic ->
      M.LowMagic <$> substTypeLowMagic sub lowMagic
    M.GetTypeTag mid typeTagExpr e -> do
      typeTagExpr' <- substTypeWith sub typeTagExpr
      e' <- substTypeWith sub e
      return $ M.GetTypeTag mid typeTagExpr' e'
    M.GetConsSize typeExpr ->
      M.GetConsSize <$> substTypeWith sub typeExpr
    M.GetConstructorArgTypes sgl listExpr typeExpr index -> do
      listExpr' <- substTypeWith sub listExpr
      typeExpr' <- substTypeWith sub typeExpr
      index' <- substTypeInTerm sub index
      return $ M.GetConstructorArgTypes sgl listExpr' typeExpr' index'
    M.CompileError msg ->
      return $ M.CompileError msg

substTypeLowMagic :: SubstType -> LM.LowMagic BLT.BaseLowType TM.Type TM.Term -> IO (LM.LowMagic BLT.BaseLowType TM.Type TM.Term)
substTypeLowMagic sub lowMagic =
  case lowMagic of
    LM.Cast from to value -> do
      from' <- substTypeWith sub from
      to' <- substTypeWith sub to
      value' <- substTypeInTerm sub value
      return $ LM.Cast from' to' value'
    LM.Store t unit value pointer -> do
      value' <- substTypeInTerm sub value
      pointer' <- substTypeInTerm sub pointer
      return $ LM.Store t unit value' pointer'
    LM.Load t pointer -> do
      pointer' <- substTypeInTerm sub pointer
      return $ LM.Load t pointer'
    LM.Alloca t size -> do
      size' <- substTypeInTerm sub size
      return $ LM.Alloca t size'
    LM.External domList cod extFunName args varArgs -> do
      args' <- mapM (substTypeInTerm sub) args
      varArgs' <- mapM (\(arg, typ) -> do
        arg' <- substTypeInTerm sub arg
        return (arg', typ)) varArgs
      return $ LM.External domList cod extFunName args' varArgs'
    LM.Global name t ->
      return $ LM.Global name t
    LM.OpaqueValue e ->
      LM.OpaqueValue <$> substTypeInTerm sub e
    LM.CallType func arg1 arg2 -> do
      func' <- substTypeWith sub func
      arg1' <- substTypeInTerm sub arg1
      arg2' <- substTypeInTerm sub arg2
      return $ LM.CallType func' arg1' arg2'
    LM.TermType ty -> do
      ty' <- substTypeWith sub ty
      return $ LM.TermType ty'
