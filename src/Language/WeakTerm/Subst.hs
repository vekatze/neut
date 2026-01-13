module Language.WeakTerm.Subst
  ( Handle,
    new,
    SubstEntry (..),
    Subst,
    subst,
    substType,
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
import Language.Common.DefaultArgs qualified as DefaultArgs
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.ImpArgs qualified as ImpArgs
import Language.Common.LamKind qualified as LK
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.WeakTerm.FreeVars qualified as WT
import Language.WeakTerm.WeakTerm qualified as WT

data SubstEntry
  = Var Ident
  | Term WT.WeakTerm
  | Type WT.WeakType

type Subst =
  IntMap.IntMap SubstEntry

newtype Handle = Handle
  { gensymHandle :: Gensym.Handle
  }

new :: Gensym.Handle -> Handle
new gensymHandle = do
  Handle {..}

subst :: Handle -> Subst -> WT.WeakTerm -> IO WT.WeakTerm
subst h sub term =
  case term of
    m :< WT.Var x
      | Just entry <- IntMap.lookup (Ident.toInt x) sub ->
          case entry of
            Var x' ->
              return $ m :< WT.Var x'
            Term e ->
              return e
            Type (_ :< WT.TVar x') ->
              return $ m :< WT.Var x'
            Type _ ->
              return term
      | otherwise ->
          return term
    _ :< WT.VarGlobal {} ->
      return term
    m :< WT.PiIntro (AttrL.Attr {lamKind}) impArgs expArgs defaultArgs e -> do
      let fvs = S.map Ident.toInt $ WT.freeVarsAll term
      let subDomSet = S.fromList $ IntMap.keys sub
      if S.intersection fvs subDomSet == S.empty
        then return term
        else do
          newLamID <- liftIO $ Gensym.newCount (gensymHandle h)
          case lamKind of
            LK.Fix opacity xt -> do
              (impArgs', sub') <- subst' h sub impArgs
              (expArgs', sub'') <- subst' h sub' expArgs
              (defaultArgs', sub''') <- substDefaultArgs h sub'' defaultArgs
              ([xt'], sub'''') <- subst' h sub''' [xt]
              e' <- subst h sub'''' e
              let fixAttr = AttrL.Attr {lamKind = LK.Fix opacity xt', identity = newLamID}
              return (m :< WT.PiIntro fixAttr impArgs' expArgs' defaultArgs' e')
            LK.Normal mName codType -> do
              (impArgs', sub') <- subst' h sub impArgs
              (expArgs', sub'') <- subst' h sub' expArgs
              (defaultArgs', sub''') <- substDefaultArgs h sub'' defaultArgs
              codType' <- substType h sub''' codType
              e' <- subst h sub''' e
              let lamAttr = AttrL.Attr {lamKind = LK.Normal mName codType', identity = newLamID}
              return (m :< WT.PiIntro lamAttr impArgs' expArgs' defaultArgs' e')
    m :< WT.PiElim b e impArgs expArgs defaultArgs -> do
      e' <- subst h sub e
      impArgs' <- ImpArgs.traverseImpArgs (substType h sub) impArgs
      defaultArgs' <- DefaultArgs.traverseDefaultArgs (subst h sub) defaultArgs
      expArgs' <- mapM (subst h sub) expArgs
      return $ m :< WT.PiElim b e' impArgs' expArgs' defaultArgs'
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
    m :< WT.TauIntro ty -> do
      ty' <- substType h sub ty
      return $ m :< WT.TauIntro ty'
    m :< WT.TauElim (mx, x) e1 e2 -> do
      e1' <- subst h sub e1
      x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
      let sub' = IntMap.insert (Ident.toInt x) (Var x') sub
      e2' <- subst h sub' e2
      return $ m :< WT.TauElim (mx, x') e1' e2'
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

substType :: Handle -> Subst -> WT.WeakType -> IO WT.WeakType
substType h sub ty =
  case ty of
    _ :< WT.Tau ->
      return ty
    m :< WT.TVar x
      | Just entry <- IntMap.lookup (Ident.toInt x) sub ->
          case entry of
            Var x' ->
              return $ m :< WT.TVar x'
            Term (_ :< WT.Var x') ->
              return $ m :< WT.TVar x'
            Term _ ->
              return ty
            Type t ->
              return t
      | otherwise ->
          return ty
    _ :< WT.TVarGlobal {} ->
      return ty
    m :< WT.TyApp t args -> do
      t' <- substType h sub t
      args' <- mapM (substType h sub) args
      return $ m :< WT.TyApp t' args'
    m :< WT.Pi piKind impArgs expArgs defaultArgs t -> do
      (impArgs', sub') <- subst' h sub impArgs
      (expArgs', sub'') <- subst' h sub' expArgs
      (defaultArgs', sub''') <- substDefaultArgs h sub'' defaultArgs
      t' <- substType h sub''' t
      return $ m :< WT.Pi piKind impArgs' expArgs' defaultArgs' t'
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
    m :< WT.Resource dd resourceID unitType discarder copier -> do
      unitType' <- substType h sub unitType
      discarder' <- subst h sub discarder
      copier' <- subst h sub copier
      return $ m :< WT.Resource dd resourceID unitType' discarder' copier'
    m :< WT.TypeHole holeID es -> do
      es' <- mapM (substType h sub) es
      return $ m :< WT.TypeHole holeID es'

substBinder ::
  Handle ->
  Subst ->
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
      let sub' = IntMap.insert (Ident.toInt x) (Var x') sub
      (xts', e') <- substBinder h sub' xts e
      return ((m, x', t') : xts', e')

subst' ::
  Handle ->
  Subst ->
  [BinderF WT.WeakType] ->
  IO ([BinderF WT.WeakType], Subst)
subst' h sub binder =
  case binder of
    [] -> do
      return ([], sub)
    ((m, x, t) : xts) -> do
      t' <- substType h sub t
      x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
      let sub' = IntMap.insert (Ident.toInt x) (Var x') sub
      (xts', sub'') <- subst' h sub' xts
      return ((m, x', t') : xts', sub'')

substDefaultArgs ::
  Handle ->
  Subst ->
  [(BinderF WT.WeakType, WT.WeakTerm)] ->
  IO ([(BinderF WT.WeakType, WT.WeakTerm)], Subst)
substDefaultArgs h sub binderList =
  case binderList of
    [] -> do
      return ([], sub)
    ((m, x, t), defaultValue) : rest -> do
      t' <- substType h sub t
      defaultValue' <- subst h sub defaultValue
      x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
      let sub' = IntMap.insert (Ident.toInt x) (Var x') sub
      (rest', sub'') <- substDefaultArgs h sub' rest
      return (((m, x', t'), defaultValue') : rest', sub'')

subst'' ::
  Handle ->
  Subst ->
  BinderF WT.WeakType ->
  [BinderF WT.WeakType] ->
  WT.WeakTerm ->
  IO (BinderF WT.WeakType, [BinderF WT.WeakType], WT.WeakTerm)
subst'' h sub (m, x, t) binder e = do
  t' <- substType h sub t
  x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
  let sub' = IntMap.insert (Ident.toInt x) (Var x') sub
  (xts', e') <- substBinder h sub' binder e
  return ((m, x', t'), xts', e')

subst''' ::
  Handle ->
  Subst ->
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
      let sub' = IntMap.insert (Ident.toInt x) (Var x') sub
      (xts', e') <- subst''' h sub' xts decisionTree
      return ((m, x', t') : xts', e')

substLet ::
  Handle ->
  Subst ->
  (BinderF WT.WeakType, WT.WeakTerm) ->
  IO ((BinderF WT.WeakType, WT.WeakTerm), Subst)
substLet h sub ((m, x, t), e) = do
  e' <- subst h sub e
  t' <- substType h sub t
  x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
  let sub' = IntMap.insert (Ident.toInt x) (Var x') sub
  return (((m, x', t'), e'), sub')

substLetSeq ::
  Handle ->
  Subst ->
  [(BinderF WT.WeakType, WT.WeakTerm)] ->
  IO ([(BinderF WT.WeakType, WT.WeakTerm)], Subst)
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
  Subst ->
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
  Subst ->
  DT.CaseList WT.WeakType WT.WeakTerm ->
  IO (DT.CaseList WT.WeakType WT.WeakTerm)
substCaseList h sub (fallbackClause, clauseList) = do
  fallbackClause' <- substDecisionTree h sub fallbackClause
  clauseList' <- mapM (substCase h sub) clauseList
  return (fallbackClause', clauseList')

substCase ::
  Handle ->
  Subst ->
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

substLeafVar :: Subst -> Ident -> Maybe Ident
substLeafVar sub leafVar =
  case IntMap.lookup (Ident.toInt leafVar) sub of
    Just (Var leafVar') ->
      return leafVar'
    Just (Term _) ->
      Nothing
    Just (Type _) ->
      return leafVar
    Nothing ->
      return leafVar

substVar :: Subst -> Ident -> Ident
substVar sub x =
  case IntMap.lookup (Ident.toInt x) sub of
    Just (Var x') ->
      x'
    _ ->
      x

substAttrData :: Handle -> Subst -> AttrD.Attr name (BinderF WT.WeakType) -> IO (AttrD.Attr name (BinderF WT.WeakType))
substAttrData h sub attr = do
  let consNameList = AttrD.consNameList attr
  consNameList' <-
    mapM
      ( \(cn, binders, cl) -> do
          binders' <-
            mapM
              ( \(mx, x, t) -> do
                  t' <- substType h sub t
                  return (mx, x, t')
              )
              binders
          return (cn, binders', cl)
      )
      consNameList
  return $ attr {AttrD.consNameList = consNameList'}

substAttrDataIntro :: Handle -> Subst -> AttrDI.Attr name (BinderF WT.WeakType) -> IO (AttrDI.Attr name (BinderF WT.WeakType))
substAttrDataIntro h sub attr = do
  let consNameList = AttrDI.consNameList attr
  consNameList' <-
    mapM
      ( \(cn, binders, cl) -> do
          binders' <-
            mapM
              ( \(mx, x, t) -> do
                  t' <- substType h sub t
                  return (mx, x, t')
              )
              binders
          return (cn, binders', cl)
      )
      consNameList
  return $ attr {AttrDI.consNameList = consNameList'}

substMagic :: Handle -> Subst -> WT.WeakMagic WT.WeakType WT.WeakType WT.WeakTerm -> IO (WT.WeakMagic WT.WeakType WT.WeakType WT.WeakTerm)
substMagic h sub (WT.WeakMagic magic) = do
  magic' <- case magic of
    M.LowMagic lowMagic -> do
      lowMagic' <- substLowMagic h sub lowMagic
      return $ M.LowMagic lowMagic'
    M.GetTypeTag sgl typeTagExpr e -> do
      typeTagExpr' <- substType h sub typeTagExpr
      e' <- substType h sub e
      return $ M.GetTypeTag sgl typeTagExpr' e'
    M.GetDataArgs sgl listExpr typeExpr -> do
      listExpr' <- substType h sub listExpr
      typeExpr' <- substType h sub typeExpr
      return $ M.GetDataArgs sgl listExpr' typeExpr'
    M.GetConsSize typeExpr -> do
      typeExpr' <- substType h sub typeExpr
      return $ M.GetConsSize typeExpr'
    M.GetWrapperContentType typeExpr -> do
      typeExpr' <- substType h sub typeExpr
      return $ M.GetWrapperContentType typeExpr'
    M.GetVectorContentType sgl typeExpr -> do
      typeExpr' <- substType h sub typeExpr
      return $ M.GetVectorContentType sgl typeExpr'
    M.GetNoemaContentType typeExpr -> do
      typeExpr' <- substType h sub typeExpr
      return $ M.GetNoemaContentType typeExpr'
    M.GetBoxContentType typeExpr -> do
      typeExpr' <- substType h sub typeExpr
      return $ M.GetBoxContentType typeExpr'
    M.GetConstructorArgTypes sgl listExpr typeExpr index -> do
      listExpr' <- substType h sub listExpr
      typeExpr' <- substType h sub typeExpr
      index' <- subst h sub index
      return $ M.GetConstructorArgTypes sgl listExpr' typeExpr' index'
    M.GetConsName textType typeExpr index -> do
      textType' <- substType h sub textType
      typeExpr' <- substType h sub typeExpr
      index' <- subst h sub index
      return $ M.GetConsName textType' typeExpr' index'
    M.GetConsConstFlag boolType typeExpr index -> do
      boolType' <- substType h sub boolType
      typeExpr' <- substType h sub typeExpr
      index' <- subst h sub index
      return $ M.GetConsConstFlag boolType' typeExpr' index'
    M.ShowType textTypeExpr typeExpr -> do
      textTypeExpr' <- substType h sub textTypeExpr
      typeExpr' <- substType h sub typeExpr
      return $ M.ShowType textTypeExpr' typeExpr'
    M.TextCons textTypeExpr rune text -> do
      textTypeExpr' <- substType h sub textTypeExpr
      rune' <- subst h sub rune
      text' <- subst h sub text
      return $ M.TextCons textTypeExpr' rune' text'
    M.TextUncons mid text -> do
      text' <- subst h sub text
      return $ M.TextUncons mid text'
    M.CompileError typeExpr msg -> do
      typeExpr' <- substType h sub typeExpr
      msg' <- subst h sub msg
      return $ M.CompileError typeExpr' msg'
  return $ WT.WeakMagic magic'

substLowMagic :: Handle -> Subst -> LM.LowMagic WT.WeakType WT.WeakType WT.WeakTerm -> IO (LM.LowMagic WT.WeakType WT.WeakType WT.WeakTerm)
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
      varArgs' <-
        mapM
          ( \(a, t) -> do
              a' <- subst h sub a
              t' <- substType h sub t
              return (a', t')
          )
          varArgs
      return $ LM.External domList' cod' extFunName args' varArgs'
    LM.Global name t -> do
      t' <- substType h sub t
      return $ LM.Global name t'
    LM.OpaqueValue e -> do
      e' <- subst h sub e
      return $ LM.OpaqueValue e'
    LM.CallType func arg1 arg2 -> do
      func' <- subst h sub func
      arg1' <- subst h sub arg1
      arg2' <- subst h sub arg2
      return $ LM.CallType func' arg1' arg2'
