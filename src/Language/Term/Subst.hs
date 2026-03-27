module Language.Term.Subst
  ( Handle,
    new,
    SubstEntry (..),
    Subst,
    subst,
    substType,
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
import Language.Common.PiElimKind qualified as PEK
import Language.Common.Magic qualified as M
import Language.Common.VarKind qualified as VK
import Language.Term.FreeVars qualified as TM
import Language.Term.Term qualified as TM

data SubstEntry
  = Var Ident
  | Term TM.Term
  | Type TM.Type

type Subst =
  IntMap.IntMap SubstEntry

newtype Handle = Handle
  { gensymHandle :: Gensym.Handle
  }

new :: Gensym.Handle -> Handle
new gensymHandle = do
  Handle {..}

subst :: Handle -> Subst -> TM.Term -> IO TM.Term
subst h sub term =
  case term of
    m :< TM.Var x
      | Just entry <- IntMap.lookup (Ident.toInt x) sub ->
          case entry of
            Var x' ->
              return $ m :< TM.Var x'
            Term e ->
              return e
            Type (_ :< TM.TVar x') ->
              return $ m :< TM.Var x'
            Type _ ->
              return term
      | otherwise ->
          return term
    _ :< TM.VarGlobal {} ->
      return term
    m :< TM.PiIntro (AttrL.Attr {lamKind}) impArgs expArgs defaultArgs e -> do
      let fvs = S.map Ident.toInt $ TM.freeVars term
      let subDomSet = S.fromList $ IntMap.keys sub
      if S.intersection fvs subDomSet == S.empty
        then return term
        else do
          newLamID <- liftIO $ Gensym.newCount (gensymHandle h)
          case lamKind of
            LK.Fix opacity isDestPassing xt -> do
              (impArgs', sub') <- substBinder h sub impArgs
              (expArgs', sub'') <- substBinder h sub' expArgs
              (defaultArgs', sub''') <- substDefaultArgs h sub'' defaultArgs
              ([xt'], sub'''') <- substBinder h sub''' [xt]
              e' <- subst h sub'''' e
              let fixAttr = AttrL.Attr {lamKind = LK.Fix opacity isDestPassing xt', identity = newLamID}
              return (m :< TM.PiIntro fixAttr impArgs' expArgs' defaultArgs' e')
            LK.Normal name isDestPassing codType -> do
              (impArgs', sub') <- substBinder h sub impArgs
              (expArgs', sub'') <- substBinder h sub' expArgs
              (defaultArgs', sub''') <- substDefaultArgs h sub'' defaultArgs
              codType' <- substType h sub''' codType
              e' <- subst h sub''' e
              let lamAttr = AttrL.Attr {lamKind = LK.Normal name isDestPassing codType', identity = newLamID}
              return (m :< TM.PiIntro lamAttr impArgs' expArgs' defaultArgs' e')
    m :< TM.PiElim b e impArgs expArgs defaultArgs -> do
      b' <- PEK.traverseArg (substType h sub) b
      e' <- subst h sub e
      impArgs' <- mapM (substType h sub) impArgs
      expArgs' <- mapM (subst h sub) expArgs
      defaultArgs' <- mapM (traverse (subst h sub)) defaultArgs
      return (m :< TM.PiElim b' e' impArgs' expArgs' defaultArgs')
    m :< TM.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (substType h sub) dataArgs
      consArgs' <- mapM (subst h sub) consArgs
      attr' <- substAttrDataIntro h sub attr
      return $ m :< TM.DataIntro attr' consName dataArgs' consArgs'
    m :< TM.DataElim isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (subst h sub) es
      let binder = zipWith (\o t -> (m, VK.Normal, o, t)) os ts
      (binder', decisionTree') <- subst'' h sub binder decisionTree
      let os' = map (\(_, _, o, _) -> o) binder'
      let ts' = map (\(_, _, _, t) -> t) binder'
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
    m :< TM.TauIntro ty -> do
      ty' <- substType h sub ty
      return $ m :< TM.TauIntro ty'
    m :< TM.TauElim (mx, x) e1 e2 -> do
      e1' <- subst h sub e1
      x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
      let sub' = IntMap.insert (Ident.toInt x) (Var x') sub
      e2' <- subst h sub' e2
      return $ m :< TM.TauElim (mx, x') e1' e2'
    m :< TM.Let opacity mxt e1 e2 -> do
      e1' <- subst h sub e1
      ([mxt'], e2') <- subst' h sub [mxt] e2
      return $ m :< TM.Let opacity mxt' e1' e2'
    _ :< TM.Prim _ ->
      return term
    m :< TM.Magic der -> do
      der' <- substMagic h sub der
      return (m :< TM.Magic der')

substType :: Handle -> Subst -> TM.Type -> IO TM.Type
substType h sub ty =
  case ty of
    _ :< TM.Tau ->
      return ty
    m :< TM.TVar x
      | Just entry <- IntMap.lookup (Ident.toInt x) sub ->
          case entry of
            Var x' ->
              return $ m :< TM.TVar x'
            Term (_ :< TM.Var x') ->
              return $ m :< TM.TVar x'
            Term _ ->
              return ty
            Type t ->
              return t
      | otherwise ->
          return ty
    _ :< TM.TVarGlobal {} ->
      return ty
    m :< TM.TyApp t args -> do
      t' <- substType h sub t
      args' <- mapM (substType h sub) args
      return $ m :< TM.TyApp t' args'
    m :< TM.Pi piKind impArgs expArgs defaultArgs t -> do
      (impArgs', sub') <- substBinder h sub impArgs
      (expArgs', sub'') <- substBinder h sub' expArgs
      (defaultArgs', sub''') <- substBinder h sub'' defaultArgs
      t' <- substType h sub''' t
      return (m :< TM.Pi piKind impArgs' expArgs' defaultArgs' t')
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
    m :< TM.Resource dd resourceID -> do
      return $ m :< TM.Resource dd resourceID

substBinder ::
  Handle ->
  Subst ->
  [BinderF TM.Type] ->
  IO ([BinderF TM.Type], Subst)
substBinder h sub binder =
  case binder of
    [] -> do
      return ([], sub)
    ((m, k, x, t) : xts) -> do
      t' <- substType h sub t
      x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
      let sub' = IntMap.insert (Ident.toInt x) (Var x') sub
      (xts', sub'') <- substBinder h sub' xts
      return ((m, k, x', t') : xts', sub'')

substDefaultArgs ::
  Handle ->
  Subst ->
  [(BinderF TM.Type, TM.Term)] ->
  IO ([(BinderF TM.Type, TM.Term)], Subst)
substDefaultArgs h sub binderList =
  case binderList of
    [] -> do
      return ([], sub)
    ((m, k, x, t), defaultValue) : xts -> do
      t' <- substType h sub t
      defaultValue' <- subst h sub defaultValue
      x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
      let sub' = IntMap.insert (Ident.toInt x) (Var x') sub
      (xts', sub'') <- substDefaultArgs h sub' xts
      return (((m, k, x', t'), defaultValue') : xts', sub'')

subst' ::
  Handle ->
  Subst ->
  [BinderF TM.Type] ->
  TM.Term ->
  IO ([BinderF TM.Type], TM.Term)
subst' h sub binder e =
  case binder of
    [] -> do
      e' <- subst h sub e
      return ([], e')
    ((m, k, x, t) : xts) -> do
      t' <- substType h sub t
      x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
      let sub' = IntMap.insert (Ident.toInt x) (Var x') sub
      (xts', e') <- subst' h sub' xts e
      return ((m, k, x', t') : xts', e')

subst'' ::
  Handle ->
  Subst ->
  [BinderF TM.Type] ->
  DT.DecisionTree TM.Type TM.Term ->
  IO ([BinderF TM.Type], DT.DecisionTree TM.Type TM.Term)
subst'' h sub binder decisionTree =
  case binder of
    [] -> do
      decisionTree' <- substDecisionTree h sub decisionTree
      return ([], decisionTree')
    ((m, k, x, t) : xts) -> do
      t' <- substType h sub t
      x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
      let sub' = IntMap.insert (Ident.toInt x) (Var x') sub
      (xts', e') <- subst'' h sub' xts decisionTree
      return ((m, k, x', t') : xts', e')

substLet ::
  Handle ->
  Subst ->
  (BinderF TM.Type, TM.Term) ->
  IO ((BinderF TM.Type, TM.Term), Subst)
substLet h sub ((m, k, x, t), e) = do
  e' <- subst h sub e
  t' <- substType h sub t
  x' <- liftIO $ Gensym.newIdentFromIdent (gensymHandle h) x
  let sub' = IntMap.insert (Ident.toInt x) (Var x') sub
  return (((m, k, x', t'), e'), sub')

substLetSeq ::
  Handle ->
  Subst ->
  [(BinderF TM.Type, TM.Term)] ->
  IO ([(BinderF TM.Type, TM.Term)], Subst)
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
  Subst ->
  DT.CaseList TM.Type TM.Term ->
  IO (DT.CaseList TM.Type TM.Term)
substCaseList h sub (fallbackClause, clauseList) = do
  fallbackClause' <- substDecisionTree h sub fallbackClause
  clauseList' <- mapM (substCase h sub) clauseList
  return (fallbackClause', clauseList')

substCase ::
  Handle ->
  Subst ->
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

substAttrData :: Handle -> Subst -> AttrD.Attr name (BinderF TM.Type) -> IO (AttrD.Attr name (BinderF TM.Type))
substAttrData h sub attr = do
  let consNameList = AttrD.consNameList attr
  consNameList' <-
    mapM
      ( \(cn, binders, cl) -> do
          binders' <-
            mapM
              ( \(mx, k, x, t) -> do
                  t' <- substType h sub t
                  return (mx, k, x, t')
              )
              binders
          return (cn, binders', cl)
      )
      consNameList
  return $ attr {AttrD.consNameList = consNameList'}

substAttrDataIntro :: Handle -> Subst -> AttrDI.Attr name (BinderF TM.Type) -> IO (AttrDI.Attr name (BinderF TM.Type))
substAttrDataIntro h sub attr = do
  let consNameList = AttrDI.consNameList attr
  consNameList' <-
    mapM
      ( \(cn, binders, cl) -> do
          binders' <-
            mapM
              ( \(mx, k, x, t) -> do
                  t' <- substType h sub t
                  return (mx, k, x, t')
              )
              binders
          return (cn, binders', cl)
      )
      consNameList
  return $ attr {AttrDI.consNameList = consNameList'}

substMagic :: Handle -> Subst -> M.Magic BLT.BaseLowType TM.Type TM.Term -> IO (M.Magic BLT.BaseLowType TM.Type TM.Term)
substMagic h sub magic =
  case magic of
    M.LowMagic lowMagic ->
      M.LowMagic <$> substLowMagic h sub lowMagic
    M.Malloc size -> do
      size' <- subst h sub size
      return $ M.Malloc size'
    M.Realloc ptr size -> do
      ptr' <- subst h sub ptr
      size' <- subst h sub size
      return $ M.Realloc ptr' size'
    M.Free unitType ptr -> do
      unitType' <- substType h sub unitType
      ptr' <- subst h sub ptr
      return $ M.Free unitType' ptr'
    M.InspectType mid typeValueExpr e -> do
      typeValueExpr' <- substType h sub typeValueExpr
      e' <- substType h sub e
      return $ M.InspectType mid typeValueExpr' e'
    M.EqType moduleID typeExpr1 typeExpr2 -> do
      typeExpr1' <- substType h sub typeExpr1
      typeExpr2' <- substType h sub typeExpr2
      return $ M.EqType moduleID typeExpr1' typeExpr2'
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

substLowMagic :: Handle -> Subst -> LM.LowMagic BLT.BaseLowType TM.Type TM.Term -> IO (LM.LowMagic BLT.BaseLowType TM.Type TM.Term)
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
      varArgs' <-
        mapM
          ( \(arg, typ) -> do
              arg' <- subst h sub arg
              return (arg', typ)
          )
          varArgs
      return $ LM.External domList cod extFunName args' varArgs'
    LM.Global name t ->
      return $ LM.Global name t
    LM.OpaqueValue e ->
      LM.OpaqueValue <$> subst h sub e
    LM.CallType func arg1 arg2 -> do
      func' <- subst h sub func
      arg1' <- subst h sub arg1
      arg2' <- subst h sub arg2
      return $ LM.CallType func' arg1' arg2'
