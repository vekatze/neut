module Language.Term.Subst
  ( Handle,
    new,
    SubstEntry (..),
    Subst,
    subst,
    substAndRefresh,
    substAndRefreshWithTrace,
    substType,
    refresh,
    refreshType,
    refreshBinders,
    subst',
    substAndRefresh',
    substAndRefreshWithTrace',
    subst'',
    substDecisionTree,
  )
where

import Control.Comonad.Cofree
import Data.IntMap qualified as IntMap
import Gensym.Gensym qualified as Gensym
import Gensym.Handle qualified as Gensym
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.BaseLowType qualified as BLT
import Language.Common.Binder
import Language.Common.CallConv qualified as CC
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.DecisionTree qualified as DT
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.LamKind qualified as LK
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.Common.VarKind qualified as VK
import Language.Term.Term qualified as TM
import Language.Term.Trace qualified as Trace
import Language.Term.TraceSites qualified as TraceSites
import Logger.Hint (Hint)

data SubstEntry
  = Var Ident
  | Term TM.Term
  | Type TM.Type

type Subst =
  IntMap.IntMap SubstEntry

data Handle = Handle
  { gensymHandle :: Gensym.Handle,
    shouldRefreshBinders :: Bool,
    traceMapper :: Maybe Trace.Remapping
  }

new :: Gensym.Handle -> Handle
new gensymHandle = do
  let shouldRefreshBinders = False
  let traceMapper = Nothing
  Handle {..}

newtype Level
  = Level Int
  deriving (Eq, Ord)

rootLevel :: Level
rootLevel =
  Level 0

nextLevel :: Level -> Level
nextLevel (Level level) =
  Level (level + 1)

noRenamedLevel :: Level
noRenamedLevel =
  Level maxBound

data Env = Env
  { entries :: Subst,
    renaming :: IntMap.IntMap (Ident, Level),
    depth :: Level
  }

initialEnv :: Subst -> Env
initialEnv sub =
  Env {entries = sub, renaming = IntMap.empty, depth = rootLevel}

data Change
  = Unchanged
  | Changed !Bool !Level

instance Semigroup Change where
  Unchanged <> change =
    change
  change <> Unchanged =
    change
  Changed isAltered1 renamedLevel1 <> Changed isAltered2 renamedLevel2 =
    Changed (isAltered1 || isAltered2) (min renamedLevel1 renamedLevel2)

instance Monoid Change where
  mempty =
    Unchanged

data Outcome a
  = Kept
  | Made a !Bool !Level

changeOf :: Outcome a -> Change
changeOf outcome =
  case outcome of
    Kept ->
      Unchanged
    Made _ isAltered renamedLevel ->
      Changed isAltered renamedLevel

changes :: [Outcome a] -> Change
changes =
  foldMap changeOf

valueOr :: a -> Outcome a -> a
valueOr original outcome =
  case outcome of
    Kept ->
      original
    Made x _ _ ->
      x

values :: [a] -> [Outcome a] -> [a]
values =
  zipWith valueOr

altered :: a -> Outcome a
altered x =
  Made x True noRenamedLevel

release :: Env -> Change -> Change
release env change =
  case change of
    Changed isAltered renamedLevel
      | renamedLevel >= depth env ->
          Changed isAltered noRenamedLevel
    _ ->
      change

{-# INLINE build #-}
build :: Change -> a -> Outcome a
build change x =
  case change of
    Changed isAltered renamedLevel
      | isAltered || renamedLevel /= noRenamedLevel ->
          Made x isAltered renamedLevel
    _ ->
      Kept

{-# INLINE buildTraced #-}
buildTraced :: Handle -> Hint -> Change -> TM.TermF TM.Term -> Outcome TM.Term
buildTraced h m change node =
  case traceMapper h of
    Nothing ->
      build change (m :< node)
    Just remapping -> do
      let node' = TraceSites.setTraceID (Trace.remapOrKeep remapping $ TraceSites.traceIDOf node) node
      case change of
        Changed _ renamedLevel ->
          Made (m :< node') True renamedLevel
        Unchanged ->
          Made (m :< node') True noRenamedLevel

subst :: Handle -> Subst -> TM.Term -> IO TM.Term
subst h sub term =
  valueOr term <$> substTerm h (initialEnv sub) term

substType :: Handle -> Subst -> TM.Type -> IO TM.Type
substType h sub ty =
  valueOr ty <$> substTy h (initialEnv sub) ty

refresh :: Handle -> TM.Term -> IO TM.Term
refresh h =
  subst (h {shouldRefreshBinders = True}) IntMap.empty

refreshType :: Handle -> TM.Type -> IO TM.Type
refreshType h =
  substType (h {shouldRefreshBinders = True}) IntMap.empty

substAndRefresh :: Handle -> Subst -> TM.Term -> IO TM.Term
substAndRefresh h =
  subst (h {shouldRefreshBinders = True})

substAndRefreshWithTrace :: Handle -> Trace.Remapping -> Subst -> TM.Term -> IO TM.Term
substAndRefreshWithTrace h remapping =
  subst (h {shouldRefreshBinders = True, traceMapper = Just remapping})

subst' ::
  Handle ->
  Subst ->
  [BinderF TM.Type] ->
  TM.Term ->
  IO ([BinderF TM.Type], TM.Term)
subst' h sub binders e = do
  (binders', env) <- substBinders h (initialEnv sub) binders
  e' <- substTerm h env e
  return (values binders binders', valueOr e e')

substAndRefresh' :: Handle -> Subst -> [BinderF TM.Type] -> TM.Term -> IO ([BinderF TM.Type], TM.Term)
substAndRefresh' h =
  subst' (h {shouldRefreshBinders = True})

substAndRefreshWithTrace' :: Handle -> Trace.Remapping -> Subst -> [BinderF TM.Type] -> TM.Term -> IO ([BinderF TM.Type], TM.Term)
substAndRefreshWithTrace' h remapping =
  subst' (h {shouldRefreshBinders = True, traceMapper = Just remapping})

subst'' ::
  Handle ->
  Subst ->
  [BinderF TM.Type] ->
  DT.DecisionTree TM.Type TM.Term ->
  IO ([BinderF TM.Type], DT.DecisionTree TM.Type TM.Term)
subst'' h sub binders tree = do
  (binders', env) <- substBinders h (initialEnv sub) binders
  tree' <- substTree h env tree
  return (values binders binders', valueOr tree tree')

substDecisionTree ::
  Handle ->
  Subst ->
  DT.DecisionTree TM.Type TM.Term ->
  IO (DT.DecisionTree TM.Type TM.Term)
substDecisionTree h sub tree =
  valueOr tree <$> substTree h (initialEnv sub) tree

refreshBinders :: Handle -> Subst -> [BinderF TM.Type] -> IO ([BinderF TM.Type], Subst)
refreshBinders h sub binders = do
  (binders', env) <- substBinders (h {shouldRefreshBinders = True}) (initialEnv sub) binders
  let renamings = IntMap.map (\(x', _) -> Var x') (renaming env)
  return (values binders binders', IntMap.union renamings (entries env))

renamedVariable :: Handle -> Env -> Ident -> Maybe (Outcome Ident)
renamedVariable h env x = do
  (x', level) <- IntMap.lookup (Ident.toInt x) (renaming env)
  return $ Made x' (shouldRefreshBinders h) level

bindIdent :: Handle -> Env -> Ident -> IO (Outcome Ident, Env)
bindIdent h env x = do
  x' <- Gensym.newIdentFromIdent (gensymHandle h) x
  let key = Ident.toInt x
  let level = depth env
  let env' =
        env
          { entries = IntMap.delete key (entries env),
            renaming = IntMap.insert key (x', level) (renaming env),
            depth = nextLevel level
          }
  return (Made x' (shouldRefreshBinders h) level, env')

substBinder :: Handle -> Env -> BinderF TM.Type -> IO (Outcome (BinderF TM.Type), Env)
substBinder h env (m, k, x, t) = do
  t' <- substTy h env t
  (x', env') <- bindIdent h env x
  return (build (changeOf x' <> changeOf t') (m, k, valueOr x x', valueOr t t'), env')

substBinders :: Handle -> Env -> [BinderF TM.Type] -> IO ([Outcome (BinderF TM.Type)], Env)
substBinders h env binders =
  case binders of
    [] ->
      return ([], env)
    binder : rest -> do
      (binder', env') <- substBinder h env binder
      (rest', env'') <- substBinders h env' rest
      return (binder' : rest', env'')

substLetSeq ::
  Handle ->
  Env ->
  [(BinderF TM.Type, TM.Term)] ->
  IO (Outcome [(BinderF TM.Type, TM.Term)], Env)
substLetSeq h env letSeq =
  case letSeq of
    [] ->
      return (Kept, env)
    (binder, e) : rest -> do
      e' <- substTerm h env e
      (binder', env') <- substBinder h env binder
      (rest', env'') <- substLetSeq h env' rest
      let change = changeOf binder' <> changeOf e' <> changeOf rest'
      return (build change ((valueOr binder binder', valueOr e e') : valueOr rest rest'), env'')

substTerm :: Handle -> Env -> TM.Term -> IO (Outcome TM.Term)
substTerm h env term =
  case term of
    m :< TM.Var x ->
      case IntMap.lookup (Ident.toInt x) (entries env) of
        Just (Var x') ->
          return $ altered $ m :< TM.Var x'
        Just (Term e) ->
          return $ altered e
        Just (Type (_ :< TM.TVar x')) ->
          return $ altered $ m :< TM.Var x'
        Just (Type _) ->
          return Kept
        Nothing ->
          case renamedVariable h env x of
            Just x' ->
              return $ build (changeOf x') $ m :< TM.Var (valueOr x x')
            Nothing ->
              return Kept
    _ :< TM.VarGlobal {} ->
      return Kept
    m :< TM.PiIntro (AttrL.Attr {lamKind}) impArgs expArgs defaultArgs e -> do
      newLamID <- Gensym.newCount (gensymHandle h)
      (impArgs', env1) <- substBinders h env impArgs
      (expArgs', env2) <- substBinders h env1 expArgs
      (defaultArgs', env3) <- substLetSeq h env2 defaultArgs
      let change = changes impArgs' <> changes expArgs' <> changeOf defaultArgs'
      let lam attr body = m :< TM.PiIntro attr (values impArgs impArgs') (values expArgs expArgs') (valueOr defaultArgs defaultArgs') body
      case lamKind of
        LK.Fix kind isDestPassing xt -> do
          (xt', env4) <- substBinder h env3 xt
          e' <- substTerm h env4 e
          let change' = release env $ change <> changeOf xt' <> changeOf e'
          let attr = AttrL.Attr {lamKind = LK.Fix kind isDestPassing (valueOr xt xt'), identity = newLamID}
          return $ build change' $ lam attr (valueOr e e')
        LK.Normal name isDestPassing codType -> do
          codType' <- substTy h env3 codType
          e' <- substTerm h env3 e
          let change' = release env $ change <> changeOf codType' <> changeOf e'
          let attr = AttrL.Attr {lamKind = LK.Normal name isDestPassing (valueOr codType codType'), identity = newLamID}
          return $ build change' $ lam attr (valueOr e e')
    m :< TM.PiElim traceID b e impArgs expArgs defaultArgs -> do
      b' <- CC.traverseTypes (\t -> (,) t <$> substTy h env t) b
      e' <- substTerm h env e
      impArgs' <- mapM (substTy h env) impArgs
      expArgs' <- mapM (substTerm h env) expArgs
      defaultArgs' <- mapM (traverse (\d -> (,) d <$> substTerm h env d)) defaultArgs
      let change = foldMap (changeOf . snd) b' <> changeOf e' <> changes impArgs' <> changes expArgs' <> foldMap (foldMap (changeOf . snd)) defaultArgs'
      let node = TM.PiElim traceID (fmap (uncurry valueOr) b') (valueOr e e') (values impArgs impArgs') (values expArgs expArgs') (map (fmap (uncurry valueOr)) defaultArgs')
      return $ buildTraced h m change node
    m :< TM.DataIntro attr consName dataArgs consImpArgs consArgs -> do
      dataArgs' <- mapM (substTy h env) dataArgs
      consImpArgs' <- mapM (substTy h env) consImpArgs
      consArgs' <- mapM (substTerm h env) consArgs
      let change = changes dataArgs' <> changes consImpArgs' <> changes consArgs'
      return $ build change $ m :< TM.DataIntro attr consName (values dataArgs dataArgs') (values consImpArgs consImpArgs') (values consArgs consArgs')
    m :< TM.DataElim traceID isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (substTerm h env) es
      let binders = zipWith (\o t -> (m, VK.normal, o, t)) os ts
      (binders', env') <- substBinders h env binders
      decisionTree' <- substTree h env' decisionTree
      let change = release env $ changes es' <> changes binders' <> changeOf decisionTree'
      let binders'' = values binders binders'
      let os' = map (\(_, _, o, _) -> o) binders''
      let ts' = map (\(_, _, _, t) -> t) binders''
      return $ buildTraced h m change $ TM.DataElim traceID isNoetic (zip3 os' (values es es') ts') (valueOr decisionTree decisionTree')
    m :< TM.BoxIntro traceID letSeq e -> do
      (letSeq', env') <- substLetSeq h env letSeq
      e' <- substTerm h env' e
      let change = release env $ changeOf letSeq' <> changeOf e'
      return $ buildTraced h m change $ TM.BoxIntro traceID (valueOr letSeq letSeq') (valueOr e e')
    m :< TM.BoxIntroLift t e -> do
      t' <- substTy h env t
      e' <- substTerm h env e
      return $ build (changeOf t' <> changeOf e') $ m :< TM.BoxIntroLift (valueOr t t') (valueOr e e')
    m :< TM.EmbedIntro e -> do
      e' <- substTerm h env e
      return $ build (changeOf e') $ m :< TM.EmbedIntro (valueOr e e')
    m :< TM.BoxElim traceID castSeq mxt e1 uncastSeq e2 -> do
      (castSeq', env1) <- substLetSeq h env castSeq
      e1' <- substTerm h env1 e1
      (mxt', env2) <- substBinder h env1 mxt
      (uncastSeq', env3) <- substLetSeq h env2 uncastSeq
      e2' <- substTerm h env3 e2
      let change = release env $ changeOf castSeq' <> changeOf e1' <> changeOf mxt' <> changeOf uncastSeq' <> changeOf e2'
      return $ buildTraced h m change $ TM.BoxElim traceID (valueOr castSeq castSeq') (valueOr mxt mxt') (valueOr e1 e1') (valueOr uncastSeq uncastSeq') (valueOr e2 e2')
    m :< TM.CodeIntro e -> do
      e' <- substTerm h env e
      return $ build (changeOf e') $ m :< TM.CodeIntro (valueOr e e')
    m :< TM.CodeElim traceID e -> do
      e' <- substTerm h env e
      return $ buildTraced h m (changeOf e') $ TM.CodeElim traceID (valueOr e e')
    m :< TM.TauIntro ty -> do
      ty' <- substTy h env ty
      return $ build (changeOf ty') $ m :< TM.TauIntro (valueOr ty ty')
    m :< TM.TauElim traceID (mx, k, x) e1 e2 -> do
      e1' <- substTerm h env e1
      (x', env') <- bindIdent h env x
      e2' <- substTerm h env' e2
      let change = release env $ changeOf x' <> changeOf e1' <> changeOf e2'
      return $ buildTraced h m change $ TM.TauElim traceID (mx, k, valueOr x x') (valueOr e1 e1') (valueOr e2 e2')
    m :< TM.Let mxt e1 e2 -> do
      e1' <- substTerm h env e1
      (mxt', env') <- substBinder h env mxt
      e2' <- substTerm h env' e2
      let change = release env $ changeOf mxt' <> changeOf e1' <> changeOf e2'
      return $ build change $ m :< TM.Let (valueOr mxt mxt') (valueOr e1 e1') (valueOr e2 e2')
    m :< TM.Invoke tropeNames body -> do
      body' <- substTerm h env body
      return $ build (changeOf body') $ m :< TM.Invoke tropeNames (valueOr body body')
    _ :< TM.Prim {} ->
      return Kept
    m :< TM.Magic traceID der -> do
      der' <- substMagic h env der
      return $ buildTraced h m (changeOf der') $ TM.Magic traceID (valueOr der der')

substTy :: Handle -> Env -> TM.Type -> IO (Outcome TM.Type)
substTy h env ty =
  case ty of
    _ :< TM.Tau ->
      return Kept
    m :< TM.TVar x ->
      case IntMap.lookup (Ident.toInt x) (entries env) of
        Just (Var x') ->
          return $ altered $ m :< TM.TVar x'
        Just (Term (_ :< TM.Var x')) ->
          return $ altered $ m :< TM.TVar x'
        Just (Term _) ->
          return Kept
        Just (Type t) ->
          return $ altered t
        Nothing ->
          case renamedVariable h env x of
            Just x' ->
              return $ build (changeOf x') $ m :< TM.TVar (valueOr x x')
            Nothing ->
              return Kept
    _ :< TM.TVarGlobal {} ->
      return Kept
    m :< TM.TyApp t args -> do
      t' <- substTy h env t
      args' <- mapM (substTy h env) args
      return $ build (changeOf t' <> changes args') $ m :< TM.TyApp (valueOr t t') (values args args')
    m :< TM.Pi piKind impArgs expArgs defaultArgs t -> do
      (impArgs', env1) <- substBinders h env impArgs
      (expArgs', env2) <- substBinders h env1 expArgs
      (defaultArgs', env3) <- substBinders h env2 defaultArgs
      t' <- substTy h env3 t
      let change = release env $ changes impArgs' <> changes expArgs' <> changes defaultArgs' <> changeOf t'
      return $ build change $ m :< TM.Pi piKind (values impArgs impArgs') (values expArgs expArgs') (values defaultArgs defaultArgs') (valueOr t t')
    m :< TM.Data attr name es -> do
      es' <- mapM (substTy h env) es
      return $ build (changes es') $ m :< TM.Data attr name (values es es')
    m :< TM.Box t -> do
      t' <- substTy h env t
      return $ build (changeOf t') $ m :< TM.Box (valueOr t t')
    m :< TM.BoxNoema t -> do
      t' <- substTy h env t
      return $ build (changeOf t') $ m :< TM.BoxNoema (valueOr t t')
    m :< TM.Embed t -> do
      t' <- substTy h env t
      return $ build (changeOf t') $ m :< TM.Embed (valueOr t t')
    m :< TM.Code t -> do
      t' <- substTy h env t
      return $ build (changeOf t') $ m :< TM.Code (valueOr t t')
    _ :< TM.PrimType {} ->
      return Kept
    _ :< TM.Void ->
      return Kept
    _ :< TM.Resource {} ->
      return Kept

substTree ::
  Handle ->
  Env ->
  DT.DecisionTree TM.Type TM.Term ->
  IO (Outcome (DT.DecisionTree TM.Type TM.Term))
substTree h env tree =
  case tree of
    DT.Leaf xs letSeq e -> do
      let xs' = map (substLeafVar h env) xs
      (letSeq', env') <- substLetSeq h env letSeq
      e' <- substTerm h env' e
      let change = release env $ changes xs' <> changeOf letSeq' <> changeOf e'
      let xs'' = concat $ zipWith (\x x' -> valueOr [x] x') xs xs'
      return $ build change $ DT.Leaf xs'' (valueOr letSeq letSeq') (valueOr e e')
    DT.Unreachable ->
      return Kept
    DT.Switch (cursorVar, cursor) caseList -> do
      let cursorVar' = substVar h env cursorVar
      cursor' <- substTy h env cursor
      caseList' <- substCaseList h env caseList
      let change = changeOf cursorVar' <> changeOf cursor' <> changeOf caseList'
      return $ build change $ DT.Switch (valueOr cursorVar cursorVar', valueOr cursor cursor') (valueOr caseList caseList')

substCaseList ::
  Handle ->
  Env ->
  DT.CaseList TM.Type TM.Term ->
  IO (Outcome (DT.CaseList TM.Type TM.Term))
substCaseList h env (fallbackClause, clauseList) = do
  fallbackClause' <- substTree h env fallbackClause
  clauseList' <- mapM (substCase h env) clauseList
  let change = changeOf fallbackClause' <> changes clauseList'
  return $ build change (valueOr fallbackClause fallbackClause', values clauseList clauseList')

substCase ::
  Handle ->
  Env ->
  DT.Case TM.Type TM.Term ->
  IO (Outcome (DT.Case TM.Type TM.Term))
substCase h env decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      cont' <- substTree h env cont
      return $ build (changeOf cont') $ DT.LiteralCase mPat i (valueOr cont cont')
    DT.ConsCase record@(DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      dataTerms' <- mapM (substTy h env) dataTerms
      dataTypes' <- mapM (substTy h env) dataTypes
      let binders = consImpArgs ++ consArgs
      (binders', env') <- substBinders h env binders
      cont' <- substTree h env' cont
      let change = release env $ changes dataTerms' <> changes dataTypes' <> changes binders' <> changeOf cont'
      let (consImpArgs', consArgs') = splitAt (length consImpArgs) (values binders binders')
      return $
        build change $
          DT.ConsCase
            record
              { DT.dataArgs = zip (values dataTerms dataTerms') (values dataTypes dataTypes'),
                DT.consImpArgs = consImpArgs',
                DT.consArgs = consArgs',
                DT.cont = valueOr cont cont'
              }

substLeafVar :: Handle -> Env -> Ident -> Outcome [Ident]
substLeafVar h env leafVar =
  case IntMap.lookup (Ident.toInt leafVar) (entries env) of
    Just (Var leafVar') ->
      altered [leafVar']
    Just (Term _) ->
      altered []
    Just (Type _) ->
      Kept
    Nothing ->
      case renamedVariable h env leafVar of
        Just leafVar' ->
          build (changeOf leafVar') [valueOr leafVar leafVar']
        Nothing ->
          Kept

substVar :: Handle -> Env -> Ident -> Outcome Ident
substVar h env x =
  case IntMap.lookup (Ident.toInt x) (entries env) of
    Just (Var x') ->
      altered x'
    _ ->
      case renamedVariable h env x of
        Just x' ->
          x'
        Nothing ->
          Kept

substMagic :: Handle -> Env -> M.Magic BLT.BaseLowType TM.Type TM.Term -> IO (Outcome (M.Magic BLT.BaseLowType TM.Type TM.Term))
substMagic h env magic =
  case magic of
    M.LowMagic lowMagic -> do
      lowMagic' <- substLowMagic h env lowMagic
      return $ build (changeOf lowMagic') $ M.LowMagic (valueOr lowMagic lowMagic')
    M.Calloc sizeType num size -> do
      sizeType' <- substTy h env sizeType
      num' <- substTerm h env num
      size' <- substTerm h env size
      return $ build (changeOf sizeType' <> changeOf num' <> changeOf size') $ M.Calloc (valueOr sizeType sizeType') (valueOr num num') (valueOr size size')
    M.Malloc sizeType size -> do
      sizeType' <- substTy h env sizeType
      size' <- substTerm h env size
      return $ build (changeOf sizeType' <> changeOf size') $ M.Malloc (valueOr sizeType sizeType') (valueOr size size')
    M.Realloc sizeType ptr size -> do
      sizeType' <- substTy h env sizeType
      ptr' <- substTerm h env ptr
      size' <- substTerm h env size
      return $ build (changeOf sizeType' <> changeOf ptr' <> changeOf size') $ M.Realloc (valueOr sizeType sizeType') (valueOr ptr ptr') (valueOr size size')
    M.Free unitType ptr -> do
      unitType' <- substTy h env unitType
      ptr' <- substTerm h env ptr
      return $ build (changeOf unitType' <> changeOf ptr') $ M.Free (valueOr unitType unitType') (valueOr ptr ptr')
    M.InspectType mid typeValueExpr e -> do
      typeValueExpr' <- substTy h env typeValueExpr
      e' <- substTy h env e
      return $ build (changeOf typeValueExpr' <> changeOf e') $ M.InspectType mid (valueOr typeValueExpr typeValueExpr') (valueOr e e')
    M.EqType moduleID typeExpr1 typeExpr2 -> do
      typeExpr1' <- substTy h env typeExpr1
      typeExpr2' <- substTy h env typeExpr2
      return $ build (changeOf typeExpr1' <> changeOf typeExpr2') $ M.EqType moduleID (valueOr typeExpr1 typeExpr1') (valueOr typeExpr2 typeExpr2')
    M.ShowType typeExpr -> do
      typeExpr' <- substTy h env typeExpr
      return $ build (changeOf typeExpr') $ M.ShowType (valueOr typeExpr typeExpr')
    M.TextCons rune text -> do
      rune' <- substTerm h env rune
      text' <- substTerm h env text
      return $ build (changeOf rune' <> changeOf text') $ M.TextCons (valueOr rune rune') (valueOr text text')
    M.TextUncons mid text -> do
      text' <- substTerm h env text
      return $ build (changeOf text') $ M.TextUncons mid (valueOr text text')
    M.MakeSwitch mid key fallback clauses -> do
      key' <- substTerm h env key
      fallback' <- substTerm h env fallback
      clauses' <- substTerm h env clauses
      return $ build (changeOf key' <> changeOf fallback' <> changeOf clauses') $ M.MakeSwitch mid (valueOr key key') (valueOr fallback fallback') (valueOr clauses clauses')
    M.FreeVariables mid code -> do
      code' <- substTerm h env code
      return $ build (changeOf code') $ M.FreeVariables mid (valueOr code code')
    M.CompileError msg -> do
      msg' <- substTerm h env msg
      return $ build (changeOf msg') $ M.CompileError (valueOr msg msg')
    M.GetOriginFileName ->
      return Kept
    M.GetOriginLine ->
      return Kept
    M.GetOriginColumn ->
      return Kept

substLowMagic :: Handle -> Env -> LM.LowMagic BLT.BaseLowType TM.Type TM.Term -> IO (Outcome (LM.LowMagic BLT.BaseLowType TM.Type TM.Term))
substLowMagic h env lowMagic =
  case lowMagic of
    LM.Cast from to value -> do
      from' <- substTy h env from
      to' <- substTy h env to
      value' <- substTerm h env value
      return $ build (changeOf from' <> changeOf to' <> changeOf value') $ LM.Cast (valueOr from from') (valueOr to to') (valueOr value value')
    LM.Store t unit value pointer -> do
      value' <- substTerm h env value
      pointer' <- substTerm h env pointer
      return $ build (changeOf value' <> changeOf pointer') $ LM.Store t unit (valueOr value value') (valueOr pointer pointer')
    LM.Load t pointer -> do
      pointer' <- substTerm h env pointer
      return $ build (changeOf pointer') $ LM.Load t (valueOr pointer pointer')
    LM.Alloca t size -> do
      size' <- substTerm h env size
      return $ build (changeOf size') $ LM.Alloca t (valueOr size size')
    LM.External domList cod extFunName args varArgs -> do
      args' <- mapM (substTerm h env) args
      let (varArgTerms, varArgTypes) = unzip varArgs
      varArgTerms' <- mapM (substTerm h env) varArgTerms
      let change = changes args' <> changes varArgTerms'
      return $ build change $ LM.External domList cod extFunName (values args args') (zip (values varArgTerms varArgTerms') varArgTypes)
    LM.Global {} ->
      return Kept
    LM.OpaqueValue e -> do
      e' <- substTerm h env e
      return $ build (changeOf e') $ LM.OpaqueValue (valueOr e e')
    LM.CallType func arg1 arg2 arg3 -> do
      func' <- substTerm h env func
      arg1' <- substTerm h env arg1
      arg2' <- substTerm h env arg2
      arg3' <- substTerm h env arg3
      return $ build (changeOf func' <> changeOf arg1' <> changeOf arg2' <> changeOf arg3') $ LM.CallType (valueOr func func') (valueOr arg1 arg1') (valueOr arg2 arg2') (valueOr arg3 arg3')
