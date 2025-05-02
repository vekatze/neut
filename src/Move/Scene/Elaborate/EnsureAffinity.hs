module Move.Scene.Elaborate.EnsureAffinity
  ( Handle,
    new,
    ensureAffinity,
  )
where

import Control.Comonad.Cofree
import Control.Lens (Bifunctor (bimap))
import Control.Monad
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.IntMap qualified as IntMap
import Data.Set qualified as S
import Move.Context.EIO (EIO, raiseCritical)
import Move.Context.OptimizableData qualified as OptimizableData
import Move.Context.Type qualified as Type
import Move.Scene.Elaborate.Handle.Elaborate qualified as Elaborate
import Move.Scene.Elaborate.Handle.WeakDef qualified as WeakDef
import Move.Scene.Elaborate.WeakTerm.Subst qualified as Subst
import Rule.Attr.Data qualified as AttrD
import Rule.Attr.Lam qualified as AttrL
import Rule.Binder
import Rule.DecisionTree qualified as DT
import Rule.DefiniteDescription qualified as DD
import Rule.Hint
import Rule.Ident
import Rule.Ident.Reify
import Rule.LamKind qualified as LK
import Rule.Magic qualified as M
import Rule.OptimizableData
import Rule.OptimizableData qualified as OD
import Rule.Remark qualified as R
import Rule.Stuck qualified as Stuck
import Rule.Term qualified as TM
import Rule.Term.FreeVarsWithHints (freeVarsWithHints)
import Rule.Term.Weaken (weaken)
import Rule.WeakTerm qualified as WT
import Rule.WeakTerm.ToText qualified as WT

type AffineConstraint =
  (TM.Term, TM.Term)

type WeakAffineConstraint =
  (WT.WeakTerm, WT.WeakTerm)

type VarEnv = IntMap.IntMap TM.Term

data Handle
  = Handle
  { elaborateHandle :: Elaborate.Handle,
    varEnv :: VarEnv,
    foundVarSetRef :: IORef (IntMap.IntMap Bool),
    mustPerformExpCheck :: Bool
  }

new :: Elaborate.Handle -> IO Handle
new elaborateHandle = do
  foundVarSetRef <- liftIO $ newIORef IntMap.empty
  let mustPerformExpCheck = True
  let varEnv = IntMap.empty
  return $ Handle {..}

ensureAffinity :: Handle -> TM.Term -> EIO [R.Remark]
ensureAffinity h e = do
  cs <- analyze h e
  synthesize h $ map (bimap weaken weaken) cs

extendHandle :: BinderF TM.Term -> Handle -> Handle
extendHandle (_, x, t) h = do
  h {varEnv = IntMap.insert (toInt x) t (varEnv h)}

extendHandle' :: [BinderF TM.Term] -> Handle -> Handle
extendHandle' mxts h = do
  case mxts of
    [] ->
      h
    mxt : rest ->
      extendHandle mxt (extendHandle' rest h)

mergeVarSet :: IntMap.IntMap Bool -> IntMap.IntMap Bool -> IntMap.IntMap Bool
mergeVarSet set1 set2 = do
  IntMap.unionWith (||) set1 set2

isExistingVar :: Ident -> Handle -> IO (Maybe Bool)
isExistingVar i h = do
  foundVarSet <- readIORef $ foundVarSetRef h
  return $ IntMap.lookup (toInt i) foundVarSet

insertVar :: Ident -> Handle -> IO ()
insertVar i h = do
  modifyIORef' (foundVarSetRef h) $ IntMap.insert (toInt i) False

insertRelevantVar :: Ident -> Handle -> IO ()
insertRelevantVar i h = do
  modifyIORef' (foundVarSetRef h) $ IntMap.insert (toInt i) True

cloneHandle :: Handle -> IO Handle
cloneHandle h = do
  foundVarSet <- readIORef $ foundVarSetRef h
  foundVarSetRef <- newIORef foundVarSet
  let mustPerformExpCheck = True
  return $ h {foundVarSetRef, mustPerformExpCheck}

deactivateExpCheck :: Handle -> Handle
deactivateExpCheck h =
  h {mustPerformExpCheck = False}

analyzeVar :: Handle -> Hint -> Ident -> EIO [AffineConstraint]
analyzeVar h m x = do
  if isCartesian x || not (mustPerformExpCheck h)
    then return []
    else do
      boolOrNone <- liftIO $ isExistingVar x h
      case boolOrNone of
        Nothing -> do
          liftIO $ insertVar x h
          return []
        Just alreadyRegistered ->
          if alreadyRegistered
            then return []
            else do
              liftIO $ insertRelevantVar x h
              _ :< t <- lookupTypeEnv (varEnv h) m x
              return [(m :< t, m :< t)]

analyze :: Handle -> TM.Term -> EIO [AffineConstraint]
analyze h term = do
  case term of
    _ :< TM.Tau ->
      return []
    m :< TM.Var x -> do
      analyzeVar h m x
    _ :< TM.VarGlobal {} -> do
      return []
    _ :< TM.Pi impArgs expArgs t -> do
      (cs1, h') <- analyzeBinder h impArgs
      (cs2, h'') <- analyzeBinder h' expArgs
      cs3 <- analyze h'' t
      return $ cs1 ++ cs2 ++ cs3
    m :< TM.PiIntro (AttrL.Attr {lamKind}) impArgs expArgs e -> do
      case lamKind of
        LK.Fix (mx, x, codType) -> do
          (cs1, h') <- analyzeBinder h impArgs
          (cs2, h'') <- analyzeBinder h' expArgs
          cs3 <- analyze h'' codType
          let piType = m :< TM.Pi impArgs expArgs codType
          liftIO $ insertRelevantVar x h''
          cs4 <- analyze (extendHandle (mx, x, piType) h'') e
          css <- forM (S.toList $ freeVarsWithHints term) $ uncurry (analyzeVar h)
          return $ cs1 ++ cs2 ++ cs3 ++ cs4 ++ concat css
        LK.Normal codType -> do
          (cs1, h') <- analyzeBinder h impArgs
          (cs2, h'') <- analyzeBinder h' expArgs
          cs3 <- analyze h'' codType
          cs4 <- analyze h'' e
          return $ cs1 ++ cs2 ++ cs3 ++ cs4
    _ :< TM.PiElim e es -> do
      cs <- analyze h e
      css <- mapM (analyze h) es
      return $ cs ++ concat css
    _ :< TM.Data _ _ es -> do
      css <- mapM (analyze $ deactivateExpCheck h) es
      return $ concat css
    _ :< TM.DataIntro _ _ dataArgs consArgs -> do
      css1 <- mapM (analyze $ deactivateExpCheck h) dataArgs
      css2 <- mapM (analyze $ deactivateExpCheck h) consArgs
      return $ concat css1 ++ concat css2
    m :< TM.DataElim _ oets tree -> do
      let (os, es, ts) = unzip3 oets
      cs1 <- concat <$> mapM (analyze h) es
      cs2 <- concat <$> mapM (analyze h) ts
      let mots = zipWith (\o t -> (m, o, t)) os ts
      cs3 <- analyzeDecisionTree (extendHandle' mots h) tree
      return $ cs1 ++ cs2 ++ cs3
    _ :< TM.Box t -> do
      analyze h t
    _ :< TM.BoxNoema t -> do
      analyze h t
    _ :< TM.BoxIntro letSeq e -> do
      (cs1, h') <- analyzeLet h letSeq
      cs2 <- analyze h' e
      return $ cs1 ++ cs2
    _ :< TM.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      (cs, h') <- analyzeLet h $ castSeq ++ [(mxt, e1)] ++ uncastSeq
      cs' <- analyze h' e2
      return $ cs ++ cs'
    _ :< TM.Let _ mxt e1 e2 -> do
      (cs1, h') <- analyzeLet h [(mxt, e1)]
      cs2 <- analyze h' e2
      return $ cs1 ++ cs2
    _ :< TM.Prim {} -> do
      return []
    _ :< TM.Magic magic -> do
      case magic of
        M.Cast from to value -> do
          cs0 <- analyze h from
          cs1 <- analyze h to
          cs2 <- analyze h value
          return $ cs0 ++ cs1 ++ cs2
        M.Store _ unit e1 e2 -> do
          cs1 <- analyze h unit
          cs2 <- analyze h e1
          cs3 <- analyze h e2
          return $ cs1 ++ cs2 ++ cs3
        M.Load _ e -> do
          analyze h e
        M.Alloca _ size -> do
          analyze h size
        M.External _ _ _ es ets -> do
          let args = es ++ map fst ets
          concat <$> mapM (analyze h) args
        M.Global _ _ ->
          return []
        M.OpaqueValue e ->
          analyze h e
    _ :< TM.Resource _ _ unitType discarder copier -> do
      cs1 <- analyze h unitType
      cs2 <- analyze h discarder
      cs3 <- analyze h copier
      return $ cs1 ++ cs2 ++ cs3
    _ :< TM.Void ->
      return []

analyzeBinder ::
  Handle ->
  [BinderF TM.Term] ->
  EIO ([AffineConstraint], Handle)
analyzeBinder h binder =
  case binder of
    [] -> do
      return ([], h)
    ((mx, x, t) : xts) -> do
      cs <- analyze h t
      (cs', h') <- analyzeBinder (extendHandle (mx, x, t) h) xts
      return (cs ++ cs', h')

analyzeLet ::
  Handle ->
  [(BinderF TM.Term, TM.Term)] ->
  EIO ([AffineConstraint], Handle)
analyzeLet h xtes =
  case xtes of
    [] ->
      return ([], h)
    ((m, x, t), e) : rest -> do
      cs0 <- analyze h t
      cs1 <- analyze h e
      (cs', h') <- analyzeLet (extendHandle (m, x, t) h) rest
      return (cs0 ++ cs1 ++ cs', h')

lookupTypeEnv :: VarEnv -> Hint -> Ident -> EIO TM.Term
lookupTypeEnv varEnv m x =
  case IntMap.lookup (toInt x) varEnv of
    Just t ->
      return t
    Nothing ->
      raiseCritical m $
        "Scene.Elaborate.EnsureAffinity: the type of the variable `"
          <> toText' x
          <> "` is not registered in the type environment"

analyzeDecisionTree ::
  Handle ->
  DT.DecisionTree TM.Term ->
  EIO [AffineConstraint]
analyzeDecisionTree h tree =
  case tree of
    DT.Leaf _ letSeq body -> do
      (cs1, h') <- analyzeLet h letSeq
      cs2 <- analyze h' body
      return $ cs1 ++ cs2
    DT.Unreachable -> do
      return []
    DT.Switch (_, cursorType) caseList -> do
      cs1 <- analyze h cursorType
      cs2 <- analyzeClauseList h caseList
      return $ cs1 ++ cs2

analyzeClauseList ::
  Handle ->
  DT.CaseList TM.Term ->
  EIO [AffineConstraint]
analyzeClauseList h (fallbackClause, caseList) = do
  newVarSetRef <- liftIO $ newIORef IntMap.empty
  css <- forM caseList $ \c -> do
    h' <- liftIO $ cloneHandle h
    cs <- analyzeCase h' c
    branchVarSet <- liftIO $ readIORef $ foundVarSetRef h'
    liftIO $ modifyIORef' newVarSetRef $ mergeVarSet branchVarSet
    return cs
  cs <- analyzeDecisionTree h fallbackClause
  fallbackVarSet <- liftIO $ readIORef $ foundVarSetRef h
  liftIO $ modifyIORef' newVarSetRef $ mergeVarSet fallbackVarSet
  newVarSet <- liftIO $ readIORef newVarSetRef
  liftIO $ writeIORef (foundVarSetRef h) newVarSet
  return $ cs ++ concat css

analyzeCase ::
  Handle ->
  DT.Case TM.Term ->
  EIO [AffineConstraint]
analyzeCase h decisionCase = do
  case decisionCase of
    DT.LiteralCase _ _ cont -> do
      analyzeDecisionTree h cont
    DT.ConsCase (DT.ConsCaseRecord {..}) -> do
      let (es1, ts1) = unzip dataArgs
      cs1 <- concat <$> mapM (analyze h) (es1 ++ ts1)
      (cs2, h') <- analyzeBinder h consArgs
      cs3 <- analyzeDecisionTree h' cont
      return $ cs1 ++ cs2 ++ cs3

synthesize :: Handle -> [WeakAffineConstraint] -> EIO [R.Remark]
synthesize h cs = do
  errorList <- concat <$> mapM (simplifyAffine h S.empty) cs
  return $ map constructErrorMessageAffine errorList

newtype AffineConstraintError
  = AffineConstraintError WT.WeakTerm

constructErrorMessageAffine :: AffineConstraintError -> R.Remark
constructErrorMessageAffine (AffineConstraintError t) =
  R.newRemark (WT.metaOf t) R.Error $
    "The type of this affine variable is not affine, but:\n"
      <> WT.toText t

simplifyAffine ::
  Handle ->
  S.Set DD.DefiniteDescription ->
  WeakAffineConstraint ->
  EIO [AffineConstraintError]
simplifyAffine h dataNameSet (t, orig@(m :< _)) = do
  t' <- Elaborate.reduce (elaborateHandle h) t
  case t' of
    _ :< WT.Tau -> do
      return []
    _ :< WT.Data (AttrD.Attr {consNameList}) dataName dataArgs -> do
      optDataOrNone <- liftIO $ lookupOptimizableData h dataName
      case optDataOrNone of
        Just OD.Enum -> do
          return []
        Just OD.Unary -> do
          let dataNameSet' = S.insert dataName dataNameSet
          constraintsFromDataArgs <- fmap concat $ forM dataArgs $ \dataArg ->
            simplifyAffine h dataNameSet' (dataArg, orig)
          dataConsArgsList <-
            if S.member dataName dataNameSet
              then return []
              else mapM (getConsArgTypes h m . fst) consNameList
          constraintsFromDataConsArgs <- fmap concat $ forM dataConsArgsList $ \dataConsArgs -> do
            dataConsArgs' <- substConsArgs h IntMap.empty dataConsArgs
            fmap concat $ forM dataConsArgs' $ \(_, _, consArg) -> do
              simplifyAffine h dataNameSet' (consArg, orig)
          return $ constraintsFromDataArgs ++ constraintsFromDataConsArgs
        _ -> do
          return [AffineConstraintError orig]
    _ :< WT.BoxNoema {} ->
      return []
    _ :< WT.Prim {} -> do
      return []
    _ -> do
      defMap <- liftIO $ WeakDef.read' (Elaborate.weakDefHandle (elaborateHandle h))
      case Stuck.asStuckedTerm t' of
        Just (Stuck.VarGlobal dd, evalCtx)
          | Just lam <- Map.lookup dd defMap -> do
              simplifyAffine h dataNameSet (Stuck.resume lam evalCtx, orig)
        _ -> do
          return [AffineConstraintError orig]

substConsArgs :: Handle -> WT.SubstWeakTerm -> [BinderF WT.WeakTerm] -> EIO [BinderF WT.WeakTerm]
substConsArgs h sub consArgs =
  case consArgs of
    [] ->
      return []
    (m, x, t) : rest -> do
      t' <- liftIO $ Subst.subst (Elaborate.substHandle (elaborateHandle h)) sub t
      let opaque = m :< WT.Tau -- allow `a` in `Cons(a: type, x: a)`
      let sub' = IntMap.insert (toInt x) (Right opaque) sub
      rest' <- substConsArgs h sub' rest
      return $ (m, x, t') : rest'

getConsArgTypes ::
  Handle ->
  Hint ->
  DD.DefiniteDescription ->
  EIO [BinderF WT.WeakTerm]
getConsArgTypes h m consName = do
  t <- Type.lookup' (Elaborate.typeHandle (elaborateHandle h)) m consName
  case t of
    _ :< WT.Pi impArgs expArgs _ -> do
      return $ impArgs ++ expArgs
    _ ->
      raiseCritical m $ "The type of a constructor must be a Π-type, but it's not:\n" <> WT.toText t

lookupOptimizableData :: Handle -> DD.DefiniteDescription -> IO (Maybe OptimizableData)
lookupOptimizableData h dd = do
  OptimizableData.lookup (Elaborate.optDataHandle (elaborateHandle h)) dd
