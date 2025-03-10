module Scene.Elaborate.EnsureAffinity (ensureAffinity) where

import Context.App (App)
import Context.OptimizableData qualified as OptimizableData
import Context.Throw qualified as Throw
import Context.Type qualified as Type
import Context.WeakDefinition qualified as WeakDefinition
import Control.Comonad.Cofree
import Control.Lens (Bifunctor (bimap))
import Control.Monad
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.IntMap qualified as IntMap
import Data.Set qualified as S
import Entity.Attr.Data qualified as AttrD
import Entity.Attr.Lam qualified as AttrL
import Entity.Binder
import Entity.DecisionTree qualified as DT
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.Ident
import Entity.Ident.Reify
import Entity.LamKind qualified as LK
import Entity.Magic qualified as M
import Entity.OptimizableData qualified as OD
import Entity.Remark qualified as R
import Entity.Stuck qualified as Stuck
import Entity.Term qualified as TM
import Entity.Term.FreeVarsWithHints (freeVarsWithHints)
import Entity.Term.Weaken (weaken)
import Entity.WeakTerm qualified as WT
import Entity.WeakTerm.ToText qualified as WT
import Scene.WeakTerm.Reduce
import Scene.WeakTerm.Subst qualified as Subst

type AffineConstraint =
  (TM.Term, TM.Term)

type WeakAffineConstraint =
  (WT.WeakTerm, WT.WeakTerm)

type VarEnv = IntMap.IntMap TM.Term

data Axis
  = Axis
  { varEnv :: VarEnv,
    foundVarSetRef :: IORef (IntMap.IntMap Bool),
    mustPerformExpCheck :: Bool
  }

createNewAxis :: App Axis
createNewAxis = do
  let varEnv = IntMap.empty
  foundVarSetRef <- liftIO $ newIORef IntMap.empty
  let mustPerformExpCheck = True
  return Axis {varEnv, foundVarSetRef, mustPerformExpCheck}

extendAxis :: BinderF TM.Term -> Axis -> Axis
extendAxis (_, x, t) axis = do
  axis {varEnv = IntMap.insert (toInt x) t (varEnv axis)}

extendAxis' :: [BinderF TM.Term] -> Axis -> Axis
extendAxis' mxts axis = do
  case mxts of
    [] ->
      axis
    mxt : rest ->
      extendAxis mxt (extendAxis' rest axis)

mergeVarSet :: IntMap.IntMap Bool -> IntMap.IntMap Bool -> IntMap.IntMap Bool
mergeVarSet set1 set2 = do
  IntMap.unionWith (||) set1 set2

isExistingVar :: Ident -> Axis -> App (Maybe Bool)
isExistingVar i axis = do
  foundVarSet <- liftIO $ readIORef $ foundVarSetRef axis
  return $ IntMap.lookup (toInt i) foundVarSet

insertVar :: Ident -> Axis -> App ()
insertVar i axis = do
  liftIO $ modifyIORef' (foundVarSetRef axis) $ IntMap.insert (toInt i) False

insertRelevantVar :: Ident -> Axis -> App ()
insertRelevantVar i axis = do
  liftIO $ modifyIORef' (foundVarSetRef axis) $ IntMap.insert (toInt i) True

ensureAffinity :: TM.Term -> App [R.Remark]
ensureAffinity e = do
  axis <- createNewAxis
  cs <- analyze axis e
  synthesize $ map (bimap weaken weaken) cs

cloneAxis :: Axis -> App Axis
cloneAxis axis = do
  liftIO $ do
    foundVarSet <- readIORef $ foundVarSetRef axis
    foundVarSetRef <- newIORef foundVarSet
    let mustPerformExpCheck = True
    return Axis {varEnv = varEnv axis, foundVarSetRef, mustPerformExpCheck}

deactivateExpCheck :: Axis -> Axis
deactivateExpCheck axis =
  axis {mustPerformExpCheck = False}

analyzeVar :: Axis -> Hint -> Ident -> App [AffineConstraint]
analyzeVar axis m x = do
  if isCartesian x || not (mustPerformExpCheck axis)
    then return []
    else do
      boolOrNone <- isExistingVar x axis
      case boolOrNone of
        Nothing -> do
          insertVar x axis
          return []
        Just alreadyRegistered ->
          if alreadyRegistered
            then return []
            else do
              insertRelevantVar x axis
              _ :< t <- lookupTypeEnv (varEnv axis) m x
              return [(m :< t, m :< t)]

analyze :: Axis -> TM.Term -> App [AffineConstraint]
analyze axis term = do
  case term of
    _ :< TM.Tau ->
      return []
    m :< TM.Var x -> do
      analyzeVar axis m x
    _ :< TM.VarGlobal {} -> do
      return []
    _ :< TM.Pi impArgs expArgs t -> do
      (cs1, axis') <- analyzeBinder axis impArgs
      (cs2, axis'') <- analyzeBinder axis' expArgs
      cs3 <- analyze axis'' t
      return $ cs1 ++ cs2 ++ cs3
    m :< TM.PiIntro (AttrL.Attr {lamKind}) impArgs expArgs e -> do
      case lamKind of
        LK.Fix (mx, x, codType) -> do
          (cs1, axis') <- analyzeBinder axis impArgs
          (cs2, axis'') <- analyzeBinder axis' expArgs
          cs3 <- analyze axis'' codType
          let piType = m :< TM.Pi impArgs expArgs codType
          insertRelevantVar x axis''
          cs4 <- analyze (extendAxis (mx, x, piType) axis'') e
          css <- forM (S.toList $ freeVarsWithHints term) $ \(my, y) -> do
            analyzeVar axis my y
          return $ cs1 ++ cs2 ++ cs3 ++ cs4 ++ concat css
        LK.Normal codType -> do
          (cs1, axis') <- analyzeBinder axis impArgs
          (cs2, axis'') <- analyzeBinder axis' expArgs
          cs3 <- analyze axis'' codType
          cs4 <- analyze axis'' e
          return $ cs1 ++ cs2 ++ cs3 ++ cs4
    _ :< TM.PiElim e es -> do
      cs <- analyze axis e
      css <- mapM (analyze axis) es
      return $ cs ++ concat css
    _ :< TM.Data _ _ es -> do
      css <- mapM (analyze $ deactivateExpCheck axis) es
      return $ concat css
    _ :< TM.DataIntro _ _ dataArgs consArgs -> do
      css1 <- mapM (analyze $ deactivateExpCheck axis) dataArgs
      css2 <- mapM (analyze $ deactivateExpCheck axis) consArgs
      return $ concat css1 ++ concat css2
    m :< TM.DataElim _ oets tree -> do
      let (os, es, ts) = unzip3 oets
      cs1 <- concat <$> mapM (analyze axis) es
      cs2 <- concat <$> mapM (analyze axis) ts
      let mots = zipWith (\o t -> (m, o, t)) os ts
      cs3 <- analyzeDecisionTree (extendAxis' mots axis) tree
      return $ cs1 ++ cs2 ++ cs3
    _ :< TM.Box t -> do
      analyze axis t
    _ :< TM.BoxNoema t -> do
      analyze axis t
    _ :< TM.BoxIntro letSeq e -> do
      (cs1, axis') <- analyzeLet axis letSeq
      cs2 <- analyze axis' e
      return $ cs1 ++ cs2
    _ :< TM.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      (cs, axis') <- analyzeLet axis $ castSeq ++ [(mxt, e1)] ++ uncastSeq
      cs' <- analyze axis' e2
      return $ cs ++ cs'
    _ :< TM.Let _ mxt e1 e2 -> do
      (cs1, axis') <- analyzeLet axis [(mxt, e1)]
      cs2 <- analyze axis' e2
      return $ cs1 ++ cs2
    _ :< TM.Prim {} -> do
      return []
    _ :< TM.Magic magic -> do
      case magic of
        M.Cast from to value -> do
          cs0 <- analyze axis from
          cs1 <- analyze axis to
          cs2 <- analyze axis value
          return $ cs0 ++ cs1 ++ cs2
        M.Store _ unit e1 e2 -> do
          cs1 <- analyze axis unit
          cs2 <- analyze axis e1
          cs3 <- analyze axis e2
          return $ cs1 ++ cs2 ++ cs3
        M.Load _ e -> do
          analyze axis e
        M.Alloca _ size -> do
          analyze axis size
        M.External _ _ _ es ets -> do
          let args = es ++ map fst ets
          concat <$> mapM (analyze axis) args
        M.Global _ _ ->
          return []
        M.OpaqueValue e ->
          analyze axis e
    _ :< TM.Resource _ _ unitType discarder copier -> do
      cs1 <- analyze axis unitType
      cs2 <- analyze axis discarder
      cs3 <- analyze axis copier
      return $ cs1 ++ cs2 ++ cs3
    _ :< TM.Void ->
      return []

analyzeBinder ::
  Axis ->
  [BinderF TM.Term] ->
  App ([AffineConstraint], Axis)
analyzeBinder axis binder =
  case binder of
    [] -> do
      return ([], axis)
    ((mx, x, t) : xts) -> do
      cs <- analyze axis t
      (cs', axis') <- analyzeBinder (extendAxis (mx, x, t) axis) xts
      return (cs ++ cs', axis')

analyzeLet ::
  Axis ->
  [(BinderF TM.Term, TM.Term)] ->
  App ([AffineConstraint], Axis)
analyzeLet axis xtes =
  case xtes of
    [] ->
      return ([], axis)
    ((m, x, t), e) : rest -> do
      cs0 <- analyze axis t
      cs1 <- analyze axis e
      (cs', axis') <- analyzeLet (extendAxis (m, x, t) axis) rest
      return (cs0 ++ cs1 ++ cs', axis')

lookupTypeEnv :: VarEnv -> Hint -> Ident -> App TM.Term
lookupTypeEnv varEnv m x =
  case IntMap.lookup (toInt x) varEnv of
    Just t ->
      return t
    Nothing ->
      Throw.raiseCritical m $
        "Scene.Elaborate.EnsureAffinity: the type of the variable `"
          <> toText' x
          <> "` is not registered in the type environment"

analyzeDecisionTree ::
  Axis ->
  DT.DecisionTree TM.Term ->
  App [AffineConstraint]
analyzeDecisionTree axis tree =
  case tree of
    DT.Leaf _ letSeq body -> do
      (cs1, axis') <- analyzeLet axis letSeq
      cs2 <- analyze axis' body
      return $ cs1 ++ cs2
    DT.Unreachable -> do
      return []
    DT.Switch (_, cursorType) caseList -> do
      cs1 <- analyze axis cursorType
      cs2 <- analyzeClauseList axis caseList
      return $ cs1 ++ cs2

analyzeClauseList ::
  Axis ->
  DT.CaseList TM.Term ->
  App [AffineConstraint]
analyzeClauseList axis (fallbackClause, caseList) = do
  newVarSetRef <- liftIO $ newIORef IntMap.empty
  css <- forM caseList $ \c -> do
    axis' <- cloneAxis axis
    cs <- analyzeCase axis' c
    branchVarSet <- liftIO $ readIORef $ foundVarSetRef axis'
    liftIO $ modifyIORef' newVarSetRef $ mergeVarSet branchVarSet
    return cs
  cs <- analyzeDecisionTree axis fallbackClause
  fallbackVarSet <- liftIO $ readIORef $ foundVarSetRef axis
  liftIO $ modifyIORef' newVarSetRef $ mergeVarSet fallbackVarSet
  newVarSet <- liftIO $ readIORef newVarSetRef
  liftIO $ writeIORef (foundVarSetRef axis) newVarSet
  return $ cs ++ concat css

analyzeCase ::
  Axis ->
  DT.Case TM.Term ->
  App [AffineConstraint]
analyzeCase axis decisionCase = do
  case decisionCase of
    DT.LiteralCase _ _ cont -> do
      analyzeDecisionTree axis cont
    DT.ConsCase (DT.ConsCaseRecord {..}) -> do
      let (es1, ts1) = unzip dataArgs
      cs1 <- concat <$> mapM (analyze axis) (es1 ++ ts1)
      (cs2, axis') <- analyzeBinder axis consArgs
      cs3 <- analyzeDecisionTree axis' cont
      return $ cs1 ++ cs2 ++ cs3

synthesize :: [WeakAffineConstraint] -> App [R.Remark]
synthesize cs = do
  errorList <- concat <$> mapM (simplifyAffine S.empty) cs
  return $ map constructErrorMessageAffine errorList

newtype AffineConstraintError
  = AffineConstraintError WT.WeakTerm

constructErrorMessageAffine :: AffineConstraintError -> R.Remark
constructErrorMessageAffine (AffineConstraintError t) =
  R.newRemark (WT.metaOf t) R.Error $
    "The type of this affine variable is not affine, but:\n"
      <> WT.toText t

simplifyAffine ::
  S.Set DD.DefiniteDescription ->
  WeakAffineConstraint ->
  App [AffineConstraintError]
simplifyAffine dataNameSet (t, orig@(m :< _)) = do
  t' <- reduce t
  case t' of
    _ :< WT.Tau -> do
      return []
    _ :< WT.Data (AttrD.Attr {consNameList}) dataName dataArgs -> do
      od <- OptimizableData.lookup dataName
      case od of
        Just OD.Enum -> do
          return []
        Just OD.Unary -> do
          let dataNameSet' = S.insert dataName dataNameSet
          constraintsFromDataArgs <- fmap concat $ forM dataArgs $ \dataArg ->
            simplifyAffine dataNameSet' (dataArg, orig)
          dataConsArgsList <-
            if S.member dataName dataNameSet
              then return []
              else mapM (getConsArgTypes m . fst) consNameList
          constraintsFromDataConsArgs <- fmap concat $ forM dataConsArgsList $ \dataConsArgs -> do
            dataConsArgs' <- substConsArgs IntMap.empty dataConsArgs
            fmap concat $ forM dataConsArgs' $ \(_, _, consArg) -> do
              simplifyAffine dataNameSet' (consArg, orig)
          return $ constraintsFromDataArgs ++ constraintsFromDataConsArgs
        _ -> do
          return [AffineConstraintError orig]
    _ :< WT.BoxNoema {} ->
      return []
    _ :< WT.Prim {} -> do
      return []
    _ -> do
      defMap <- WeakDefinition.read
      case Stuck.asStuckedTerm t' of
        Just (Stuck.VarGlobal dd, evalCtx)
          | Just lam <- Map.lookup dd defMap -> do
              simplifyAffine dataNameSet (Stuck.resume lam evalCtx, orig)
        _ -> do
          return [AffineConstraintError orig]

substConsArgs :: WT.SubstWeakTerm -> [BinderF WT.WeakTerm] -> App [BinderF WT.WeakTerm]
substConsArgs sub consArgs =
  case consArgs of
    [] ->
      return []
    (m, x, t) : rest -> do
      t' <- Subst.subst sub t
      let opaque = m :< WT.Tau -- allow `a` in `Cons(a: type, x: a)`
      let sub' = IntMap.insert (toInt x) (Right opaque) sub
      rest' <- substConsArgs sub' rest
      return $ (m, x, t') : rest'

getConsArgTypes ::
  Hint ->
  DD.DefiniteDescription ->
  App [BinderF WT.WeakTerm]
getConsArgTypes m consName = do
  t <- Type.lookup m consName
  case t of
    _ :< WT.Pi impArgs expArgs _ -> do
      return $ impArgs ++ expArgs
    _ ->
      Throw.raiseCritical m $ "The type of a constructor must be a Π-type, but it's not:\n" <> WT.toText t
