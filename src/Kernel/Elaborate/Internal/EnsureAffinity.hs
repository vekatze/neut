module Kernel.Elaborate.Internal.EnsureAffinity
  ( Handle,
    new,
    ensureAffinity,
  )
where

import App.App (App)
import App.Run (raiseCritical)
import Control.Comonad.Cofree
import Control.Lens (Bifunctor (bimap))
import Control.Monad
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.IntMap qualified as IntMap
import Data.Maybe (catMaybes)
import Data.Set qualified as S
import Kernel.Common.Handle.Global.OptimizableData qualified as OptimizableData
import Kernel.Common.Handle.Global.Type qualified as Type
import Kernel.Common.OptimizableData
import Kernel.Common.OptimizableData qualified as OD
import Kernel.Elaborate.Internal.Handle.Elaborate qualified as Elaborate
import Kernel.Elaborate.Internal.Handle.WeakTypeDef qualified as WeakTypeDef
import Kernel.Elaborate.Stuck qualified as Stuck
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.Binder
import Language.Common.DecisionTree qualified as DT
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Ident
import Language.Common.Ident.Reify
import Language.Common.LamKind qualified as LK
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.Common.PiKind qualified as PK
import Language.Common.VarKind qualified as VK
import Language.Term.FreeVarsWithHints (freeVarsWithHints)
import Language.Term.Term qualified as TM
import Language.Term.Weaken (weakenType)
import Language.WeakTerm.Subst (SubstEntry (..))
import Language.WeakTerm.Subst qualified as Subst
import Language.WeakTerm.ToText qualified as WT
import Language.WeakTerm.WeakTerm qualified as WT
import Logger.Hint
import Logger.Log qualified as L
import Logger.LogLevel qualified as L

type AffineConstraint =
  (TM.Type, TM.Type)

type WeakAffineConstraint =
  (WT.WeakType, WT.WeakType)

type VarEnv = IntMap.IntMap (VK.VarKind, TM.Type)

data Handle = Handle
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

ensureAffinity :: Handle -> TM.Term -> App [L.Log]
ensureAffinity h e = do
  cs <- analyze h e
  synthesize h $ map (bimap weakenType weakenType) cs

extendHandle :: BinderF TM.Type -> Handle -> Handle
extendHandle (_, k, x, t) h = do
  h {varEnv = IntMap.insert (toInt x) (k, t) (varEnv h)}

extendHandle' :: [BinderF TM.Type] -> Handle -> Handle
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

analyzeVar :: Handle -> Hint -> Ident -> App [AffineConstraint]
analyzeVar h m x = do
  if not (mustPerformExpCheck h)
    then return []
    else do
      varKind <- lookupVarKind (varEnv h) m x
      if VK.isExp varKind
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

analyze :: Handle -> TM.Term -> App [AffineConstraint]
analyze h term = do
  case term of
    m :< TM.Var x -> do
      analyzeVar h m x
    _ :< TM.VarGlobal {} -> do
      return []
    m :< TM.PiIntro (AttrL.Attr {lamKind}) impArgs expArgs defaultArgs e -> do
      let ds = map snd defaultArgs
      case lamKind of
        LK.Fix _ isDestPassing (mx, _k, x, codType) -> do
          (cs1, h') <- analyzeBinder h (impArgs ++ expArgs)
          (cs2, h'') <- analyzeBinder h' (map fst defaultArgs)
          cs3 <- concat <$> mapM (analyze h'') ds
          cs4 <- analyzeType h'' codType
          let piKind = if isDestPassing then PK.DestPass False else PK.normal
          let piType = m :< TM.Pi piKind impArgs expArgs (map fst defaultArgs) codType
          liftIO $ insertRelevantVar x h''
          cs5 <- analyze (extendHandle (mx, VK.Normal, x, piType) h'') e
          css <- forM (S.toList $ freeVarsWithHints term) $ uncurry (analyzeVar h)
          return $ cs1 ++ cs2 ++ cs3 ++ cs4 ++ cs5 ++ concat css
        LK.Normal _ _ codType -> do
          (cs1, h') <- analyzeBinder h (impArgs ++ expArgs)
          (cs2, h'') <- analyzeBinder h' (map fst defaultArgs)
          cs3 <- concat <$> mapM (analyze h'') ds
          cs4 <- analyzeType h'' codType
          cs5 <- analyze h'' e
          return $ cs1 ++ cs2 ++ cs3 ++ cs4 ++ cs5
    _ :< TM.PiElim _ e impArgs expArgs defaultArgs -> do
      cs <- analyze h e
      css1 <- mapM (analyzeType h) impArgs
      css2 <- mapM (analyze h) expArgs
      css3 <- mapM (analyze h) (catMaybes defaultArgs)
      return $ cs ++ concat css1 ++ concat css2 ++ concat css3
    _ :< TM.DataIntro _ _ dataArgs consArgs -> do
      css1 <- mapM (analyzeType $ deactivateExpCheck h) dataArgs
      css2 <- mapM (analyze $ deactivateExpCheck h) consArgs
      return $ concat css1 ++ concat css2
    m :< TM.DataElim _ oets tree -> do
      let (os, es, ts) = unzip3 oets
      cs1 <- concat <$> mapM (analyze h) es
      cs2 <- concat <$> mapM (analyzeType h) ts
      let mots = zipWith (\o t -> (m, VK.Normal, o, t)) os ts
      cs3 <- analyzeDecisionTree (extendHandle' mots h) tree
      return $ cs1 ++ cs2 ++ cs3
    _ :< TM.BoxIntro letSeq e -> do
      (cs1, h') <- analyzeLet h letSeq
      cs2 <- analyze h' e
      return $ cs1 ++ cs2
    _ :< TM.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      (cs, h') <- analyzeLet h $ castSeq ++ [(mxt, e1)] ++ uncastSeq
      cs' <- analyze h' e2
      return $ cs ++ cs'
    _ :< TM.CodeIntro e -> do
      analyze h e
    _ :< TM.CodeElim e -> do
      analyze h e
    _ :< TM.TauIntro ty -> do
      analyzeType h ty
    _ :< TM.TauElim (mx, x) e1 e2 -> do
      let mxt = (mx, VK.Normal, x, mx :< TM.Tau)
      (cs1, h') <- analyzeLet h [(mxt, e1)]
      cs2 <- analyze h' e2
      return $ cs1 ++ cs2
    _ :< TM.Let _ mxt e1 e2 -> do
      (cs1, h') <- analyzeLet h [(mxt, e1)]
      cs2 <- analyze h' e2
      return $ cs1 ++ cs2
    _ :< TM.Prim {} -> do
      return []
    _ :< TM.Magic magic -> do
      case magic of
        M.LowMagic lowMagic ->
          case lowMagic of
            LM.Cast from to value -> do
              cs0 <- analyzeType h from
              cs1 <- analyzeType h to
              cs2 <- analyze h value
              return $ cs0 ++ cs1 ++ cs2
            LM.Store _ _ e1 e2 -> do
              cs2 <- analyze h e1
              cs3 <- analyze h e2
              return $ cs2 ++ cs3
            LM.Load _ e -> do
              cs1 <- analyze h e
              return cs1
            LM.Alloca _ size -> do
              cs1 <- analyze h size
              return cs1
            LM.External _ _ _ es ets -> do
              let args = es ++ map fst ets
              css <- concat <$> mapM (analyze h) args
              return css
            LM.Global {} -> do
              return []
            LM.OpaqueValue e ->
              analyze h e
            LM.CallType func arg1 arg2 -> do
              cs1 <- analyze h func
              cs2 <- analyze h arg1
              cs3 <- analyze h arg2
              return $ cs1 ++ cs2 ++ cs3
        M.InspectType _ typeValueExpr typeExpr -> do
          cs1 <- analyzeType h typeValueExpr
          cs2 <- analyzeType h typeExpr
          return $ cs1 ++ cs2
        M.EqType _ typeExpr1 typeExpr2 -> do
          cs1 <- analyzeType h typeExpr1
          cs2 <- analyzeType h typeExpr2
          return $ cs1 ++ cs2
        M.ShowType textTypeExpr typeExpr -> do
          cs1 <- analyzeType h textTypeExpr
          cs2 <- analyzeType h typeExpr
          return $ cs1 ++ cs2
        M.TextCons textTypeExpr rune text -> do
          cs1 <- analyzeType h textTypeExpr
          cs2 <- analyze h rune
          cs3 <- analyze h text
          return $ cs1 ++ cs2 ++ cs3
        M.TextUncons _ text -> do
          analyze h text
        M.CompileError typeExpr msg -> do
          cs1 <- analyzeType h typeExpr
          cs2 <- analyze h msg
          return $ cs1 ++ cs2

analyzeType :: Handle -> TM.Type -> App [AffineConstraint]
analyzeType h ty =
  case ty of
    _ :< TM.Tau ->
      return []
    _ :< TM.TVar {} ->
      return []
    _ :< TM.TVarGlobal {} ->
      return []
    _ :< TM.TyApp t args -> do
      cs0 <- analyzeType h t
      css <- mapM (analyzeType h) args
      return $ cs0 ++ concat css
    _ :< TM.Pi _ impArgs expArgs defaultArgs t -> do
      let impBinders = impArgs ++ expArgs ++ defaultArgs
      (cs1, h') <- analyzeBinder h impBinders
      (cs2, h'') <- analyzeBinder h' expArgs
      cs3 <- analyzeType h'' t
      return $ cs1 ++ cs2 ++ cs3
    _ :< TM.Data _ _ es -> do
      css <- mapM (analyzeType $ deactivateExpCheck h) es
      return $ concat css
    _ :< TM.Box t -> do
      analyzeType h t
    _ :< TM.BoxNoema t -> do
      analyzeType h t
    _ :< TM.Code t -> do
      analyzeType h t
    _ :< TM.PrimType {} ->
      return []
    _ :< TM.Void ->
      return []
    _ :< TM.Resource _ _ -> do
      return []

analyzeBinder ::
  Handle ->
  [BinderF TM.Type] ->
  App ([AffineConstraint], Handle)
analyzeBinder h binder =
  case binder of
    [] -> do
      return ([], h)
    ((mx, k, x, t) : xts) -> do
      cs <- analyzeType h t
      (cs', h') <- analyzeBinder (extendHandle (mx, k, x, t) h) xts
      return (cs ++ cs', h')

analyzeLet ::
  Handle ->
  [(BinderF TM.Type, TM.Term)] ->
  App ([AffineConstraint], Handle)
analyzeLet h xtes =
  case xtes of
    [] ->
      return ([], h)
    ((m, k, x, t), e) : rest -> do
      cs0 <- analyzeType h t
      cs1 <- analyze h e
      (cs', h') <- analyzeLet (extendHandle (m, k, x, t) h) rest
      return (cs0 ++ cs1 ++ cs', h')

lookupTypeEnv :: VarEnv -> Hint -> Ident -> App TM.Type
lookupTypeEnv varEnv m x =
  case IntMap.lookup (toInt x) varEnv of
    Just (_, t) ->
      return t
    Nothing ->
      raiseCritical m $
        "Scene.Elaborate.EnsureAffinity: the type of the variable `"
          <> toText' x
          <> "` is not registered in the type environment"

lookupVarKind :: VarEnv -> Hint -> Ident -> App VK.VarKind
lookupVarKind varEnv m x =
  case IntMap.lookup (toInt x) varEnv of
    Just (k, _) ->
      return k
    Nothing ->
      raiseCritical m $
        "Scene.Elaborate.EnsureAffinity: the kind of the variable `"
          <> toText' x
          <> "` is not registered in the type environment"

analyzeDecisionTree ::
  Handle ->
  DT.DecisionTree TM.Type TM.Term ->
  App [AffineConstraint]
analyzeDecisionTree h tree =
  case tree of
    DT.Leaf _ letSeq body -> do
      (cs1, h') <- analyzeLet h letSeq
      cs2 <- analyze h' body
      return $ cs1 ++ cs2
    DT.Unreachable -> do
      return []
    DT.Switch (_, cursorType) caseList -> do
      cs1 <- analyzeType h cursorType
      cs2 <- analyzeClauseList h caseList
      return $ cs1 ++ cs2

analyzeClauseList ::
  Handle ->
  DT.CaseList TM.Type TM.Term ->
  App [AffineConstraint]
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
  DT.Case TM.Type TM.Term ->
  App [AffineConstraint]
analyzeCase h decisionCase = do
  case decisionCase of
    DT.LiteralCase _ _ cont -> do
      analyzeDecisionTree h cont
    DT.ConsCase (DT.ConsCaseRecord {..}) -> do
      let (es1, ts1) = unzip dataArgs
      cs1 <- concat <$> mapM (analyzeType h) (es1 ++ ts1)
      (cs2, h') <- analyzeBinder h consArgs
      cs3 <- analyzeDecisionTree h' cont
      return $ cs1 ++ cs2 ++ cs3

synthesize :: Handle -> [WeakAffineConstraint] -> App [L.Log]
synthesize h cs = do
  errorList <- concat <$> mapM (simplifyAffine h S.empty) cs
  return $ map constructErrorMessageAffine errorList

newtype AffineConstraintError
  = AffineConstraintError WT.WeakType

constructErrorMessageAffine :: AffineConstraintError -> L.Log
constructErrorMessageAffine (AffineConstraintError t) =
  L.newLog (WT.metaOfType t) L.Error "Variable must be declared with `!` to be copyable"

simplifyAffine ::
  Handle ->
  S.Set DD.DefiniteDescription ->
  WeakAffineConstraint ->
  App [AffineConstraintError]
simplifyAffine h dataNameSet (t, orig@(m :< _)) = do
  t' <- Elaborate.reduceType (elaborateHandle h) t
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
              else mapM (getConsArgTypes h m . (\(name, _, _) -> name)) consNameList
          constraintsFromDataConsArgs <- fmap concat $ forM dataConsArgsList $ \dataConsArgs -> do
            dataConsArgs' <- substConsArgs h IntMap.empty dataConsArgs
            fmap concat $ forM dataConsArgs' $ \(_, _, _, consArg) -> do
              simplifyAffine h dataNameSet' (consArg, orig)
          return $ constraintsFromDataArgs ++ constraintsFromDataConsArgs
        _ -> do
          return [AffineConstraintError orig]
    _ :< WT.Code tCode ->
      simplifyAffine h dataNameSet (tCode, orig)
    _ :< WT.BoxNoema {} ->
      return []
    _ :< WT.PrimType {} -> do
      return []
    _ -> do
      defMap <- liftIO $ WeakTypeDef.read' (Elaborate.weakTypeDefHandle (elaborateHandle h))
      case Stuck.asStuckedType t' of
        Just (Stuck.VarGlobal dd, evalCtx)
          | Just lam <- Map.lookup dd defMap -> do
              mt'' <- liftIO $ Stuck.resume (Elaborate.substHandle (elaborateHandle h)) lam evalCtx
              case mt'' of
                Just t'' -> do
                  simplifyAffine h dataNameSet (t'', orig)
                Nothing -> do
                  return [AffineConstraintError orig]
        _ -> do
          return [AffineConstraintError orig]

substConsArgs :: Handle -> Subst.Subst -> [BinderF WT.WeakType] -> App [BinderF WT.WeakType]
substConsArgs h sub consArgs =
  case consArgs of
    [] ->
      return []
    (m, k, x, t) : rest -> do
      t' <- liftIO $ Subst.substType (Elaborate.substHandle (elaborateHandle h)) sub t
      let opaque = m :< WT.Tau -- allow `a` in `Cons(a: type, x: a)`
      let sub' = IntMap.insert (toInt x) (Type opaque) sub
      rest' <- substConsArgs h sub' rest
      return $ (m, k, x, t') : rest'

getConsArgTypes ::
  Handle ->
  Hint ->
  DD.DefiniteDescription ->
  App [BinderF WT.WeakType]
getConsArgTypes h m consName = do
  t <- Type.lookup' (Elaborate.typeHandle (elaborateHandle h)) m consName
  case t of
    _ :< WT.Pi (PK.DataIntro False) impArgs expArgs defaultArgs (_ :< WT.Pi (PK.Normal _) impArgs' expArgs' defaultArgs' _dataType) -> do
      return $ impArgs ++ expArgs ++ defaultArgs ++ impArgs' ++ expArgs' ++ defaultArgs'
    _ :< WT.Pi (PK.DataIntro True) impArgs expArgs defaultArgs _dataType -> do
      return $ impArgs ++ expArgs ++ defaultArgs
    _ ->
      raiseCritical m $ "Got a malformed constructor type:\n" <> WT.toTextType t

lookupOptimizableData :: Handle -> DD.DefiniteDescription -> IO (Maybe OptimizableData)
lookupOptimizableData h dd = do
  OptimizableData.lookup (Elaborate.optDataHandle (elaborateHandle h)) dd
