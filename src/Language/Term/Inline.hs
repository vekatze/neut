module Language.Term.Inline
  ( new,
    inline,
    inlineType,
  )
where

import App.App (App)
import App.Run (raiseCritical, raiseError)
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class
import Data.Bitraversable (bimapM)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Gensym.Gensym qualified as Gensym
import Kernel.Common.Handle.Local.Tag qualified as Tag
import Kernel.Elaborate.Internal.Handle.TypeDef qualified as TypeDef
import Language.Common.ArgNum qualified as AN
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.Attr.VarGlobal qualified as AttrVG
import Language.Common.BaseLowType qualified as BLT
import Language.Common.Binder
import Language.Common.CallConv qualified as CC
import Language.Common.CodeVariant qualified as CV
import Language.Common.CreateSymbol qualified as Sym
import Language.Common.DecisionTree qualified as DT
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.LamKind qualified as LK
import Language.Common.Literal qualified as L
import Language.Common.LocalDefKind qualified as LDK
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.Common.Opacity qualified as O
import Language.Common.StmtKind qualified as SK
import Language.Common.VarKind qualified as VK
import Language.Term.Eq qualified as TermEq
import Language.Term.FreeVars qualified as TM
import Language.Term.Inline.ConstantFold qualified as ConstantFold
import Language.Term.Inline.Env qualified as Env
import Language.Term.Inline.Handle
import Language.Term.Inline.Magic qualified as Magic
import Language.Term.PrimValue qualified as PV
import Language.Term.Stmt qualified as Stmt
import Language.Term.Subst qualified as Subst
import Language.Term.Term qualified as TM
import Language.Term.Trace qualified as TermTrace
import Language.Term.TraceID
import Language.Term.TraceSites qualified as TraceSites
import Logger.Hint

new :: Env.Env -> Hint -> Bool -> IO Handle
new env location traceEnabled = do
  let Env.Env {..} = env
  let substHandle = Subst.new gensymHandle
  currentStepRef <- liftIO $ newIORef 0
  macroCallStack <- liftIO $ newIORef []
  localMetaDefMap <- liftIO $ newIORef Map.empty
  let initialStage = 0
  let currentStage = initialStage
  let insideDefineMeta = False
  let localMetaMemo = []
  let activeDefineMetaList = []
  let pendingValues = IntMap.empty
  let nextPendingSerial = 0
  usedPendingSerialRef <- liftIO $ newIORef maxBound
  emittedBinders <- liftIO $ newIORef IntSet.empty
  let localTypeEnv = IntMap.empty
  return $ Handle {..}

getTropeDepth :: Handle -> TropeDepth
getTropeDepth h =
  length $ activeDefineMetaList h

withoutPending :: Handle -> Handle
withoutPending h =
  h {pendingValues = IntMap.empty}

withoutPendingFor :: [Ident] -> Handle -> Handle
withoutPendingFor xs h =
  h {pendingValues = foldr (IntMap.delete . Ident.toInt) (pendingValues h) xs}

withNormalized :: [(Ident, Normal)] -> Handle -> Handle
withNormalized xes h = do
  let tropeDepth = getTropeDepth h
  withPending [(Ident.toInt x, Normalized tropeDepth e) | (x, e) <- xes] h

withSelfValue :: LK.LamKindF a -> TM.Term -> Handle -> Handle
withSelfValue lamKind self h =
  case getSelfIdent lamKind of
    Just selfIdent ->
      withNormalized [(selfIdent, asValue self)] h
    Nothing ->
      h

data ParameterSource
  = GivenArgument Normal
  | DefaultValue TM.Term

toDefaultParameter :: (BinderF TM.Type, TM.Term) -> Maybe TM.Term -> (BinderF TM.Type, ParameterSource)
toDefaultParameter (binder, defaultValue) override =
  case override of
    Just e ->
      (binder, GivenArgument (asNormal e))
    Nothing ->
      (binder, DefaultValue defaultValue)

isTakenFromDefinition :: (a, ParameterSource) -> Bool
isTakenFromDefinition (_, source) =
  case source of
    GivenArgument _ ->
      False
    DefaultValue _ ->
      True

hasNonValueSource :: [(a, ParameterSource)] -> Bool
hasNonValueSource params =
  not $ all (\(_, source) -> isValueSource source) params

isValueSource :: ParameterSource -> Bool
isValueSource source =
  case source of
    GivenArgument e ->
      isNormalValue e
    DefaultValue e ->
      TM.isValue e

getGivenTerm :: (a, ParameterSource) -> Maybe TM.Term
getGivenTerm (_, source) =
  case source of
    GivenArgument e ->
      Just $ normalTerm e
    DefaultValue _ ->
      Nothing

withRenamed :: [(Ident, Ident)] -> Handle -> Handle
withRenamed xys =
  withPending [(Ident.toInt x, Renamed y) | (x, y) <- xys]

withTypeSubst :: Subst.Subst -> Handle -> Handle
withTypeSubst sub =
  withPending [(x, TypeValue t) | (x, Subst.Type t) <- IntMap.toList sub]

enterBinder :: Handle -> Ident -> App (Ident, Handle)
enterBinder h x = do
  emitted <- liftIO $ readIORef (emittedBinders h)
  if IntSet.member (Ident.toInt x) emitted
    then do
      x' <- liftIO $ Sym.newIdentFromIdent (gensymHandle h) x
      liftIO $ modifyIORef' (emittedBinders h) (IntSet.insert (Ident.toInt x'))
      return (x', withRenamed [(x, x')] h)
    else do
      liftIO $ modifyIORef' (emittedBinders h) (IntSet.insert (Ident.toInt x))
      return (x, withoutPendingFor [x] h)

enterBinders :: Handle -> [Ident] -> App ([Ident], Handle)
enterBinders h xs =
  case xs of
    [] ->
      return ([], h)
    x : rest -> do
      (x', h') <- enterBinder h x
      (rest', h'') <- enterBinders h' rest
      return (x' : rest', h'')

withPending :: [(Int, PendingTerm)] -> Handle -> Handle
withPending entries h = do
  let start = nextPendingSerial h
  let insertPending (serial, (x, e)) = IntMap.insert x (Pending serial e)
  h
    { pendingValues = foldr insertPending (pendingValues h) (zip [start ..] entries),
      nextPendingSerial = start + length entries
    }

lookupPending :: Handle -> Hint -> Ident -> App (Maybe Normal)
lookupPending h m x =
  case IntMap.lookup (Ident.toInt x) (pendingValues h) of
    Nothing ->
      return Nothing
    Just (Pending {pendingSerial, pendingTerm}) -> do
      case pendingTerm of
        TypeValue (_ :< TM.TVar _) ->
          liftIO $ recordPendingUse h pendingSerial
        TypeValue _ ->
          return ()
        _ ->
          liftIO $ recordPendingUse h pendingSerial
      case pendingTerm of
        Normalized tropeDepth e
          | tropeDepth == getTropeDepth h ->
              return $ Just e
          | otherwise ->
              Just <$> inlineNormal (withoutPending h) (normalTerm e)
        Renamed x' ->
          return $ Just $ asValue $ m :< TM.Var x'
        TypeValue (_ :< TM.TVar x') ->
          return $ Just $ asValue $ m :< TM.Var x'
        TypeValue _ ->
          return Nothing

toSubstEntry :: PendingTerm -> Subst.SubstEntry
toSubstEntry pendingTerm =
  case pendingTerm of
    Normalized _ e ->
      Subst.Term (normalTerm e)
    Renamed x' ->
      Subst.Var x'
    TypeValue t ->
      Subst.Type t

getPendingType :: Hint -> PendingTerm -> Maybe TM.Type
getPendingType m pendingTerm =
  case pendingTerm of
    TypeValue t ->
      Just t
    Renamed x' ->
      Just $ m :< TM.TVar x'
    Normalized _ (Normal {normalTerm = _ :< TM.Var x'}) ->
      Just $ m :< TM.TVar x'
    _ ->
      Nothing

resolveLeafVar :: Handle -> Ident -> IO [Ident]
resolveLeafVar h x =
  case IntMap.lookup (Ident.toInt x) (pendingValues h) of
    Nothing ->
      return [x]
    Just (Pending {pendingSerial, pendingTerm}) -> do
      case pendingTerm of
        Renamed x' -> do
          recordPendingUse h pendingSerial
          return [x']
        TypeValue _ ->
          return [x]
        _ -> do
          recordPendingUse h pendingSerial
          return []

resolveCursorVar :: Handle -> Ident -> IO Ident
resolveCursorVar h x =
  case IntMap.lookup (Ident.toInt x) (pendingValues h) of
    Just (Pending {pendingSerial, pendingTerm = Renamed x'}) -> do
      recordPendingUse h pendingSerial
      return x'
    _ ->
      return x

recordPendingUse :: Handle -> Int -> IO ()
recordPendingUse h serial =
  modifyIORef' (usedPendingSerialRef h) (min serial)

trackPendingUse :: Handle -> App a -> App (a, Bool)
trackPendingUse h action = do
  let ref = usedPendingSerialRef h
  outer <- liftIO $ readIORef ref
  liftIO $ writeIORef ref maxBound
  result <- action
  inner <- liftIO $ readIORef ref
  liftIO $ writeIORef ref (min outer inner)
  return (result, inner < nextPendingSerial h)

applyPending :: Handle -> TM.Term -> IO TM.Term
applyPending h e = do
  if IntMap.null (pendingValues h)
    then return e
    else do
      result <- Subst.substIfChanged (substHandle h) (IntMap.map (toSubstEntry . pendingTerm) (pendingValues h)) e
      case result of
        Nothing ->
          return e
        Just e' -> do
          let free = IntMap.fromSet (const ()) $ IntSet.fromList $ map Ident.toInt $ S.toList $ TM.freeVars e
          let used = IntMap.intersection (pendingValues h) free
          let candidates = if IntMap.null used then pendingValues h else used
          recordPendingUse h $ minimum $ map pendingSerial $ IntMap.elems candidates
          return e'

inline :: Handle -> TM.Term -> App TM.Term
inline h e = do
  liftIO $ recordLocalBinders h
  inline' h e

recordLocalBinders :: Handle -> IO ()
recordLocalBinders h =
  modifyIORef' (emittedBinders h) (IntSet.union (IntMap.keysSet (localTypeEnv h)))

asNormal :: TM.Term -> Normal
asNormal e =
  Normal e (TM.isValue e)

asValue :: TM.Term -> Normal
asValue e =
  Normal e True

inlineType :: Handle -> TM.Type -> App TM.Type
inlineType h t = do
  liftIO $ recordLocalBinders h
  inlineType' h t

incrementStep :: Handle -> IO ()
incrementStep h = do
  let Handle {currentStepRef} = h
  modifyIORef' currentStepRef (+ 1)

isActive :: Handle -> Bool
isActive h =
  currentStage h >= initialStage h

enterOpaqueDefineMetaBody :: Handle -> Handle
enterOpaqueDefineMetaBody h =
  withoutPending $ h {currentStage = 1, initialStage = 1, insideDefineMeta = True, traceEnabled = False}

detectPossibleInfiniteLoop :: Handle -> App ()
detectPossibleInfiniteLoop h = do
  let Handle {inlineLimit, currentStepRef, location} = h
  currentStep <- liftIO $ readIORef currentStepRef
  when (inlineLimit < currentStep) $ do
    raiseError location $ "Exceeded max recursion depth of " <> T.pack (show inlineLimit)

inline' :: Handle -> TM.Term -> App TM.Term
inline' h e =
  normalTerm <$> inlineNormal h e

inlineNormal :: Handle -> TM.Term -> App Normal
inlineNormal h rawTerm = do
  detectPossibleInfiniteLoop h
  case rawTerm of
    m :< TM.Var x ->
      fromMaybe (asValue rawTerm) <$> lookupPending h m x
    _ :< TM.VarGlobal {} ->
      return $ asValue rawTerm
    m :< TM.PiIntro attr@(AttrL.Attr {lamKind}) impArgs expArgs defaultArgs e -> do
      ((lamKind', impArgs', expArgs', defaultArgs', e'), isAltered) <- trackPendingUse h $ do
        (impArgs', hImp) <- inlineTypeBinders h impArgs
        (expArgs', hExp) <- inlineTypeBinders hImp expArgs
        (defaultArgs', hDefault) <- inlineLetSeq hExp defaultArgs
        (lamKindSelf, hSelf) <- enterSelf hDefault lamKind
        let hBody = withBinders (impArgs' ++ expArgs' ++ map fst defaultArgs') hSelf
        e' <- if currentStage h >= 1 then liftIO (applyPending hBody e) else inline' hBody e
        lamKind' <- case lamKindSelf of
          LK.Fix kind isDestPassing (mx, k, x, codType) -> do
            codType' <- inlineType' hDefault codType
            return $ LK.Fix kind isDestPassing (mx, k, x, codType')
          LK.Normal mName isDestPassing codType -> do
            codType' <- inlineType' hDefault codType
            return $ LK.Normal mName isDestPassing codType'
        return (lamKind', impArgs', expArgs', defaultArgs', e')
      identity <-
        if isAltered
          then liftIO $ Gensym.newCount (gensymHandle h)
          else return $ AttrL.identity attr
      return $ asValue $ m :< TM.PiIntro (AttrL.Attr {lamKind = lamKind', identity}) impArgs' expArgs' defaultArgs' e'
    m :< TM.PiElim callTraceID conv e impArgs expArgs defaultArgs -> do
      conv' <- CC.traverseTypes (inlineType' h) conv
      e' <- inline' h e
      impArgs' <- mapM (inlineType' h) impArgs
      expArgsNormal <- mapM (inlineNormal h) expArgs
      let expArgs' = map normalTerm expArgsNormal
      defaultArgs' <- mapM (traverse (inline' h)) defaultArgs
      let Handle {dmap} = h
      localDefMap <- liftIO $ readIORef (localMetaDefMap h)
      let residual = m :< TM.PiElim callTraceID conv' e' impArgs' expArgs' defaultArgs'
      let reduceApplication mDefaultCallee impParams expBinders defDefaults canReduce reduce =
            if length impParams /= length impArgs' || length expBinders /= length expArgs' || length defDefaults /= length defaultArgs'
              then registerResidual h residual
              else do
                let impIds = map (\(_, _, x, _) -> x) impParams
                let subType = IntMap.fromList $ zip (map Ident.toInt impIds) (map Subst.Type impArgs')
                let defaultFrameCtx = (\dd -> (dd, m, callTraceID)) <$> mDefaultCallee
                let givenParams = zip expBinders (map GivenArgument expArgsNormal)
                let defaultParams = zipWith toDefaultParameter defDefaults defaultArgs'
                let params = givenParams ++ defaultParams
                if CC.isNoetic conv' || not canReduce
                  then
                    if not (any isTakenFromDefinition defaultParams) || (hasNonValueSource params && not (isActive h))
                      then registerResidual h residual
                      else do
                        bindParameters h subType defaultFrameCtx params $ \values _ -> do
                          let (expValues, defaultValues) = splitAt (length expArgs') (map normalTerm values)
                          registerResidual h $ m :< TM.PiElim callTraceID conv' e' impArgs' expValues (map Just defaultValues)
                  else reduce subType defaultFrameCtx params
      case e' of
        (_ :< TM.PiIntro attr@(AttrL.Attr {lamKind, identity}) impBinders expBinders defBinders body) -> do
          let canReduce = isLocalDefineMeta lamKind || canReduceByLamKind lamKind
          reduceApplication Nothing impBinders expBinders defBinders canReduce $ \subType _ params ->
            if isLocalDefineMeta lamKind
              then do
                case mapM getGivenTerm params of
                  Just args -> do
                    (g, defInfo) <- registerLocalMeta h m identity attr impBinders expBinders defBinders body
                    asNormal <$> specializeLocalMeta h m g subType impArgs' (map fst params) (defBody defInfo) (codType defInfo) args
                  Nothing ->
                    registerResidual h residual
              else do
                if hasNonValueSource params && not (isActive h)
                  then registerResidual h residual
                  else do
                    let paramIds = map (\((_, _, x, _), _) -> x) params
                    bindParameters h subType Nothing params $ \values letBinders ->
                      expand (withBinders letBinders $ withNormalized (zip paramIds values) $ withSelfValue lamKind e' $ withTypeSubst subType $ withoutPending h) body
        (mUse :< TM.VarGlobal _ dd)
          | Just defInfo <- Map.lookup dd dmap -> do
              let DefInfo {defImpBinders, defExpBinders, defDefaultArgs, defBody, codType, defKind, traceSiteIDs} = defInfo
              let canReduce = canInlineDefKind defKind
              reduceApplication (Just dd) defImpBinders defExpBinders defDefaultArgs canReduce $ \subType defaultFrameCtx params ->
                case defKind of
                  Macro -> do
                    case mapM getGivenTerm params of
                      Just args -> do
                        Normal {normalTerm = _ :< tracedCall} <- registerResidual h residual
                        let specializedCallTraceID = TraceSites.traceIDOf tracedCall
                        asNormal <$> specializeMacro h m mUse specializedCallTraceID dd defKind subType impArgs' (map fst params) defBody codType args
                      Nothing ->
                        registerResidual h residual
                  _ -> do
                    let tracer = if isMacroDef defKind then withMacroHint h dd defKind m else id
                    if hasNonValueSource params && not (isActive h)
                      then registerResidual h residual
                      else do
                        remapTrace <- prepareDefinitionTraceRemapping h dd defKind traceSiteIDs m callTraceID
                        (expParams', body') <- instantiateDefinition h remapTrace subType (map fst params) defBody
                        let paramIds = map (\(_, _, x, _) -> x) expParams'
                        tracer $ bindParameters h subType defaultFrameCtx params $ \values letBinders ->
                          expand (withBinders letBinders $ withNormalized (zip paramIds values) $ withoutPending h) body'
          | Just defInfo <- Map.lookup dd localDefMap -> do
              let DefInfo {defImpBinders, defExpBinders, defDefaultArgs, defBody, codType} = defInfo
              reduceApplication Nothing defImpBinders defExpBinders defDefaultArgs True $ \subType _ params ->
                case mapM getGivenTerm params of
                  Just args ->
                    asNormal <$> specializeLocalMeta h m dd subType impArgs' (map fst params) defBody codType args
                  Nothing ->
                    registerResidual h residual
        (_ :< TM.Prim (PV.Op op))
          | CC.isNormal conv' -> do
              case ConstantFold.evaluatePrimOp m op expArgs' of
                Just result -> do
                  return $ asNormal result
                Nothing ->
                  registerResidual h residual
        _ ->
          registerResidual h residual
    m :< TM.DataIntro attr consName dataArgs consImpArgs consArgs -> do
      dataArgs' <- mapM (inlineType' h) dataArgs
      consImpArgs' <- mapM (inlineType' h) consImpArgs
      consArgs' <- mapM (inlineNormal h) consArgs
      let e' = m :< TM.DataIntro attr consName dataArgs' consImpArgs' (map normalTerm consArgs')
      return $ Normal e' (all isNormalValue consArgs')
    m :< TM.DataElim traceID isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (inlineNormal h) es
      ts' <- mapM (inlineType' h) ts
      reduceDataElim h m traceID isNoetic (zip3 os es' ts') decisionTree
    m :< TM.BoxIntro traceID letSeq e -> do
      (letSeq', hLet) <- inlineLetSeq h letSeq
      e' <- inline' (withBinders (map fst letSeq') hLet) e
      registerResidual h $ m :< TM.BoxIntro traceID letSeq' e'
    m :< TM.BoxIntroLift t e -> do
      t' <- inlineType' h t
      e' <- inlineNormal h e
      return $ Normal (m :< TM.BoxIntroLift t' (normalTerm e')) (isNormalValue e')
    m :< TM.EmbedIntro e -> do
      e' <- inline' h e
      return $ asValue $ m :< TM.EmbedIntro e'
    m :< TM.BoxElim traceID castSeq mxt e1 uncastSeq e2 -> do
      (castSeq', hCastSeq) <- inlineLetSeq h castSeq
      let hCast = withBinders (map fst castSeq') hCastSeq
      ((mx, k, x, t), e1') <- bimapM (inlineTypeBinder hCast) (inline' hCast) (mxt, e1)
      (x', hMxt) <- enterBinder hCast x
      let mxt' = (mx, k, x', t)
      let hCont = withBinders [mxt'] hMxt
      (uncastSeq', hUncast) <- inlineLetSeq hCont uncastSeq
      e2' <- inline' (withBinders (map fst uncastSeq') hUncast) e2
      registerResidual h $ m :< TM.BoxElim traceID castSeq' mxt' e1' uncastSeq' e2'
    m :< TM.CodeIntro codeVariant e -> do
      let hInner = h {currentStage = currentStage h - CV.getStageOffset codeVariant}
      e' <- inline' hInner e
      return $ asValue $ m :< TM.CodeIntro codeVariant e'
    m :< TM.CodeElim traceID e -> do
      let hInner = h {currentStage = currentStage h + 1}
      e' <- inline' hInner e
      case e' of
        _ :< TM.CodeIntro _ e'' -> do
          liftIO $ incrementStep h
          return $ asNormal e''
        _ ->
          registerResidual h $ m :< TM.CodeElim traceID e'
    m :< TM.TauIntro ty -> do
      ty' <- inlineType' h ty
      return $ asValue $ m :< TM.TauIntro ty'
    m :< TM.TauElim traceID (mx, k, x) e1 e2 -> do
      e1' <- inline' h e1
      case e1' of
        _ :< TM.TauIntro ty -> do
          expand (withPending [(Ident.toInt x, TypeValue ty)] h) e2
        _ -> do
          (x', h') <- enterBinder h x
          e2' <- inline' h' e2
          registerResidual h $ m :< TM.TauElim traceID (mx, k, x') e1' e2'
    m :< TM.Let mxt@(_, _, x, _) e1 e2 -> do
      e1' <- inlineNormal h e1
      if isNormalValue e1'
        then expand (withNormalized [(x, e1')] h) e2
        else do
          (mx, k, _, t) <- inlineTypeBinder h mxt
          (x', hBody) <- enterBinder h x
          let mxt' = (mx, k, x', t)
          e2' <- inline' (withBinders [mxt'] hBody) e2
          return $ asNormal $ m :< TM.Let mxt' (normalTerm e1') e2'
    m :< TM.Invoke tropeNames body -> do
      defineMetaList <- activateTropes h m tropeNames
      inlineNormal (h {activeDefineMetaList = defineMetaList ++ activeDefineMetaList h}) body
    m :< TM.Prim prim -> do
      case prim of
        PV.Int intType size value -> do
          intType' <- inlineType' (withoutPending h) intType
          return $ asValue $ m :< TM.Prim (PV.Int intType' size value)
        PV.Float floatType size value -> do
          floatType' <- inlineType' (withoutPending h) floatType
          return $ asValue $ m :< TM.Prim (PV.Float floatType' size value)
        PV.Op {} ->
          return $ asValue rawTerm
        PV.NoeticString stringType text -> do
          stringType' <- inlineType' (withoutPending h) stringType
          return $ asValue $ m :< TM.Prim (PV.NoeticString stringType' text)
        PV.NoeticBinary binaryType bytes -> do
          binaryType' <- inlineType' (withoutPending h) binaryType
          return $ asValue $ m :< TM.Prim (PV.NoeticBinary binaryType' bytes)
        PV.Text text ->
          return $ asValue $ m :< TM.Prim (PV.Text text)
        PV.Blob bytes ->
          return $ asValue $ m :< TM.Prim (PV.Blob bytes)
        PV.Rune {} ->
          return $ asValue rawTerm
    m :< TM.Magic traceID magic -> do
      case magic of
        M.LowMagic lowMagic -> do
          case lowMagic of
            LM.Cast _ _ e ->
              inlineNormal h e
            _ -> do
              lowMagic' <- inlineLowMagic h lowMagic
              registerResidual h (m :< TM.Magic traceID (M.LowMagic lowMagic'))
        M.Calloc sizeType num size -> do
          sizeType' <- inlineType' h sizeType
          num' <- inline' h num
          size' <- inline' h size
          registerResidual h (m :< TM.Magic traceID (M.Calloc sizeType' num' size'))
        M.Malloc sizeType size -> do
          sizeType' <- inlineType' h sizeType
          size' <- inline' h size
          registerResidual h (m :< TM.Magic traceID (M.Malloc sizeType' size'))
        M.MallocOpaque sizeType size -> do
          sizeType' <- inlineType' h sizeType
          size' <- inline' h size
          registerResidual h (m :< TM.Magic traceID (M.MallocOpaque sizeType' size'))
        M.Realloc sizeType ptr size -> do
          sizeType' <- inlineType' h sizeType
          ptr' <- inline' h ptr
          size' <- inline' h size
          registerResidual h (m :< TM.Magic traceID (M.Realloc sizeType' ptr' size'))
        M.Free unitType ptr -> do
          unitType' <- inlineType' h unitType
          ptr' <- inline' h ptr
          registerResidual h (m :< TM.Magic traceID (M.Free unitType' ptr'))
        M.InspectType mid _ typeExpr -> do
          typeExpr' <- inlineType' h typeExpr
          Magic.evaluateInspectType h m mid typeExpr' >>= expand (withoutPending h)
        M.EqType moduleID typeExpr1 typeExpr2 -> do
          typeExpr1' <- inlineType' h typeExpr1
          typeExpr2' <- inlineType' h typeExpr2
          asNormal <$> Magic.evaluateEqType m moduleID typeExpr1' typeExpr2'
        M.ShowType typeExpr -> do
          typeExpr' <- inlineType' h typeExpr
          asNormal <$> Magic.evaluateShowType m typeExpr'
        M.TextCons rune text -> do
          rune' <- inline' h rune
          text' <- inline' h text
          asNormal <$> Magic.evaluateTextCons h m rune' text'
        M.TextUncons mid text -> do
          text' <- inline' h text
          Magic.evaluateTextUncons h m mid text' >>= expand (withoutPending h)
        M.MakeSwitch mid key fallback clauses -> do
          key' <- inline' h key
          fallback' <- inline' h fallback
          clauses' <- inline' h clauses
          Magic.evaluateMakeSwitch h m mid key' fallback' clauses' >>= expand (withoutPending h)
        M.FreeVariables mid code -> do
          code' <- inline' h code
          Magic.evaluateFreeVariables h (inlineType' (withoutPending h)) m mid code' >>= expand (withoutPending h)
        M.CompileError msg -> do
          msg' <- inline' h msg
          asNormal <$> Magic.evaluateCompileError h m msg'
        M.GetOriginFileName -> do
          asNormal <$> Magic.evaluateGetOriginFileName h m
        M.GetOriginLine -> do
          asNormal <$> Magic.evaluateGetOriginLine h m
        M.GetOriginColumn -> do
          asNormal <$> Magic.evaluateGetOriginColumn h m

reduceDataElim ::
  Handle ->
  Hint ->
  TraceID ->
  Bool ->
  [(Ident, Normal, TM.Type)] ->
  DT.DecisionTree TM.Type TM.Term ->
  App Normal
reduceDataElim h m traceID isNoetic oets' decisionTree = do
  let (os, es', ts') = unzip3 oets'
  let residualOets = zip3 os (map normalTerm es') ts'
  let residualTree = do
        (os', hOs) <- enterBinders h os
        decisionTree' <- inlineDecisionTree (withTypedIdents (zip os' ts') hOs) decisionTree
        registerResidual h $ m :< TM.DataElim traceID isNoetic (zip3 os' (map normalTerm es') ts') decisionTree'
  if isNoetic
    then residualTree
    else do
      case decisionTree of
        DT.Leaf _ letSeq e -> do
          expand (withNormalized (zip os es') h) (TM.fromLetSeq letSeq e)
        DT.Unreachable ->
          registerResidual h $ m :< TM.DataElim traceID isNoetic residualOets DT.Unreachable
        DT.Switch (cursor, _) (fallbackTree, caseList) -> do
          case lookupSplit cursor oets' of
            Just (e, oets'')
              | _ :< TM.DataIntro (AttrDI.Attr {..}) _ _ consImpTypes consArgs <- normalTerm e -> do
              let (consImpVars, newBaseCursorList, cont) = findClause discriminant fallbackTree caseList
              let hTyped = withPending [(Ident.toInt x, TypeValue t) | (x, t) <- zip consImpVars consImpTypes] h
              newCursorTypes <- mapM (inlineType' hTyped . snd) newBaseCursorList
              let newCursorList = zip3 (map fst newBaseCursorList) (map (asComponent e) consArgs) newCursorTypes
              continueDataElim (withNormalized [(cursor, e)] hTyped) m traceID isNoetic (oets'' ++ newCursorList) cont
            Just (e, oets'')
              | Just literal <- asLiteralTerm (normalTerm e) -> do
                  case findLiteralClause literal caseList of
                    Just cont -> do
                      continueDataElim (withNormalized [(cursor, e)] h) m traceID isNoetic oets'' cont
                    Nothing
                      | L.Int value <- literal,
                        Just ([], cont) <- findConsCaseByDisc (D.MakeDiscriminant value) caseList -> do
                          continueDataElim (withNormalized [(cursor, e)] h) m traceID isNoetic oets'' cont
                      | otherwise ->
                          residualTree
            _ ->
              residualTree

continueDataElim ::
  Handle ->
  Hint ->
  TraceID ->
  Bool ->
  [(Ident, Normal, TM.Type)] ->
  DT.DecisionTree TM.Type TM.Term ->
  App Normal
continueDataElim h m traceID isNoetic oets' decisionTree = do
  liftIO $ incrementStep h
  detectPossibleInfiniteLoop h
  reduceDataElim h m traceID isNoetic oets' decisionTree

asComponent :: Normal -> TM.Term -> Normal
asComponent parent e =
  Normal e (isNormalValue parent || TM.isValue e)

expand :: Handle -> TM.Term -> App Normal
expand h term = do
  liftIO $ incrementStep h
  inlineNormal h term

registerResidual :: Handle -> TM.Term -> App Normal
registerResidual h term =
  if traceEnabled h
    then asNormal <$> liftIO (TraceSites.annotateRoot (traceHandle h) term)
    else return $ asNormal term

inlineType' :: Handle -> TM.Type -> App TM.Type
inlineType' h ty = do
  detectPossibleInfiniteLoop h
  case ty of
    _ :< TM.Tau ->
      return ty
    m :< TM.TVar x ->
      case IntMap.lookup (Ident.toInt x) (pendingValues h) of
        Just (Pending {pendingSerial, pendingTerm})
          | Just ty' <- getPendingType m pendingTerm -> do
              liftIO $ recordPendingUse h pendingSerial
              return ty'
        _ ->
          return ty
    _ :< TM.TVarGlobal {} ->
      return ty
    m :< TM.TyApp t args -> do
      t' <- inlineType' h t
      case t' of
        _ :< TM.TVarGlobal _ dd
          | Just typeDefInfo <- Map.lookup dd (typeDefMap h),
            TypeDef.TypeDefInfo {TypeDef.typeDefBinders = binders, TypeDef.typeDefBody = body} <- typeDefInfo,
            length binders == length args -> do
              args' <- mapM (inlineType' h) args
              let binderIds = map (\(_, _, x, _) -> x) binders
              let subType = IntMap.fromList $ zip (map Ident.toInt binderIds) (map Subst.Type args')
              body' <- liftIO $ Subst.substType (substHandle h) subType body
              liftIO $ incrementStep h
              inlineType' (withoutPending h) body'
        _ -> do
          args' <- mapM (inlineType' h) args
          return $ m :< TM.TyApp t' args'
    m :< TM.Pi piKind impArgs expArgs defaultArgs cod -> do
      (impArgs', h1) <- inlineTypeBinders h impArgs
      (expArgs', h2) <- inlineTypeBinders h1 expArgs
      (defaultArgs', h3) <- inlineTypeBinders h2 defaultArgs
      cod' <- inlineType' h3 cod
      return $ m :< TM.Pi piKind impArgs' expArgs' defaultArgs' cod'
    m :< TM.Data attr name es -> do
      es' <- mapM (inlineType' h) es
      return $ m :< TM.Data attr name es'
    m :< TM.Box t -> do
      t' <- inlineType' h t
      return $ m :< TM.Box t'
    m :< TM.BoxNoema t -> do
      t' <- inlineType' h t
      return $ m :< TM.BoxNoema t'
    m :< TM.Embed t -> do
      t' <- inlineType' h t
      return $ m :< TM.Embed t'
    m :< TM.Code t -> do
      t' <- inlineType' h t
      return $ m :< TM.Code t'
    _ :< TM.PrimType {} ->
      return ty
    _ :< TM.Void ->
      return ty
    m :< TM.Resource dd resourceID -> do
      return $ m :< TM.Resource dd resourceID

inlineLowMagic :: Handle -> LM.LowMagic BLT.BaseLowType TM.Type TM.Term -> App (LM.LowMagic BLT.BaseLowType TM.Type TM.Term)
inlineLowMagic h lowMagic =
  case lowMagic of
    LM.Cast from to value -> do
      from' <- inlineType' h from
      to' <- inlineType' h to
      value' <- inline' h value
      return $ LM.Cast from' to' value'
    LM.Store t unit value pointer -> do
      unit' <- inlineType' (withoutPending h) unit
      value' <- inline' h value
      pointer' <- inline' h pointer
      return $ LM.Store t unit' value' pointer'
    LM.Load t pointer -> do
      pointer' <- inline' h pointer
      return $ LM.Load t pointer'
    LM.Shift pointer offset -> do
      pointer' <- inline' h pointer
      offset' <- inline' h offset
      return $ LM.Shift pointer' offset'
    LM.Alloca t size -> do
      size' <- inline' h size
      return $ LM.Alloca t size'
    LM.External domList cod extFunName args varArgs -> do
      args' <- mapM (inline' h) args
      varArgs' <- mapM (bimapM (inline' h) pure) varArgs
      return $ LM.External domList cod extFunName args' varArgs'
    LM.Global name t ->
      return $ LM.Global name t
    LM.OpaqueValue e -> do
      e' <- inline' h e
      return $ LM.OpaqueValue e'
    LM.CallType func arg1 arg2 arg3 -> do
      func' <- inline' h func
      arg1' <- inline' h arg1
      arg2' <- inline' h arg2
      arg3' <- inline' h arg3
      return $ LM.CallType func' arg1' arg2' arg3'

withBinders :: [BinderF TM.Type] -> Handle -> Handle
withBinders xts =
  withTypedIdents (map (\(_, _, x, t) -> (x, t)) xts)

withTypedIdents :: [(Ident, TM.Type)] -> Handle -> Handle
withTypedIdents xts h =
  h
    { localTypeEnv = foldr (\(x, t) -> IntMap.insert (Ident.toInt x) t) (localTypeEnv h) xts,
      pendingValues = pendingValues (withoutPendingFor (map fst xts) h)
    }

withCursorType :: Ident -> TM.Type -> Handle -> Handle
withCursorType x t h =
  h {localTypeEnv = IntMap.insert (Ident.toInt x) t (localTypeEnv h)}

enterSelf :: Handle -> LK.LamKindF a -> App (LK.LamKindF a, Handle)
enterSelf h lamKind =
  case lamKind of
    LK.Fix kind isDestPassing (mx, k, x, codType) -> do
      (x', h') <- enterBinder h x
      return (LK.Fix kind isDestPassing (mx, k, x', codType), h')
    LK.Normal {} ->
      return (lamKind, h)

inlineLetSeq :: Handle -> [(BinderF TM.Type, TM.Term)] -> App ([(BinderF TM.Type, TM.Term)], Handle)
inlineLetSeq h letSeq =
  case letSeq of
    [] ->
      return ([], h)
    (binder, e) : rest -> do
      (mx, k, x, t) <- inlineTypeBinder h binder
      e' <- inline' h e
      (x', h') <- enterBinder h x
      (rest', h'') <- inlineLetSeq h' rest
      return (((mx, k, x', t), e') : rest', h'')

inlineTypeBinders :: Handle -> [BinderF TM.Type] -> App ([BinderF TM.Type], Handle)
inlineTypeBinders h binders =
  case binders of
    [] ->
      return ([], h)
    binder : rest -> do
      (mx, k, x, t) <- inlineTypeBinder h binder
      (x', h') <- enterBinder h x
      (rest', h'') <- inlineTypeBinders h' rest
      return ((mx, k, x', t) : rest', h'')

inlineTypeBinder :: Handle -> BinderF TM.Type -> App (BinderF TM.Type)
inlineTypeBinder h (m, k, x, t) = do
  t' <- inlineType' h t
  return (m, k, x, t')

inlineDecisionTree ::
  Handle ->
  DT.DecisionTree TM.Type TM.Term ->
  App (DT.DecisionTree TM.Type TM.Term)
inlineDecisionTree h tree =
  case tree of
    DT.Leaf xs letSeq e -> do
      xs' <- liftIO $ concat <$> mapM (resolveLeafVar h) xs
      (letSeq', hLet) <- inlineLetSeq h letSeq
      e' <- inline' (withBinders (map fst letSeq') hLet) e
      return $ DT.Leaf xs' letSeq' e'
    DT.Unreachable ->
      return DT.Unreachable
    DT.Switch (cursorVar, cursor) clauseList -> do
      cursorVar' <- liftIO $ resolveCursorVar h cursorVar
      cursor' <- inlineType' h cursor
      clauseList' <- inlineCaseList (withCursorType cursorVar' cursor' h) clauseList
      return $ DT.Switch (cursorVar', cursor') clauseList'

inlineCaseList ::
  Handle ->
  DT.CaseList TM.Type TM.Term ->
  App (DT.CaseList TM.Type TM.Term)
inlineCaseList h (fallbackTree, clauseList) = do
  fallbackTree' <- inlineDecisionTree h fallbackTree
  clauseList' <- mapM (inlineCase h) clauseList
  return (fallbackTree', clauseList')

inlineCase ::
  Handle ->
  DT.Case TM.Type TM.Term ->
  App (DT.Case TM.Type TM.Term)
inlineCase h decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      cont' <- inlineDecisionTree h cont
      return $ DT.LiteralCase mPat i cont'
    DT.ConsCase record@(DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      dataTerms' <- mapM (inlineType' h) dataTerms
      dataTypes' <- mapM (inlineType' h) dataTypes
      (consImpArgs', hImp) <- inlineTypeBinders h consImpArgs
      (consArgs', hArgs) <- inlineTypeBinders hImp consArgs
      cont' <- inlineDecisionTree (withBinders consArgs' hArgs) cont
      return $
        DT.ConsCase
          record
            { DT.dataArgs = zip dataTerms' dataTypes',
              DT.consImpArgs = consImpArgs',
              DT.consArgs = consArgs',
              DT.cont = cont'
            }

findClause ::
  D.Discriminant ->
  DT.DecisionTree TM.Type TM.Term ->
  [DT.Case TM.Type TM.Term] ->
  ([Ident], [(Ident, TM.Type)], DT.DecisionTree TM.Type TM.Term)
findClause consDisc fallbackTree clauseList =
  case clauseList of
    [] ->
      ([], [], fallbackTree)
    clause : rest ->
      case DT.findCase consDisc clause of
        Just (consImpVars, consArgs, clauseTree) ->
          (consImpVars, consArgs, clauseTree)
        Nothing ->
          findClause consDisc fallbackTree rest

findLiteralClause ::
  L.Literal ->
  [DT.Case TM.Type TM.Term] ->
  Maybe (DT.DecisionTree TM.Type TM.Term)
findLiteralClause literal clauseList =
  case clauseList of
    [] ->
      Nothing
    clause : rest ->
      case clause of
        DT.LiteralCase _ literal' cont
          | literal == literal' ->
              Just cont
        _ ->
          findLiteralClause literal rest

findConsCaseByDisc ::
  D.Discriminant ->
  [DT.Case TM.Type TM.Term] ->
  Maybe ([BinderF TM.Type], DT.DecisionTree TM.Type TM.Term)
findConsCaseByDisc disc clauseList =
  case clauseList of
    [] ->
      Nothing
    clause : rest ->
      case clause of
        DT.ConsCase (DT.ConsCaseRecord {disc = clauseDisc, consArgs, cont})
          | disc == clauseDisc ->
              Just (consArgs, cont)
        _ ->
          findConsCaseByDisc disc rest

asLiteralTerm :: TM.Term -> Maybe L.Literal
asLiteralTerm term =
  case term of
    _ :< TM.Prim (PV.Int _ _ value) ->
      Just (L.Int value)
    _ :< TM.Prim (PV.Rune rune) ->
      Just (L.Rune rune)
    _ ->
      Nothing

lookupSplit :: Ident -> [(Ident, b, c)] -> Maybe (b, [(Ident, b, c)])
lookupSplit cursor =
  lookupSplit' cursor []

lookupSplit' :: Ident -> [(Ident, b, c)] -> [(Ident, b, c)] -> Maybe (b, [(Ident, b, c)])
lookupSplit' cursor acc oets =
  case oets of
    [] ->
      Nothing
    oet@(o, e, _) : rest ->
      if o == cursor
        then Just (e, reverse acc ++ rest)
        else lookupSplit' cursor (oet : acc) rest

type DefaultFrameContext = (DD.DefiniteDescription, Hint, TraceID)

bindParameters ::
  Handle ->
  Subst.Subst ->
  Maybe DefaultFrameContext ->
  [(BinderF TM.Type, ParameterSource)] ->
  ([Normal] -> [BinderF TM.Type] -> App Normal) ->
  App Normal
bindParameters h subType defaultFrameCtx params cont =
  bindParameters' h (withoutPending h) subType defaultFrameCtx params [] [] cont

bindParameters' ::
  Handle ->
  Handle ->
  Subst.Subst ->
  Maybe DefaultFrameContext ->
  [(BinderF TM.Type, ParameterSource)] ->
  [Normal] ->
  [BinderF TM.Type] ->
  ([Normal] -> [BinderF TM.Type] -> App Normal) ->
  App Normal
bindParameters' h hDefault subType defaultFrameCtx params values letBinders cont =
  case params of
    [] ->
      cont (reverse values) (reverse letBinders)
    ((mx, k, x, t), source) : rest -> do
      e <- case source of
        GivenArgument e ->
          return e
        DefaultValue defaultValue -> do
          defaultValue' <- liftIO $ refreshDefaultValue h defaultFrameCtx subType defaultValue
          inlineNormal hDefault defaultValue'
      if isNormalValue e
        then bindParameters' h (withNormalized [(x, e)] hDefault) subType defaultFrameCtx rest (e : values) letBinders cont
        else do
          t' <- liftIO (Subst.substType (substHandle h) subType t) >>= inlineType' (withoutPending h)
          v <- liftIO $ Sym.newIdentFromIdent (gensymHandle h) x
          liftIO $ modifyIORef' (emittedBinders h) (IntSet.insert (Ident.toInt v))
          let binder = (mx, k, v, t')
          let value = asValue $ mx :< TM.Var v
          let hDefault' = withBinders [binder] $ withNormalized [(x, value)] hDefault
          cont' <- bindParameters' h hDefault' subType defaultFrameCtx rest (value : values) (binder : letBinders) cont
          return $ asNormal $ mx :< TM.Let binder (normalTerm e) (normalTerm cont')

refreshDefaultValue :: Handle -> Maybe DefaultFrameContext -> Subst.Subst -> TM.Term -> IO TM.Term
refreshDefaultValue h defaultFrameCtx subType defaultValue =
  case defaultFrameCtx of
    Just (dd, callHint, callTraceID)
      | traceEnabled h -> do
          (annotated, siteIDs) <- TraceSites.annotate (traceHandle h) defaultValue
          if IntSet.null siteIDs
            then snd <$> Subst.substAndRefresh' (substHandle h) subType [] annotated
            else do
              remapping <- TermTrace.registerExpansion (traceHandle h) dd callHint callTraceID TermTrace.DefaultArgFrame siteIDs
              Subst.substAndRefreshWithTrace (substHandle h) remapping subType annotated
    _ ->
      snd <$> Subst.substAndRefresh' (substHandle h) subType [] defaultValue

prepareDefinitionTraceRemapping ::
  Handle ->
  DD.DefiniteDescription ->
  DefKind ->
  IntSet.IntSet ->
  Hint ->
  TraceID ->
  App (Maybe TermTrace.Remapping)
prepareDefinitionTraceRemapping h dd defKind traceSiteIDs callHint callTraceID = do
  if not (traceEnabled h) || IntSet.null traceSiteIDs
    then return Nothing
    else do
      remapping <- liftIO $ TermTrace.registerExpansion (traceHandle h) dd callHint callTraceID (toFrameKind defKind) traceSiteIDs
      return $ Just remapping

instantiateDefinition ::
  Handle ->
  Maybe TermTrace.Remapping ->
  Subst.Subst ->
  [BinderF TM.Type] ->
  TM.Term ->
  App ([BinderF TM.Type], TM.Term)
instantiateDefinition h remapTrace sub binders body = do
  case remapTrace of
    Nothing ->
      liftIO $ Subst.substAndRefresh' (substHandle h) sub binders body
    Just remapping -> do
      liftIO $ Subst.substAndRefreshWithTrace' (substHandle h) remapping sub binders body

toFrameKind :: DefKind -> TermTrace.FrameKind
toFrameKind defKind = do
  case defKind of
    NoInline ->
      TermTrace.NoInlineFrame
    Inline ->
      TermTrace.InlineFrame
    Macro ->
      TermTrace.MacroFrame
    MacroInline ->
      TermTrace.MacroInlineFrame
    ConstantMeta ->
      TermTrace.ConstantMetaFrame
    DataIntro ->
      TermTrace.DataIntroFrame

beginSpecialization :: Handle -> DD.DefiniteDescription -> [TM.Type] -> DD.DefiniteDescription -> App ()
beginSpecialization h dd typeArgs specializedName = do
  let entry = SpecializationEntry typeArgs specializedName
  let Handle {specializationTable} = h
  liftIO $ modifyIORef' specializationTable $ Map.insertWith (<>) dd [entry]

completeSpecialization :: Handle -> Stmt.Stmt -> App ()
completeSpecialization h stmt = do
  let Handle {pendingSpecializationDefs} = h
  liftIO $ modifyIORef' pendingSpecializationDefs (stmt :)

lookupSpecialization :: Handle -> DD.DefiniteDescription -> [TM.Type] -> App (Maybe DD.DefiniteDescription)
lookupSpecialization h dd typeArgs = do
  let Handle {specializationTable} = h
  table <- liftIO $ readIORef specializationTable
  return $ specializationName <$> lookupSpecializationEntry typeArgs (Map.lookup dd table)

lookupSpecializationEntry :: [TM.Type] -> Maybe [SpecializationEntry] -> Maybe SpecializationEntry
lookupSpecializationEntry typeArgs =
  go . fromMaybe []
  where
    go entries =
      case entries of
        [] ->
          Nothing
        entry : rest
          | TermEq.eqTypes (specializationTypeArgs entry) typeArgs ->
              Just entry
          | otherwise ->
              go rest

lookupActiveDefineMeta :: Handle -> DD.DefiniteDescription -> [TM.Type] -> App (Maybe Stmt.DefineMeta)
lookupActiveDefineMeta h targetName targetArgs = do
  let go defineMetaList =
        case defineMetaList of
          [] ->
            Nothing
          defineMeta : rest
            | Stmt.defineMetaTargetName defineMeta == targetName,
              TermEq.eqTypes (Stmt.defineMetaTargetArgs defineMeta) targetArgs ->
                Just defineMeta
            | otherwise ->
                go rest
  return $ go $ activeDefineMetaList h

retargetTag :: Handle -> Hint -> Stmt.DefineMeta -> App ()
retargetTag h mUse activeDefineMeta = do
  let SavedHint mDef = Stmt.defineMetaLoc activeDefineMeta
  liftIO $ Tag.retarget (tagHandle h) mUse mDef

activateTropes :: Handle -> Hint -> [DD.DefiniteDescription] -> App [Stmt.DefineMeta]
activateTropes h m tropeNames = do
  defineMetaLists <- mapM (lookupTrope h m) tropeNames
  return $ reverse $ concat defineMetaLists

lookupTrope :: Handle -> Hint -> DD.DefiniteDescription -> App [Stmt.DefineMeta]
lookupTrope h m tropeName = do
  case Map.lookup tropeName (tropeMap h) of
    Just defineMetaList ->
      return defineMetaList
    Nothing ->
      raiseCritical m $ "Unknown trope: `" <> DD.reify tropeName <> "`"

createSpecializationName :: Handle -> DD.DefiniteDescription -> App DD.DefiniteDescription
createSpecializationName h dd = do
  identity <- liftIO $ Gensym.newCount (gensymHandle h)
  return $ DD.getKnotDD dd identity

applySpecialization :: Hint -> TraceID -> DD.DefiniteDescription -> [TM.Term] -> TM.Term
applySpecialization m traceID specializedName expArgsAll = do
  let attr = AttrVG.new (AN.fromInt $ length expArgsAll)
  m :< TM.PiElim traceID CC.normal (m :< TM.VarGlobal attr specializedName) [] expArgsAll []

withMacroHint :: Handle -> DD.DefiniteDescription -> DefKind -> Hint -> App a -> App a
withMacroHint h dd defKind hint action = do
  let Handle {macroCallStack} = h
  liftIO $ modifyIORef' macroCallStack ((dd, defKind, hint) :)
  result <- action
  liftIO $ modifyIORef' macroCallStack (drop 1)
  return result

specializeMacro ::
  Handle ->
  Hint ->
  Hint ->
  TraceID ->
  DD.DefiniteDescription ->
  DefKind ->
  Subst.Subst ->
  [TM.Type] ->
  [BinderF TM.Type] ->
  TM.Term ->
  TM.Type ->
  [TM.Term] ->
  App TM.Term
specializeMacro h m mUse callTraceID dd defKind subType impArgs' expParams body codType expArgsAll = do
  mActiveDefineMeta <- lookupActiveDefineMeta h dd impArgs'
  case mActiveDefineMeta of
    Just activeDefineMeta -> do
      retargetTag h mUse activeDefineMeta
      let helperName = Stmt.defineMetaHelperName activeDefineMeta
      mSpecializedName <- lookupSpecialization h helperName impArgs'
      case mSpecializedName of
        Just specializedName ->
          return $ applySpecialization m callTraceID specializedName expArgsAll
        Nothing -> do
          let expParams' = Stmt.defineMetaExpArgs activeDefineMeta
          let body' = Stmt.defineMetaBody activeDefineMeta
          let codType' = Stmt.defineMetaCodType activeDefineMeta
          specializeMacroBody h m callTraceID dd defKind helperName impArgs' IntMap.empty expParams' body' codType' expArgsAll
    Nothing -> do
      mSpecializedName <- lookupSpecialization h dd impArgs'
      case mSpecializedName of
        Just specializedName ->
          return $ applySpecialization m callTraceID specializedName expArgsAll
        Nothing -> do
          specializeMacroBody h m callTraceID dd defKind dd impArgs' subType expParams body codType expArgsAll

specializeMacroBody ::
  Handle ->
  Hint ->
  TraceID ->
  DD.DefiniteDescription ->
  DefKind ->
  DD.DefiniteDescription ->
  [TM.Type] ->
  Subst.Subst ->
  [BinderF TM.Type] ->
  TM.Term ->
  TM.Type ->
  [TM.Term] ->
  App TM.Term
specializeMacroBody h m callTraceID dd defKind specializationKey impArgs' subType expParams body codType expArgsAll = do
  let tracer = withMacroHint h dd defKind m
  (expBinders', body') <- liftIO $ Subst.subst' (substHandle h) subType expParams body
  specializedName <- createSpecializationName h specializationKey
  codType' <- liftIO $ Subst.substType (substHandle h) subType codType
  beginSpecialization h specializationKey impArgs' specializedName
  let hDefineMeta = enterOpaqueDefineMetaBody h
  body'' <- tracer $ liftIO (Subst.refresh (substHandle h) body') >>= inline hDefineMeta
  let stmt = Stmt.StmtDefine False SK.Define (SavedHint m) specializedName [] expBinders' [] codType' body''
  completeSpecialization h stmt
  return $ applySpecialization m callTraceID specializedName expArgsAll

specializeLocalMeta ::
  Handle ->
  Hint ->
  DD.DefiniteDescription ->
  Subst.Subst ->
  [TM.Type] ->
  [BinderF TM.Type] ->
  TM.Term ->
  TM.Type ->
  [TM.Term] ->
  App TM.Term
specializeLocalMeta h m dd subType impArgs' expParams body codType expArgsAll = do
  case lookupLocalMeta dd impArgs' (localMetaMemo h) of
    Just specSelfId ->
      return $ m :< TM.PiElim noTrace CC.normal (m :< TM.Var specSelfId) [] expArgsAll []
    Nothing -> do
      specSelfId <- liftIO $ Sym.newIdentFromText (gensymHandle h) "meta-spec"
      (expParams', body') <- liftIO $ Subst.subst' (substHandle h) subType expParams body
      codType' <- liftIO $ Subst.substType (substHandle h) subType codType
      let hInner = (enterOpaqueDefineMetaBody h) {localMetaMemo = (dd, impArgs', specSelfId) : localMetaMemo h}
      body'' <- liftIO (Subst.refresh (substHandle h) body') >>= inline hInner
      lamID <- liftIO $ Gensym.newCount (gensymHandle h)
      let fixAttr = AttrL.Attr {lamKind = LK.Fix LDK.Define False (m, VK.normal, specSelfId, codType'), identity = lamID}
      let fixLam = m :< TM.PiIntro fixAttr [] expParams' [] body''
      return $ m :< TM.PiElim noTrace CC.normal fixLam [] expArgsAll []

lookupLocalMeta :: DD.DefiniteDescription -> [TM.Type] -> [(DD.DefiniteDescription, [TM.Type], Ident)] -> Maybe Ident
lookupLocalMeta dd typeArgs memo =
  case memo of
    [] ->
      Nothing
    (dd', typeArgs', specSelfId) : rest
      | dd == dd' && TermEq.eqTypes typeArgs' typeArgs ->
          Just specSelfId
      | otherwise ->
          lookupLocalMeta dd typeArgs rest

isLocalDefineMeta :: LK.LamKindF a -> Bool
isLocalDefineMeta lamKind =
  case lamKind of
    LK.Fix LDK.DefineMeta _ _ ->
      True
    _ ->
      False

registerLocalMeta ::
  Handle ->
  Hint ->
  Int ->
  AttrL.Attr TM.Type ->
  [BinderF TM.Type] ->
  [BinderF TM.Type] ->
  [(BinderF TM.Type, TM.Term)] ->
  TM.Term ->
  App (DD.DefiniteDescription, DefInfo)
registerLocalMeta h m identity attr impBinders expBinders defBinders body = do
  let g = DD.getLocalMetaDD identity
  defMap <- liftIO $ readIORef (localMetaDefMap h)
  case Map.lookup g defMap of
    Just defInfo ->
      return (g, defInfo)
    Nothing -> do
      let codType = case AttrL.lamKind attr of
            LK.Fix _ _ (_, _, _, t) -> t
            LK.Normal _ _ t -> t
      let selfArgNum = AN.fromInt $ length impBinders + length expBinders + length defBinders
      let gTerm = m :< TM.VarGlobal (AttrVG.new selfArgNum) g
      body' <- case AttrL.fromAttr attr of
        Just (_, _, selfId, _) -> do
          let sub = IntMap.singleton (Ident.toInt selfId) (Subst.Term gTerm)
          liftIO $ Subst.subst (substHandle h) sub body
        Nothing ->
          return body
      let defInfo =
            DefInfo
              { defImpBinders = impBinders,
                defExpBinders = expBinders,
                defDefaultArgs = defBinders,
                defBody = body',
                codType = codType,
                defKind = Macro,
                traceSiteIDs = IntSet.empty
              }
      liftIO $ modifyIORef' (localMetaDefMap h) $ Map.insert g defInfo
      return (g, defInfo)

canReduceByLamKind :: LK.LamKindF a -> Bool
canReduceByLamKind lamKind =
  case lamKind of
    LK.Normal {} ->
      True
    LK.Fix kind False _ ->
      not $ O.isOpaque $ LDK.toOpacity kind
    _ ->
      False

getSelfIdent :: LK.LamKindF a -> Maybe Ident
getSelfIdent lamKind =
  case lamKind of
    LK.Fix kind False (_, _, selfIdent, _)
      | not $ O.isOpaque $ LDK.toOpacity kind ->
          Just selfIdent
    _ ->
      Nothing

isMacroDef :: DefKind -> Bool
isMacroDef dk =
  case dk of
    Macro ->
      True
    MacroInline ->
      True
    ConstantMeta ->
      True
    _ ->
      False

canInlineDefKind :: DefKind -> Bool
canInlineDefKind dk =
  case dk of
    NoInline ->
      False
    _ ->
      True
