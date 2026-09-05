{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Monad law, left identity" #-}

module Kernel.Lower.Lower
  ( Handle,
    new,
    lower,
    lowerEntryPoint,
  )
where

import App.App (App)
import App.Run (raiseCritical, raiseCritical')
import Console.ReportMode qualified as Report
import Control.Monad
import Control.Monad.Writer.Lazy
import Data.ByteString qualified as BS
import Data.ByteString.Builder
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.IntMap qualified as IntMap
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Gensym.Gensym qualified as Gensym
import Gensym.Handle qualified as Gensym
import Kernel.Common.Allocator (Allocator, AllocatorSpec (..), allocatorForeignList, allocatorSpec)
import Kernel.Common.Const
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.ModulePath qualified as ModulePath
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.Target
import Kernel.Common.Trace qualified as Trace
import Kernel.Lower.CoercionCancel qualified as CoercionCancel
import Kernel.Lower.DeadLetElim qualified as DeadLetElim
import Kernel.Lower.FreeMallocCancel qualified as FreeMallocCancel
import Kernel.Lower.HoistStackAlloc qualified as HoistStackAlloc
import Kernel.Lower.MallocFreeCancel qualified as MallocFreeCancel
import Language.Common.ArgNum qualified as AN
import Language.Common.BaseLowType qualified as BLT
import Language.Common.BasePrimType qualified as BPT
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.DataSize qualified as DS
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.ExternalName qualified as EN
import Language.Common.Foreign qualified as F
import Language.Common.ForeignCodType qualified as F
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.Ident
import Language.Common.LowMagic qualified as LM
import Language.Common.LowType qualified as LT
import Language.Common.LowType.FromBaseLowType qualified as LT
import Language.Common.PrimNumSize
import Language.Common.PrimNumSize.ToInt
import Language.Common.SlotSize
import Language.Common.PrimOp
import Language.Common.PrimOp.ConvOp qualified as ConvOp
import Language.Common.PrimType qualified as PT
import Language.Common.CellLayout qualified as CL
import Language.Comp.Comp qualified as C
import Language.Comp.CreateVar (createVar)
import Language.Comp.EnumCase qualified as EC
import Language.Comp.Reduce qualified as Reduce
import Language.Comp.Subst qualified as Subst
import Language.LowComp.DeclarationName qualified as DN
import Language.LowComp.LowComp qualified as LC
import Language.LowComp.Render qualified as LCR
import Logger.Debug qualified as Logger
import Logger.Handle qualified as Logger

data Handle = Handle
  { allocator :: Allocator,
    modulePathHandle :: ModulePath.Handle,
    baseSize :: DS.DataSize,
    gensymHandle :: Gensym.Handle,
    envHandle :: Env.Handle,
    reduceHandle :: Reduce.Handle,
    substHandle :: Subst.Handle,
    declEnv :: IORef DN.DeclEnv,
    globalEnv :: IORef LC.GlobalEnv,
    staticTextList :: IORef [(T.Text, (Builder, Int))],
    staticDataMap :: IORef (Map.HashMap T.Text [LC.StaticMember]),
    definedNameSet :: IORef (S.Set DD.DefiniteDescription),
    referencedNameSet :: IORef (S.Set DD.DefiniteDescription),
    fileDefArityRef :: IORef (Map.HashMap DD.DefiniteDescription Int),
    exportListRef :: IORef [LC.ExportInfo],
    currentArity :: Maybe Int,
    traceConfig :: Trace.Config,
    loggerHandle :: Logger.Handle
  }

data Cont = Cont
  { inTailPosition :: Bool,
    sendResult :: LT.LowType -> LC.Value -> App LC.Comp
  }

tailCont :: Cont
tailCont =
  Cont {inTailPosition = True, sendResult = \_ v -> return $ LC.Return v}

bindCont :: (LT.LowType -> LC.Value -> App LC.Comp) -> Cont
bindCont f =
  Cont {inTailPosition = False, sendResult = f}

new :: Gensym.Handle -> Global.Handle -> Trace.Config -> Target -> C.DefMap -> App Handle
new gensymHandle (Global.Handle {..}) traceConfig target defMap = do
  allocator <- Env.getAllocatorByTarget envHandle target
  let baseSize = Platform.getDataSize platformHandle
  let substHandle = Subst.new gensymHandle
  let reduceHandle = Reduce.new substHandle gensymHandle defMap
  declEnv <- liftIO $ newIORef $ makeBaseDeclEnv baseSize (allocatorSpec allocator)
  globalEnv <- liftIO $ newIORef Map.empty
  staticTextList <- liftIO $ newIORef []
  staticDataMap <- liftIO $ newIORef Map.empty
  definedNameSet <- liftIO $ newIORef S.empty
  referencedNameSet <- liftIO $ newIORef S.empty
  exportListRef <- liftIO $ newIORef []
  fileDefArityRef <- liftIO $ newIORef Map.empty
  let currentArity = Nothing
  return $ Handle {..}

makeBaseDeclEnv :: DS.DataSize -> AllocatorSpec -> DN.DeclEnv
makeBaseDeclEnv dataSize spec = do
  Map.fromList $ flip map (allocatorForeignList dataSize spec) $ \(_, F.Foreign _ name domList cod) -> do
    (DN.Ext name, (domList, cod, DN.Fixed))

lower :: Handle -> [C.CompStmt] -> [C.CompStmt] -> App LC.LowCode
lower h stmtList auxStmtList = do
  let auxNameSet = S.fromList $ mapMaybe C.getCompStmtName auxStmtList
  liftIO $ registerInternalNames h (stmtList ++ auxStmtList)
  liftIO $ forM DD.baseTypes $ \dd ->
    insDeclEnv h (DN.In dd) AN.argNumS4 (FCT.Cod BLT.Pointer)
  stmtDefList <- catMaybes <$> mapM (lowerStmt h) stmtList
  auxDefList <- lowerAuxStmtList h auxNameSet auxStmtList
  LC.LowCodeNormal <$> liftIO (summarize h (stmtDefList ++ auxDefList))

lowerEntryPoint :: Handle -> MainTarget -> [C.CompStmt] -> App LC.LowCode
lowerEntryPoint h target stmtList = do
  liftIO $ registerInternalNames h stmtList
  mainDD <- Env.getMainDefiniteDescriptionByTarget (envHandle h) target
  liftIO $ insDeclEnv h (DN.In mainDD) mainEntryArgNum (FCT.Cod BLT.slot)
  mainDef <- liftIO $ constructMainTerm h mainDD
  stmtList' <- catMaybes <$> mapM (lowerStmt h) stmtList
  LC.LowCodeMain mainDef <$> liftIO (summarize h stmtList')

summarize :: Handle -> [LC.Def] -> IO LC.LowCodeInfo
summarize h stmtList = do
  declEnv <- readIORef $ declEnv h
  globalEnv <- readIORef $ globalEnv h
  staticTextList <- readIORef $ staticTextList h
  staticDataMap <- readIORef $ staticDataMap h
  exportList <- readIORef $ exportListRef h
  return (declEnv, globalEnv, stmtList, staticTextList, Map.toList staticDataMap, exportList)

optimize :: Handle -> LC.Comp -> IO LC.Comp
optimize h = do
  return . MallocFreeCancel.mallocFreeCancel (baseSize h)
    >=> FreeMallocCancel.freeMallocCancel FreeMallocCancel.Exact (gensymHandle h)
    >=> FreeMallocCancel.freeMallocCancel FreeMallocCancel.Compatible (gensymHandle h)
    >=> HoistStackAlloc.hoistStackAlloc (gensymHandle h) (baseSize h)
    >=> return . CoercionCancel.coercionCancel (baseSize h)
    >=> return . DeadLetElim.deadLetElim

lowerStmt :: Handle -> C.CompStmt -> App (Maybe LC.Def)
lowerStmt h stmt = do
  case stmt of
    C.Def name _ args e -> do
      let argTypes = replicate (length args) LT.slotLowType
      e0 <- lowerComp (h {currentArity = Just (length args)}) e tailCont
      e' <- liftIO $ optimize h e0
      let def = LC.DefContent LT.slotLowType (zip args argTypes) e'
      reportTrace h name (name, def)
      return $ Just (name, def)
    C.Foreign {} -> do
      return Nothing
    C.Expose exposeList -> do
      arityMap <- liftIO $ readIORef (fileDefArityRef h)
      forM_ exposeList $ \(m, dd, extName) -> do
        case Map.lookup dd arityMap of
          Nothing ->
            raiseCritical m "The target of this `expose` is not defined in this file"
          Just arity -> do
            let visibleArity = arity - LC.internalTrailingArgCount
            liftIO $ insertReferencedName h dd
            liftIO $ modifyIORef' (exportListRef h) $ (:) (extName, dd, replicate visibleArity LT.slotLowType, LT.slotLowType)
      return Nothing

reportTrace :: Handle -> DD.DefiniteDescription -> LC.Def -> App ()
reportTrace h name def = do
  modulePathMap <- liftIO $ ModulePath.get $ modulePathHandle h
  when (Trace.matches (traceConfig h) modulePathMap Report.LowCompPhase name) $ do
    liftIO $ Logger.trace (loggerHandle h) $ "[lowcomp]\n" <> LCR.renderDef def

registerInternalNames :: Handle -> [C.CompStmt] -> IO ()
registerInternalNames h stmtList =
  forM_ stmtList $ \stmt -> do
    case stmt of
      C.Def name _ defArgs _ -> do
        modifyIORef' (definedNameSet h) $ S.insert name
        modifyIORef' (fileDefArityRef h) $ Map.insert name (length defArgs)
      C.Foreign foreignList ->
        forM_ foreignList $ \(F.Foreign _ name domList cod) -> do
          insDeclEnv' h (DN.Ext name) domList cod
      C.Expose {} ->
        return ()

lowerAuxStmtList :: Handle -> S.Set DD.DefiniteDescription -> [C.CompStmt] -> App [LC.Def]
lowerAuxStmtList h auxNameSet auxStmtList =
  go S.empty []
  where
    go loweredAuxNameSet acc = do
      referencedAuxNameSet <-
        liftIO $
          S.intersection auxNameSet <$> getReferencedNameSet h
      let pendingAuxNameSet = S.difference referencedAuxNameSet loweredAuxNameSet
      let pendingStmtList =
            filter
              (maybe False (`S.member` pendingAuxNameSet) . C.getCompStmtName)
              auxStmtList
      if null pendingStmtList
        then
          return acc
        else do
          liftIO $ registerInternalNames h pendingStmtList
          pendingDefList <- catMaybes <$> mapM (lowerStmt h) pendingStmtList
          let loweredAuxNameSet' =
                S.union loweredAuxNameSet (S.fromList $ mapMaybe C.getCompStmtName pendingStmtList)
          go loweredAuxNameSet' (acc ++ pendingDefList)

mainEntryArgNum :: AN.ArgNum
mainEntryArgNum =
  AN.fromInt 2

mainEntryArgs :: [(LT.LowType, LC.Value)]
mainEntryArgs =
  [(LT.slotLowType, LC.Int 0), (LT.slotLowType, LC.Int 0)]

constructMainTerm :: Handle -> DD.DefiniteDescription -> IO LC.DefContent
constructMainTerm h mainName = do
  argc <- Gensym.newIdentFromText (gensymHandle h) "argc"
  argv <- Gensym.newIdentFromText (gensymHandle h) "argv"
  argcSlot <- Gensym.newIdentFromText (gensymHandle h) "argc-slot"
  let argcGlobal = LC.VarExternal (EN.ExternalName unsafeArgcName)
  let argvGlobal = LC.VarExternal (EN.ExternalName unsafeArgvName)
  let widenArgc = LC.PrimOp (PrimConvOp ConvOp.Zext cIntPrimType slotPrimType) [LC.VarLocal argc]
  let mainTerm =
        LC.Let argcSlot widenArgc $
          LC.Cont (LC.Store LT.slotLowType (LC.VarLocal argcSlot) argcGlobal) $
            LC.Cont (LC.Store LT.Pointer (LC.VarLocal argv) argvGlobal) $
              LC.Cont (LC.Call False LT.slotLowType (LC.VarGlobal mainName) mainEntryArgs) $
                LC.Return (LC.Int 0)
  return $ LC.DefContent cIntLowType [(argc, cIntLowType), (argv, LT.Pointer)] mainTerm

cIntPrimType :: PT.PrimType
cIntPrimType =
  PT.Int IntSize32

cIntLowType :: LT.LowType
cIntLowType =
  LT.PrimNum cIntPrimType

hasMatchingSignature :: Handle -> C.Value -> Int -> Bool
hasMatchingSignature h callee argCount =
  case (currentArity h, callee) of
    (Just arity, C.VarGlobal _ calleeArgNum _) ->
      arity == argCount && AN.reify calleeArgNum == argCount
    _ ->
      False

moveOf :: LT.LowType -> LC.Value -> LC.Op
moveOf t v =
  LC.Bitcast v t t

lowerComp :: Handle -> C.Comp -> Cont -> App LC.Comp
lowerComp h term k =
  case term of
    C.PiElimDownElim forceInline v ds -> do
      (funcVar, func) <- liftIO $ newValueLocal h "func"
      (castFuncVar, castFunc) <- liftIO $ newValueLocal h "func"
      (argVars, argValues) <- mapAndUnzipM (const $ liftIO $ newValueLocal h "arg") ds
      let args = zip (replicate (length ds) LT.slotLowType) argValues
      let codType = LT.slotLowType
      if inTailPosition k
        then
          lowerValue h funcVar v
            =<< lowerValues h (zip argVars ds)
            =<< cast h castFuncVar func LT.Pointer
            =<< return (LC.TailCall (hasMatchingSignature h v (length ds)) codType castFunc args)
        else do
          (resultVar, resultValue) <- liftIO $ newValueLocal h "result"
          let isPure = isSizeQuery forceInline ds
          rest <- sendResult k codType resultValue
          lowerValue h funcVar v
            =<< lowerValues h (zip argVars ds)
            =<< cast h castFuncVar func LT.Pointer
            =<< return (LC.Let resultVar (LC.Call isPure codType castFunc args) rest)
    C.SigmaElim shouldDeallocate slotIndex layout xs v e -> do
      (sigmaVar, sigma) <- liftIO $ newValueLocal h "sigma"
      (elemVars, elems) <- mapAndUnzipM (const $ liftIO $ newValueLocal h "elem") xs
      let slots = drop slotIndex $ CL.cellSlots layout
      body <- lowerComp h e k
      afterRead <- liftIO $ freeIfNecessary h shouldDeallocate sigma (CL.cellByteSize layout) body
      lowerValueLetCast h sigmaVar v LT.Pointer
        =<< return . fieldPointers (zip elemVars (map fst slots)) sigma
        =<< loadElements h sigma (zip xs (zip elems (map (widthLowType . snd) slots)))
        =<< return afterRead
    C.UpIntro d -> do
      (resultVar, resultValue) <- liftIO $ newValueLocal h "result"
      rest <- sendResult k LT.slotLowType resultValue
      lowerValue h resultVar d rest
    C.UpElim _ x e1 e2 ->
      lowerComp h e1 $ bindCont $ \t1 v1 -> do
        rest <- lowerComp h e2 k
        return $ LC.Let x (moveOf t1 v1) rest
    C.EnumElim fvInfo v defaultBranch branchList -> do
      let sub = IntMap.fromList fvInfo
      defaultBranch' <- liftIO $ Subst.subst (substHandle h) sub defaultBranch >>= Reduce.reduce (reduceHandle h)
      let (keys, clauses) = unzip branchList
      clauses' <- liftIO $ mapM (Subst.subst (substHandle h) sub >=> Reduce.reduce (reduceHandle h)) clauses
      let branchCont =
            Cont
              { inTailPosition = inTailPosition k,
                sendResult = \t val -> do
                  (branchPhiName, branchPhiVar) <- liftIO $ newValueLocal h "phi"
                  return $ LC.Let branchPhiName (moveOf t val) (LC.Phi [branchPhiVar])
              }
      defaultCase <- lowerComp h defaultBranch' branchCont
      caseList <-
        mapM
          ( \(tag, branch) -> do
              branch' <- lowerComp h branch branchCont
              return (enumCaseToInteger tag, branch')
          )
          (zip keys clauses')
      (phiName, phiValue) <- liftIO $ newValueLocal h "phi"
      rest <- sendResult k LT.slotLowType phiValue
      (castVar, castValue) <- liftIO $ newValueLocal h "cast"
      lowerValueLetCast h castVar v LT.slotLowType
        =<< return (LC.Switch castValue LT.slotLowType defaultCase caseList [(phiName, LT.slotLowType)] rest)
    C.OutputProvide dest sizeComp result ->
      liftIO (placeResult h dest sizeComp result)
        >>= liftIO . Reduce.reduce (reduceHandle h)
        >>= \e -> lowerComp h e k
    C.OutputRequest sizeComp f ds -> do
      sizeComp' <- liftIO $ Reduce.reduce (reduceHandle h) sizeComp
      liftIO (materializeOutputRequest h (inTailPosition k) sizeComp' f ds)
        >>= liftIO . Reduce.reduce (reduceHandle h)
        >>= \e -> lowerComp h e k
    C.Primitive theta ->
      lowerCompPrimitive h theta k
    C.Free x size cont -> do
      freeID <- liftIO $ Gensym.newCount (gensymHandle h)
      (ptrVar, ptr) <- liftIO $ newValueLocal h "ptr"
      lowerValueLetCast h ptrVar x LT.Pointer
        =<< return . LC.Cont (LC.Free ptr size freeID)
        =<< lowerComp h cont k
    C.Unreachable ->
      return LC.Unreachable

isSizeQuery :: C.ForceInline -> [C.Value] -> Bool
isSizeQuery forceInline ds =
  case ds of
    [C.Int _ 2, _, _] ->
      forceInline
    _ ->
      False

withSizeValue :: Handle -> C.Comp -> (C.Value -> IO C.Comp) -> IO C.Comp
withSizeValue h sizeComp k =
  case sizeComp of
    C.UpIntro size@(C.Int _ _) ->
      k size
    _ -> do
      (sizeName, sizeVar) <- createVar (gensymHandle h) "size"
      body <- k sizeVar
      return $ C.UpElim True sizeName sizeComp body

materializeOutputRequest :: Handle -> Bool -> C.Comp -> C.Value -> [C.Value] -> IO C.Comp
materializeOutputRequest h isTail sizeComp f ds = do
  withSizeValue h sizeComp $ \size -> do
    (destName, destVar) <- createVar (gensymHandle h) "dest"
    let call = C.PiElimDownElim False f (destVar : ds)
    body <-
      if isTail
        then return call
        else do
          (ignored, _) <- createVar (gensymHandle h) "_"
          return $
            C.UpElim True ignored call $
              C.UpIntro destVar
    return $ C.UpElim True destName (C.Primitive $ C.Alloc size) body

placeResult :: Handle -> C.Value -> C.Comp -> C.Comp -> IO C.Comp
placeResult h dest sizeComp result = do
  (valueName, valueVar) <- createVar (gensymHandle h) "value"
  (ignored, _) <- createVar (gensymHandle h) "_"
  body <- withSizeValue h sizeComp $ \size ->
    return $
      C.UpElim True ignored (C.Primitive $ C.Memcpy dest valueVar size) $
        C.Free valueVar Nothing (C.UpIntro dest)
  return $ C.UpElim False valueName result body

enumCaseToInteger :: EC.EnumCase -> Integer
enumCaseToInteger enumCase =
  case enumCase of
    EC.Int i ->
      i

lowerCompPrimitive :: Handle -> C.Primitive -> Cont -> App LC.Comp
lowerCompPrimitive h codeOp k =
  case codeOp of
    C.PrimOp op vs ->
      lowerCompPrimOp h op vs k
    C.ShiftPointer v offset -> do
      (resultVar, resultValue) <- liftIO $ newValueLocal h "result"
      (shiftedVar, shiftedValue) <- liftIO $ newValueLocal h "shifted"
      (ptrVar, ptr) <- liftIO $ newValueLocal h "pointer"
      let byteType = LT.PrimNum $ PT.Int IntSize8
      let indexList' = [(LC.Int offset, LT.PrimNum $ PT.Int IntSize32)]
      rest <- sendResult k LT.slotLowType resultValue
      lowerValueLetCast h ptrVar v LT.Pointer
        =<< return . LC.Let shiftedVar (LC.GetElementPtr (ptr, byteType) indexList')
        =<< uncast h resultVar shiftedValue LT.Pointer rest
    C.Calloc num size -> do
      (resultVar, resultValue) <- liftIO $ newValueLocal h "result"
      byteCountVarName <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "size"
      let byteCountValue = C.VarLocal byteCountVarName
      numVarName <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "num"
      let numValue = C.VarLocal numVarName
      (castSizeVar, castSizeValue) <- liftIO $ newValueLocal h "size"
      (castNumVar, castNumValue) <- liftIO $ newValueLocal h "num"
      (cellVar, cellValue) <- liftIO $ newValueLocal h "cell"
      let lowInt = LT.PrimNum $ PT.Int $ dataSizeToIntSize (baseSize h)
      rest <- sendResult k LT.slotLowType resultValue
      lowerValue h byteCountVarName size
        =<< lowerValue h numVarName num
        =<< lowerValueLetCast h castSizeVar byteCountValue lowInt
        =<< lowerValueLetCast h castNumVar numValue lowInt
        =<< return . LC.Let cellVar (LC.Calloc castNumValue castSizeValue)
        =<< uncast h resultVar cellValue LT.Pointer rest
    C.Alloc size -> do
      (resultVar, resultValue) <- liftIO $ newValueLocal h "result"
      (cellVar, cellValue) <- liftIO $ newValueLocal h "cell"
      allocID <- liftIO $ Gensym.newCount (gensymHandle h)
      rest <- sendResult k LT.slotLowType resultValue
      case size of
        C.Int _ knownByteCount ->
          LC.Let cellVar (LC.Alloc (Left knownByteCount) allocID)
            <$> uncast h resultVar cellValue LT.Pointer rest
        runtimeByteSize -> do
          byteCountVarName <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "size"
          let byteCountValue = C.VarLocal byteCountVarName
          (castVar, castValue) <- liftIO $ newValueLocal h "size"
          let lowInt = LT.PrimNum $ PT.Int $ dataSizeToIntSize (baseSize h)
          lowerValue h byteCountVarName runtimeByteSize
            =<< lowerValueLetCast h castVar byteCountValue lowInt
            =<< return . LC.Let cellVar (LC.Alloc (Right castValue) allocID)
            =<< uncast h resultVar cellValue LT.Pointer rest
    C.Realloc ptr size -> do
      (resultVar, resultValue) <- liftIO $ newValueLocal h "result"
      byteCountVarName <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "size"
      let byteCountValue = C.VarLocal byteCountVarName
      (castVar, castValue) <- liftIO $ newValueLocal h "size"
      (ptrVar, ptrValue) <- liftIO $ newValueLocal h "ptr"
      (cellVar, cellValue) <- liftIO $ newValueLocal h "cell"
      let lowInt = LT.PrimNum $ PT.Int $ dataSizeToIntSize (baseSize h)
      rest <- sendResult k LT.slotLowType resultValue
      lowerValue h byteCountVarName size
        =<< lowerValueLetCast h castVar byteCountValue lowInt
        =<< lowerValueLetCast h ptrVar ptr LT.Pointer
        =<< return . LC.Let cellVar (LC.Realloc ptrValue castValue)
        =<< uncast h resultVar cellValue LT.Pointer rest
    C.Memcpy dest src size -> do
      byteCountVarName <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "size"
      let byteCountValue = C.VarLocal byteCountVarName
      lowerValue h byteCountVarName size
        =<< lowerCompPrimitive h (memcpyExternal dest src byteCountValue) k
    C.Magic der -> do
      case der of
        LM.Cast _ _ value -> do
          (resultVar, resultValue) <- liftIO $ newValueLocal h "result"
          rest <- sendResult k LT.slotLowType resultValue
          lowerValue h resultVar value rest
        LM.Store storedType _ value pointer -> do
          let storedType' = LT.fromBaseLowType storedType
          (resultVar, resultValue) <- liftIO $ newValueLocal h "result"
          (valVar, val) <- liftIO $ newValueLocal h "val"
          (ptrVar, ptr) <- liftIO $ newValueLocal h "ptr"
          rest <- sendResult k LT.slotLowType resultValue
          lowerValueLetCast h valVar value storedType'
            =<< lowerValueLetCast h ptrVar pointer LT.Pointer
            =<< return . LC.Let resultVar (moveOf LT.slotLowType (LC.Int 0))
            =<< return (LC.Cont (LC.Store storedType' val ptr) rest)
        LM.Load loadedType pointer -> do
          let valueLowType' = LT.fromBaseLowType loadedType
          (resultVar, resultValue) <- liftIO $ newValueLocal h "result"
          (tmpVar, tmp) <- liftIO $ newValueLocal h "tmp"
          (ptrVar, ptrValue) <- liftIO $ newValueLocal h "ptr"
          rest <- sendResult k LT.slotLowType resultValue
          lowerValueLetCast h ptrVar pointer LT.Pointer
            =<< return . LC.Let tmpVar (LC.Load ptrValue valueLowType')
            =<< uncast h resultVar tmp valueLowType' rest
        LM.Alloca t size -> do
          let t' = LT.fromBaseLowType t
          let indexType = LT.PrimNum $ PT.Int $ dataSizeToIntSize (baseSize h)
          (resultVar, resultValue) <- liftIO $ newValueLocal h "result"
          (ptrVar, ptrValue) <- liftIO $ newValueLocal h "ptr"
          stackSlotID <- liftIO $ Gensym.newCount (gensymHandle h)
          rest <- sendResult k LT.slotLowType resultValue
          case size of
            C.Int _ n -> do
              let stackAllocInfo =
                    LC.StackAllocInfo
                      { stackSlotID = stackSlotID,
                        stackElemType = t',
                        stackIndexType = indexType,
                        stackSize = Left n
                      }
              return . LC.Let ptrVar (LC.StackAlloc stackAllocInfo)
                =<< return . LC.Cont (LC.StackLifetimeStart stackSlotID)
                =<< uncast h resultVar ptrValue LT.Pointer rest
            _ -> do
              (sizeVar, sizeValue) <- liftIO $ newValueLocal h "size"
              lowerValueLetCast h sizeVar size indexType
                =<< return
                  . LC.Let
                    ptrVar
                    ( LC.StackAlloc $
                        LC.StackAllocInfo
                          { stackSlotID = stackSlotID,
                            stackElemType = t',
                            stackIndexType = indexType,
                            stackSize = Right sizeValue
                          }
                    )
                =<< return . LC.Cont (LC.StackLifetimeStart stackSlotID)
                =<< uncast h resultVar ptrValue LT.Pointer rest
        LM.External domList cod name fixedArgs varArgAndTypeList -> do
          if null varArgAndTypeList
            then do
              alreadyRegistered <- liftIO $ member h (DN.Ext name)
              unless alreadyRegistered $ do
                liftIO $ insDeclEnv' h (DN.Ext name) domList cod
            else do
              liftIO $ insDeclEnvVariadic h (DN.Ext name) domList cod
          let (varArgs, varTypes) = unzip varArgAndTypeList
          let argCaster = map LT.fromBaseLowType $ domList ++ varTypes
          let suffix = if null varArgs then [] else [LT.VarArgs]
          let lowCod = F.fromForeignCodType cod
          let funcType = LT.Function (map LT.fromBaseLowType domList ++ suffix) lowCod
          let args = fixedArgs ++ varArgs
          (resultVar, resultValue) <- liftIO $ newValueLocal h "result"
          case lowCod of
            LT.Void -> do
              (argVars, argValues) <- mapAndUnzipM (const $ liftIO $ newValueLocal h "arg") args
              rest <- sendResult k LT.slotLowType resultValue
              lowerAndCastValues h (zip argVars (zip args argCaster))
                =<< return . LC.Let resultVar (moveOf LT.slotLowType (LC.Int 0))
                =<< return (LC.Cont (LC.MagicCall funcType (LC.VarExternal name) $ zip argCaster argValues) rest)
            _ -> do
              (tmpVar, tmpValue) <- liftIO $ newValueLocal h "tmp"
              (argVars, argValues) <- mapAndUnzipM (const $ liftIO $ newValueLocal h "arg") args
              rest <- sendResult k LT.slotLowType resultValue
              lowerAndCastValues h (zip argVars (zip args argCaster))
                =<< return . LC.Let tmpVar (LC.MagicCall funcType (LC.VarExternal name) $ zip argCaster argValues)
                =<< uncast h resultVar tmpValue lowCod rest
        LM.Global name t -> do
          let t' = LT.fromBaseLowType t
          liftIO $ modifyIORef' (globalEnv h) $ Map.insertWith (\_ old -> old) name t
          (resultVar, resultValue) <- liftIO $ newValueLocal h "result"
          rest <- sendResult k LT.slotLowType resultValue
          uncast h resultVar (LC.VarExternal name) t' rest
        LM.OpaqueValue e -> do
          (resultVar, resultValue) <- liftIO $ newValueLocal h "result"
          rest <- sendResult k LT.slotLowType resultValue
          lowerValue h resultVar e rest
        LM.CallType func arg1 arg2 arg3 -> do
          (resultVar, resultValue) <- liftIO $ newValueLocal h "result"
          (funcVar, funcValue) <- liftIO $ newValueLocal h "func"
          (castFuncVar, castFuncValue) <- liftIO $ newValueLocal h "func"
          (arg1Var, arg1Value) <- liftIO $ newValueLocal h "arg1"
          (arg2Var, arg2Value) <- liftIO $ newValueLocal h "arg2"
          (arg3Var, arg3Value) <- liftIO $ newValueLocal h "arg3"
          let isPure = isSizeQuery True [arg1, arg2, arg3]
          rest <- sendResult k LT.slotLowType resultValue
          lowerValue h funcVar func
            =<< lowerValue h arg1Var arg1
            =<< lowerValue h arg2Var arg2
            =<< lowerValue h arg3Var arg3
            =<< cast h castFuncVar funcValue LT.Pointer
            =<< return . LC.Let resultVar (LC.Call isPure LT.slotLowType castFuncValue [(LT.slotLowType, arg1Value), (LT.slotLowType, arg2Value), (LT.slotLowType, arg3Value)])
            =<< return rest

lowerCompPrimOp :: Handle -> PrimOp -> [C.Value] -> Cont -> App LC.Comp
lowerCompPrimOp h op vs k = do
  let (domList, cod) = getTypeInfo op
  (argVars, args) <- mapAndUnzipM (const $ liftIO $ newValueLocal h "arg") vs
  (tmpVar, tmp) <- liftIO $ newValueLocal h "tmp"
  (resultVar, resultValue) <- liftIO $ newValueLocal h "result"
  rest <- sendResult k (LT.slotLowType) resultValue
  lowerValueLetCastPrimArgs h (zip argVars (zip vs domList))
    =<< return . LC.Let tmpVar (LC.PrimOp op args)
    =<< uncast h resultVar tmp (LT.PrimNum cod) rest

lowerValueLetCastPrimArgs :: Handle -> [(Ident, (C.Value, PT.PrimType))] -> LC.Comp -> App LC.Comp
lowerValueLetCastPrimArgs h xdts cont =
  case xdts of
    [] ->
      return cont
    ((x, (d, t)) : rest) -> do
      lowerValueLetCast h x d (LT.PrimNum t)
        =<< lowerValueLetCastPrimArgs h rest cont

cast :: Handle -> Ident -> LC.Value -> LT.LowType -> LC.Comp -> App LC.Comp
cast h var v lowType = do
  case lowType of
    LT.PrimNum codPrim@(PT.Int size) ->
      narrowSlot var v codPrim (intSizeToInt size)
    LT.PrimNum PT.Rune ->
      narrowSlot var v (PT.Int IntSize32) 32
    LT.PrimNum (PT.Float size) -> \cont -> do
      let floatType = LT.PrimNum $ PT.Float size
      let intPrim = PT.Int $ floatSizeToIntSize size
      (tmp, tmpVar) <- liftIO $ newValueLocal h "tmp"
      narrowSlot tmp v intPrim (floatSizeToInt size) $
        LC.Let var (LC.Bitcast tmpVar (LT.PrimNum intPrim) floatType) cont
    _ -> \cont ->
      return $ LC.Let var (LC.IntToPointer v LT.slotLowType) cont

narrowSlot :: Ident -> LC.Value -> PT.PrimType -> Int -> LC.Comp -> App LC.Comp
narrowSlot var v codPrim bitSize cont =
  if bitSize == slotBitSize
    then return $ LC.Let var (moveOf (LT.PrimNum codPrim) v) cont
    else return $ LC.Let var (LC.PrimOp (PrimConvOp ConvOp.Trunc slotPrimType codPrim) [v]) cont

uncast :: Handle -> Ident -> LC.Value -> LT.LowType -> LC.Comp -> App LC.Comp
uncast h var castedValue lowType = do
  case lowType of
    LT.PrimNum srcPrim@(PT.Int size) ->
      widenToSlot var castedValue srcPrim (intSizeToInt size)
    LT.PrimNum PT.Rune ->
      widenToSlot var castedValue (PT.Int IntSize32) 32
    LT.PrimNum (PT.Float i) -> \cont -> do
      let floatType = LT.PrimNum $ PT.Float i
      let intPrim = PT.Int $ floatSizeToIntSize i
      (tmp, tmpVar) <- liftIO $ newValueLocal h "tmp"
      LC.Let tmp (LC.Bitcast castedValue floatType (LT.PrimNum intPrim))
        <$> widenToSlot var tmpVar intPrim (floatSizeToInt i) cont
    _ -> \cont ->
      return $ LC.Let var (LC.PointerToInt castedValue LT.slotLowType) cont

widenToSlot :: Ident -> LC.Value -> PT.PrimType -> Int -> LC.Comp -> App LC.Comp
widenToSlot var v srcPrim bitSize cont =
  if bitSize == slotBitSize
    then return $ LC.Let var (moveOf LT.slotLowType v) cont
    else return $ LC.Let var (LC.PrimOp (PrimConvOp ConvOp.Zext srcPrim slotPrimType) [v]) cont

allocateCell :: Handle -> Ident -> Int -> LC.Comp -> App LC.Comp
allocateCell h resultVar byteSize cont =
  if byteSize == 0
    then return $ LC.Let resultVar (LC.nop LC.Null) cont
    else do
      allocID <- liftIO $ Gensym.newCount (gensymHandle h)
      return $ LC.Let resultVar (LC.Alloc (Left (toInteger byteSize)) allocID) cont

createCell :: Handle -> Ident -> CL.CellLayout -> [C.Value] -> LC.Comp -> App LC.Comp
createCell h resultVar layout ds cont = do
  let slots = CL.cellSlots layout
  let widths = map (widthLowType . snd) slots
  (elemVars, elemValues) <- mapAndUnzipM (const $ liftIO $ newValueLocal h "base") ds
  (xs, vs) <- mapAndUnzipM (const $ liftIO $ newValueLocal h "item") ds
  (cellVar, cellValue) <- liftIO $ newValueLocal h "cell"
  lowerAndCastValues h (zip elemVars (zip ds widths))
    =<< allocateCell h cellVar (CL.cellByteSize layout)
    =<< return . fieldPointers (zip xs (map fst slots)) cellValue
    =<< return . storeElements (zip3 widths elemValues vs)
    =<< uncast h resultVar cellValue LT.Pointer cont

storeElements :: [(LT.LowType, LC.Value, LC.Value)] -> LC.Comp -> LC.Comp
storeElements values cont =
  foldr (\(valueType, value, elemPtr) -> store valueType value elemPtr) cont values

store :: LT.LowType -> LC.Value -> LC.Value -> LC.Comp -> LC.Comp
store lowType value pointer =
  LC.Cont (LC.Store lowType value pointer)

load :: Handle -> Ident -> LT.LowType -> LC.Value -> LC.Comp -> App LC.Comp
load h resultVar elemType pointer cont = do
  (tmpVar, tmpValue) <- liftIO $ newValueLocal h "tmp"
  (loadedVar, loadedValue) <- liftIO $ newValueLocal h "loaded"
  return . LC.Let tmpVar (LC.Bitcast pointer LT.Pointer LT.Pointer)
    =<< return . LC.Let loadedVar (LC.Load tmpValue elemType)
    =<< uncast h resultVar loadedValue elemType cont

loadElements ::
  Handle ->
  LC.Value -> -- base pointer
  [(Ident, (LC.Value, LT.LowType))] ->
  LC.Comp ->
  App LC.Comp
loadElements h basePointer values cont =
  case values of
    [] -> do
      return cont
    (targetVar, (valuePointer, valueType)) : rest -> do
      load h targetVar valueType valuePointer
        =<< loadElements h basePointer rest cont

lowerValue :: Handle -> Ident -> C.Value -> LC.Comp -> App LC.Comp
lowerValue h resultVar v cont =
  case v of
    C.VarGlobal globalName argNum cod -> do
      liftIO $ insertReferencedName h globalName
      lowNameSet <- liftIO $ getDefinedNameSet h
      unless (S.member globalName lowNameSet) $ do
        liftIO $ insDeclEnvForGlobalFunc h globalName argNum cod
      uncast h resultVar (LC.VarGlobal globalName) LT.Pointer cont
    C.VarLocal y ->
      return $ LC.Let resultVar (moveOf (LT.slotLowType) (LC.VarLocal y)) cont
    C.VarStaticBytes bytes -> do
      name <- liftIO $ registerStaticBytes h bytes
      uncast h resultVar (LC.VarTextName name) LT.Pointer cont
    C.StaticSigmaIntro name layout ds -> do
      lowerStaticSigma h name layout ds
      uncast h resultVar (LC.VarTextName name) LT.Pointer cont
    C.SigmaIntro layout ds ->
      createCell h resultVar layout ds cont
    C.Int size l -> do
      uncast h resultVar (LC.Int l) (LT.PrimNum $ PT.Int size) cont
    C.Float size f -> do
      uncast h resultVar (LC.Float size f) (LT.PrimNum $ PT.Float size) cont

lowerValues :: Handle -> [(Ident, C.Value)] -> LC.Comp -> App LC.Comp
lowerValues h xvs cont =
  case xvs of
    [] ->
      return cont
    (x, v) : rest -> do
      lowerValue h x v
        =<< lowerValues h rest cont

lowerAndCastValues :: Handle -> [(Ident, (C.Value, LT.LowType))] -> LC.Comp -> App LC.Comp
lowerAndCastValues h xvs cont =
  case xvs of
    [] ->
      return cont
    (x, (v, t)) : rest -> do
      lowerValueLetCast h x v t
        =<< lowerAndCastValues h rest cont

lowerValueLetCast :: Handle -> Ident -> C.Value -> LT.LowType -> LC.Comp -> App LC.Comp
lowerValueLetCast h resultVar v lowType cont = do
  (tmpVar, tmpValue) <- liftIO $ newValueLocal h "tmp"
  lowerValue h tmpVar v
    =<< cast h resultVar tmpValue lowType cont

freeIfNecessary :: Handle -> Bool -> LC.Value -> Int -> LC.Comp -> IO LC.Comp
freeIfNecessary h shouldDeallocate pointer byteCount cont =
  if shouldDeallocate
    then do
      freeID <- Gensym.newCount (gensymHandle h)
      return $ LC.Cont (LC.Free pointer (Just byteCount) freeID) cont
    else
      return cont

-- returns Nothing iff the branch list is empty
newValueLocal :: Handle -> T.Text -> IO (Ident, LC.Value)
newValueLocal h name = do
  x <- Gensym.newIdentFromText (gensymHandle h) name
  return (x, LC.VarLocal x)

insDeclEnv :: Handle -> DN.DeclarationName -> AN.ArgNum -> FCT.ForeignCodType BLT.BaseLowType -> IO ()
insDeclEnv h k argNum cod = do
  insDeclEnv' h k (BLT.toSlotSeq argNum) cod

insDeclEnvForGlobalFunc :: Handle -> DD.DefiniteDescription -> AN.ArgNum -> FCT.ForeignCodType BLT.BaseLowType -> IO ()
insDeclEnvForGlobalFunc h globalName argNum _ =
  insDeclEnv' h (DN.In globalName) (BLT.toSlotSeq argNum) (FCT.Cod BLT.slot)

insDeclEnv' :: Handle -> DN.DeclarationName -> [BLT.BaseLowType] -> FCT.ForeignCodType BLT.BaseLowType -> IO ()
insDeclEnv' h k domList cod = do
  modifyIORef' (declEnv h) $ Map.insert k (domList, cod, DN.Fixed)

insDeclEnvVariadic :: Handle -> DN.DeclarationName -> [BLT.BaseLowType] -> FCT.ForeignCodType BLT.BaseLowType -> IO ()
insDeclEnvVariadic h k domList cod = do
  modifyIORef' (declEnv h) $ Map.insert k (domList, cod, DN.Variadic)

member :: Handle -> DN.DeclarationName -> IO Bool
member h k = do
  denv <- readIORef (declEnv h)
  return $ Map.member k denv

insertStaticText :: Handle -> T.Text -> Builder -> Int -> IO ()
insertStaticText h name text len =
  modifyIORef' (staticTextList h) $ (:) (name, (text, len))

insertStaticData :: Handle -> T.Text -> [LC.StaticMember] -> IO ()
insertStaticData h name members =
  modifyIORef' (staticDataMap h) $ Map.insert name members

hasStaticData :: Handle -> T.Text -> IO Bool
hasStaticData h name = do
  Map.member name <$> readIORef (staticDataMap h)

registerStaticBytes :: Handle -> BS.ByteString -> IO T.Text
registerStaticBytes h bytes = do
  i <- Gensym.newCount (gensymHandle h)
  let name = "bytes;" <> T.pack (show i)
  let encodedBytes = foldMap (\w -> "\\" <> word8HexFixed w) (BS.unpack bytes)
  insertStaticText h name encodedBytes (BS.length bytes)
  return name

lowerStaticSigma :: Handle -> T.Text -> CL.CellLayout -> [C.Value] -> App ()
lowerStaticSigma h name layout ds = do
  alreadyEmitted <- liftIO $ hasStaticData h name
  unless alreadyEmitted $ do
    liftIO $ insertStaticData h name []
    members <- lowerStaticCell h layout ds
    liftIO $ insertStaticData h name members

lowerStaticCell :: Handle -> CL.CellLayout -> [C.Value] -> App [LC.StaticMember]
lowerStaticCell h layout ds = do
  values <- mapM (lowerStaticValue h) ds
  return $ fillStaticGaps (baseSize h) 0 (CL.cellByteSize layout) (zip (CL.cellSlots layout) values)

fillStaticGaps :: DS.DataSize -> Int -> Int -> [((Int, CL.FieldWidth), LC.StaticData)] -> [LC.StaticMember]
fillStaticGaps dataSize cursor byteSize members =
  case members of
    [] ->
      zeroBytes (byteSize - cursor)
    ((offset, width), value) : rest ->
      zeroBytes (offset - cursor)
        ++ LC.StaticValue width value
        : fillStaticGaps dataSize (offset + CL.fieldWidthByteSize dataSize width) byteSize rest

zeroBytes :: Int -> [LC.StaticMember]
zeroBytes byteSize =
  [LC.StaticZeroBytes byteSize | byteSize > 0]

lowerStaticValue :: Handle -> C.Value -> App LC.StaticData
lowerStaticValue h v =
  case v of
    C.StaticSigmaIntro name layout ds -> do
      lowerStaticSigma h name layout ds
      return $ LC.StaticSymbol name
    C.SigmaIntro _ [] ->
      return LC.StaticNull
    C.VarStaticBytes bytes -> do
      LC.StaticSymbol <$> liftIO (registerStaticBytes h bytes)
    C.VarGlobal globalName argNum cod -> do
      liftIO $ insertReferencedName h globalName
      lowNameSet <- liftIO $ getDefinedNameSet h
      unless (S.member globalName lowNameSet) $ do
        liftIO $ insDeclEnvForGlobalFunc h globalName argNum cod
      return $ LC.StaticGlobal globalName
    C.Int size l ->
      return $ LC.StaticInt size l
    C.Float size f ->
      return $ LC.StaticFloat size f
    _ ->
      raiseCritical' "Found a non-static value inside an embedded value"

getDefinedNameSet :: Handle -> IO (S.Set DD.DefiniteDescription)
getDefinedNameSet h = do
  readIORef (definedNameSet h)

getReferencedNameSet :: Handle -> IO (S.Set DD.DefiniteDescription)
getReferencedNameSet h = do
  readIORef (referencedNameSet h)

insertReferencedName :: Handle -> DD.DefiniteDescription -> IO ()
insertReferencedName h name = do
  modifyIORef' (referencedNameSet h) $ S.insert name

getElemPtr :: Ident -> LC.Value -> LT.LowType -> [Integer] -> LC.Comp -> LC.Comp
getElemPtr var value valueType indexList cont = do
  let indexList' = map (\i -> (LC.Int i, LT.PrimNum $ PT.Int IntSize32)) indexList
  LC.Let var (LC.GetElementPtr (value, valueType) indexList') cont

fieldPointers :: [(Ident, Int)] -> LC.Value -> LC.Comp -> LC.Comp
fieldPointers offsetVars basePointer cont = do
  let byteType = LT.PrimNum $ PT.Int IntSize8
  let f (var, offset) = getElemPtr var basePointer byteType [toInteger offset]
  foldr f cont offsetVars

widthLowType :: CL.FieldWidth -> LT.LowType
widthLowType width =
  case width of
    CL.WidthPointer ->
      LT.Pointer
    CL.Width8 ->
      LT.PrimNum $ PT.Int IntSize8
    CL.Width16 ->
      LT.PrimNum $ PT.Int IntSize16
    CL.Width32 ->
      LT.PrimNum $ PT.Int IntSize32
    CL.Width64 ->
      LT.PrimNum $ PT.Int IntSize64

memcpyExternal :: C.Value -> C.Value -> C.Value -> C.Primitive
memcpyExternal dest src byteCount = do
  let ptr = BLT.Pointer
  let int1 = BLT.PrimNum $ BPT.Int $ BPT.Explicit IntSize1
  let int64 = BLT.PrimNum $ BPT.Int $ BPT.Explicit IntSize64
  C.Magic $ LM.External [ptr, ptr, int64, int1] FCT.Void EN.memcpy [dest, src, byteCount, C.intValue0] []
