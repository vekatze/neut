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
import Codec.Binary.UTF8.String
import Control.Monad
import Control.Monad.Writer.Lazy
import Data.ByteString.Builder
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.IntMap qualified as IntMap
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Gensym.Gensym qualified as Gensym
import Gensym.Handle qualified as Gensym
import Kernel.Common.Allocator (Allocator, AllocatorSpec (..), allocatorSpec)
import Kernel.Common.Arch
import Kernel.Common.Arch qualified as A
import Kernel.Common.Const
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.Target
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
import Language.Common.LowType.ToByteSize (lowTypeToByteSize)
import Language.Common.PrimNumSize
import Language.Common.PrimOp
import Language.Common.PrimType qualified as PT
import Language.Comp.Comp qualified as C
import Language.Comp.CreateVar (createVar)
import Language.Comp.EnumCase qualified as EC
import Language.Comp.Reduce qualified as Reduce
import Language.Comp.Subst qualified as Subst
import Language.LowComp.DeclarationName qualified as DN
import Language.LowComp.LowComp qualified as LC
import Logger.Hint (internalHint)

data Handle = Handle
  { allocator :: Allocator,
    arch :: Arch,
    baseSize :: DS.DataSize,
    gensymHandle :: Gensym.Handle,
    envHandle :: Env.Handle,
    reduceHandle :: Reduce.Handle,
    substHandle :: Subst.Handle,
    declEnv :: IORef DN.DeclEnv,
    staticTextList :: IORef [(T.Text, (Builder, Int))],
    definedNameSet :: IORef (S.Set DD.DefiniteDescription),
    referencedNameSet :: IORef (S.Set DD.DefiniteDescription)
  }

new :: Global.Handle -> Target -> C.DefMap -> App Handle
new (Global.Handle {..}) target defMap = do
  allocator <- Env.getAllocatorByTarget envHandle target
  let arch = Platform.getArch platformHandle
  let baseSize = Platform.getDataSize platformHandle
  let substHandle = Subst.new gensymHandle
  let reduceHandle = Reduce.new substHandle gensymHandle defMap
  declEnv <- liftIO $ newIORef $ makeBaseDeclEnv arch (allocatorSpec allocator)
  staticTextList <- liftIO $ newIORef []
  definedNameSet <- liftIO $ newIORef S.empty
  referencedNameSet <- liftIO $ newIORef S.empty
  return $ Handle {..}

makeBaseDeclEnv :: Arch -> AllocatorSpec -> DN.DeclEnv
makeBaseDeclEnv arch spec = do
  Map.fromList $ flip map (defaultForeignList arch spec) $ \(F.Foreign _ name domList cod) -> do
    (DN.Ext name, (domList, cod))

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
  liftIO $ insDeclEnv h (DN.In mainDD) AN.zero (FCT.Cod BLT.Pointer)
  mainDef <- liftIO $ constructMainTerm h mainDD
  stmtList' <- catMaybes <$> mapM (lowerStmt h) stmtList
  LC.LowCodeMain mainDef <$> liftIO (summarize h stmtList')

summarize :: Handle -> [LC.Def] -> IO LC.LowCodeInfo
summarize h stmtList = do
  declEnv <- readIORef $ declEnv h
  staticTextList <- readIORef $ staticTextList h
  return (declEnv, stmtList, staticTextList)

optimize :: Handle -> LC.Comp -> IO LC.Comp
optimize h = do
  return . MallocFreeCancel.mallocFreeCancel
    >=> FreeMallocCancel.freeMallocCancel FreeMallocCancel.Exact (gensymHandle h)
    >=> FreeMallocCancel.freeMallocCancel FreeMallocCancel.Compatible (gensymHandle h)
    >=> HoistStackAlloc.hoistStackAlloc (gensymHandle h) (baseSize h)
    >=> return . CoercionCancel.coercionCancel
    >=> return . DeadLetElim.deadLetElim

lowerStmt :: Handle -> C.CompStmt -> App (Maybe LC.Def)
lowerStmt h stmt = do
  case stmt of
    C.Def name _ args e -> do
      e0 <- lowerComp h e
      e' <- liftIO $ optimize h e0
      return $ Just (name, LC.DefContent LT.Pointer args e')
    C.DefVoid name _ args e -> do
      e0 <- lowerComp h e
      e' <- liftIO $ optimize h e0
      return $ Just (name, LC.DefContent LT.Void args e')
    C.Foreign {} -> do
      return Nothing

registerInternalNames :: Handle -> [C.CompStmt] -> IO ()
registerInternalNames h stmtList =
  forM_ stmtList $ \stmt -> do
    case stmt of
      C.Def name _ _ _ -> do
        modifyIORef' (definedNameSet h) $ S.insert name
      C.DefVoid name _ _ _ -> do
        modifyIORef' (definedNameSet h) $ S.insert name
      C.Foreign foreignList ->
        forM_ foreignList $ \(F.Foreign _ name domList cod) -> do
          insDeclEnv' h (DN.Ext name) domList cod

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

constructMainTerm :: Handle -> DD.DefiniteDescription -> IO LC.DefContent
constructMainTerm h mainName = do
  argc <- Gensym.newIdentFromText (gensymHandle h) "argc"
  argv <- Gensym.newIdentFromText (gensymHandle h) "argv"
  let argcGlobal = LC.VarExternal (EN.ExternalName unsafeArgcName)
  let argvGlobal = LC.VarExternal (EN.ExternalName unsafeArgvName)
  let mainTerm =
        LC.Cont (LC.Store LT.Pointer (LC.VarLocal argc) argcGlobal) $
          LC.Cont (LC.Store LT.Pointer (LC.VarLocal argv) argvGlobal) $
            LC.Cont (LC.Call LT.Pointer (LC.VarGlobal mainName) []) $
              LC.Return (LC.Int 0)
  let mainType = LT.PrimNum $ PT.Int $ dataSizeToIntSize (baseSize h)
  return $ LC.DefContent mainType [argc, argv] mainTerm

lowerComp :: Handle -> C.Comp -> App LC.Comp
lowerComp h term =
  case term of
    C.Primitive theta -> do
      (resultVar, resultValue) <- liftIO $ newValueLocal h "result"
      lowerCompPrimitive h resultVar theta (LC.Return resultValue)
    C.PiElimDownElim _ v ds -> do
      (funcVar, func) <- liftIO $ newValueLocal h "func"
      (castFuncVar, castFunc) <- liftIO $ newValueLocal h "func"
      (argVars, argValues) <- mapAndUnzipM (const $ liftIO $ newValueLocal h "arg") ds
      lowerValue h funcVar v
        =<< lowerValues h (zip argVars ds)
        =<< cast h castFuncVar func LT.Pointer
        =<< return (LC.TailCall LT.Pointer castFunc (map (LT.Pointer,) argValues))
    C.SigmaElim shouldDeallocate xs v e -> do
      (sigmaVar, sigma) <- liftIO $ newValueLocal h "sigma"
      (elemVars, elems) <- mapAndUnzipM (const $ liftIO $ newValueLocal h "elem") xs
      let numOfElems = length xs
      let baseType = LT.Array numOfElems LT.Pointer
      lowerValue h sigmaVar v
        =<< return . getElemPtrList sigma elemVars baseType
        =<< loadElements h sigma (zip xs (map (,LT.Pointer) elems))
        =<< liftIO . freeIfNecessary h shouldDeallocate sigma (length xs)
        =<< lowerComp h e
    C.UpIntro d -> do
      (resultVar, resultValue) <- liftIO $ newValueLocal h "result"
      lowerValue h resultVar d (LC.Return resultValue)
    C.UpIntroVoid ->
      return LC.ReturnVoid
    C.UpElim _ x e1 e2 -> do
      e1' <- lowerComp h e1
      e2' <- lowerComp h e2
      return $ commConv x e1' e2'
    C.UpElimCallVoid f ds e2 -> do
      (funcVar, func) <- liftIO $ newValueLocal h "func"
      (castFuncVar, castFunc) <- liftIO $ newValueLocal h "func"
      (argVars, argValues) <- mapAndUnzipM (const $ liftIO $ newValueLocal h "arg") ds
      e2' <- lowerComp h e2
      lowerValue h funcVar f
        =<< lowerValues h (zip argVars ds)
        =<< cast h castFuncVar func LT.Pointer
        =<< return (LC.Cont (LC.Call LT.Void castFunc (map (LT.Pointer,) argValues)) e2')
    C.EnumElim fvInfo v defaultBranch branchList -> do
      let sub = IntMap.fromList fvInfo
      defaultBranch' <- liftIO $ Subst.subst (substHandle h) sub defaultBranch >>= Reduce.reduce (reduceHandle h)
      let (keys, clauses) = unzip branchList
      clauses' <- liftIO $ mapM (Subst.subst (substHandle h) sub >=> Reduce.reduce (reduceHandle h)) clauses
      defaultCase <- lowerEnumBranch h defaultBranch'
      caseList <-
        mapM
          ( \(tag, branch) -> do
              branch' <- lowerEnumBranch h branch
              return (enumCaseToInteger tag, branch')
          )
          (zip keys clauses')
      (phiName, phiValue) <- liftIO $ newValueLocal h "phi"
      let t = LT.PrimNum $ PT.Int $ dataSizeToIntSize (baseSize h)
      (castVar, castValue) <- liftIO $ newValueLocal h "cast"
      lowerValueLetCast h castVar v t
        =<< return (LC.Switch castValue t defaultCase caseList [phiName] (LC.Return phiValue))
    C.DestCall sizeComp f ds -> do
      sizeComp' <- liftIO $ Reduce.reduce (reduceHandle h) sizeComp
      liftIO (materializeDestCall h sizeComp' f ds)
        >>= liftIO . Reduce.reduce (reduceHandle h)
        >>= lowerComp h
    C.WriteToDest dest sizeComp result cont ->
      liftIO (materializeWriteToDest h dest sizeComp result cont)
        >>= liftIO . Reduce.reduce (reduceHandle h)
        >>= lowerComp h
    C.Free x size cont -> do
      freeID <- liftIO $ Gensym.newCount (gensymHandle h)
      (ptrVar, ptr) <- liftIO $ newValueLocal h "ptr"
      lowerValue h ptrVar x
        =<< return . LC.Cont (LC.Free ptr size freeID)
        =<< lowerComp h cont
    C.Unreachable ->
      return LC.Unreachable

materializeDestCall :: Handle -> C.Comp -> C.Value -> [C.Value] -> IO C.Comp
materializeDestCall h sizeComp f ds = do
  (sizeName, sizeVar) <- createVar (gensymHandle h) "size"
  (immediateDestName, immediateDestVar) <- createVar (gensymHandle h) "dest"
  (immediateResultName, immediateResultVar) <- createVar (gensymHandle h) "result"
  (boxedDestName, boxedDestVar) <- createVar (gensymHandle h) "dest"
  let immediateBody =
        C.UpElim
          True
          immediateDestName
          (C.Primitive $ C.Alloc $ C.Int IntSize64 (toInteger $ DS.reify (baseSize h) `div` 8))
          ( C.UpElimCallVoid
              f
              (immediateDestVar : ds)
              ( C.UpElim
                  True
                  immediateResultName
                  (C.Primitive $ C.Magic $ LM.Load BLT.Pointer immediateDestVar)
                  (C.Free immediateDestVar (Just (DS.reify (baseSize h) `div` 8)) (C.UpIntro immediateResultVar))
              )
          )
  let boxedBody size =
        C.UpElim
          True
          boxedDestName
          (C.Primitive $ C.Alloc size)
          (C.UpElimCallVoid f (boxedDestVar : ds) (C.UpIntro boxedDestVar))
  case sizeComp of
    C.UpIntro (C.Int _ v) -> do
      if v < 0
        then return immediateBody
        else return $ boxedBody $ C.Int IntSize64 v
    _ -> do
      return $
        C.UpElim
          True
          sizeName
          sizeComp
          ( C.EnumElim
              []
              sizeVar
              (boxedBody sizeVar)
              [(EC.Int (-1), immediateBody)]
          )

materializeWriteToDest :: Handle -> C.Value -> C.Comp -> C.Comp -> C.Comp -> IO C.Comp
materializeWriteToDest h dest sizeComp result cont = do
  (resultName, resultVar) <- createVar (gensymHandle h) "result"
  (sizeName, sizeVar) <- createVar (gensymHandle h) "size"
  (ignoredImmediate, _) <- createVar (gensymHandle h) "_"
  (ignoredBoxed, _) <- createVar (gensymHandle h) "_"
  (ignoredBranch, _) <- createVar (gensymHandle h) "_"
  let immediateBranch =
        C.UpElim
          True
          ignoredImmediate
          (C.Primitive $ C.Magic $ LM.Store BLT.Pointer C.null resultVar dest)
          (C.UpIntro C.null)
  let boxedBranch =
        C.UpElim
          True
          ignoredBoxed
          (C.Primitive $ C.Memcpy dest resultVar sizeVar)
          (C.Free resultVar (Just 0) (C.UpIntro C.null))
  let branch =
        C.EnumElim
          []
          sizeVar
          boxedBranch
          [(EC.Int (-1), immediateBranch)]
  return $
    C.UpElim False resultName result $
      C.UpElim True sizeName sizeComp $
        C.UpElim True ignoredBranch branch cont

lowerEnumBranch :: Handle -> C.Comp -> App LC.Comp
lowerEnumBranch h branch = do
  lowBranch <- lowerComp h branch
  (phiName, phiVar) <- liftIO $ newValueLocal h "phi"
  return $ commConv phiName lowBranch (LC.Phi [phiVar])

enumCaseToInteger :: EC.EnumCase -> Integer
enumCaseToInteger enumCase =
  case enumCase of
    EC.Int i ->
      i

lowerCompPrimitive :: Handle -> Ident -> C.Primitive -> LC.Comp -> App LC.Comp
lowerCompPrimitive h resultVar codeOp cont =
  case codeOp of
    C.PrimOp op vs ->
      lowerCompPrimOp h resultVar op vs cont
    C.ShiftPointer v size index -> do
      (ptrVar, ptr) <- liftIO $ newValueLocal h "func"
      let aggType = AggTypeArray (fromInteger size) LT.Pointer
      let indexList' = [(LC.Int 0, LT.PrimNum $ PT.Int IntSize32), (LC.Int index, LT.PrimNum $ PT.Int IntSize32)]
      lowerValue h ptrVar v
        =<< return (LC.Let resultVar (LC.GetElementPtr (ptr, toLowType aggType) indexList') cont)
    C.Calloc num size -> do
      byteCountVarName <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "size"
      let byteCountValue = C.VarLocal byteCountVarName
      numVarName <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "num"
      let numValue = C.VarLocal numVarName
      (castSizeVar, castSizeValue) <- liftIO $ newValueLocal h "size"
      (castNumVar, castNumValue) <- liftIO $ newValueLocal h "num"
      let lowInt = LT.PrimNum $ PT.Int $ dataSizeToIntSize (baseSize h)
      lowerValue h byteCountVarName size
        =<< lowerValue h numVarName num
        =<< lowerValueLetCast h castSizeVar byteCountValue lowInt
        =<< lowerValueLetCast h castNumVar numValue lowInt
        =<< return (LC.Let resultVar (LC.Calloc castNumValue castSizeValue) cont)
    C.Alloc size -> do
      allocID <- liftIO $ Gensym.newCount (gensymHandle h)
      case size of
        C.Int _ knownByteCount ->
          return $ LC.Let resultVar (LC.Alloc (Left knownByteCount) allocID) cont
        runtimeByteSize -> do
          byteCountVarName <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "size"
          let byteCountValue = C.VarLocal byteCountVarName
          (castVar, castValue) <- liftIO $ newValueLocal h "size"
          let lowInt = LT.PrimNum $ PT.Int $ dataSizeToIntSize (baseSize h)
          lowerValue h byteCountVarName runtimeByteSize
            =<< lowerValueLetCast h castVar byteCountValue lowInt
            =<< return (LC.Let resultVar (LC.Alloc (Right castValue) allocID) cont)
    C.Realloc ptr size -> do
      byteCountVarName <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "size"
      let byteCountValue = C.VarLocal byteCountVarName
      (castVar, castValue) <- liftIO $ newValueLocal h "size"
      (ptrVar, ptrValue) <- liftIO $ newValueLocal h "ptr"
      let lowInt = LT.PrimNum $ PT.Int $ dataSizeToIntSize (baseSize h)
      lowerValue h byteCountVarName size
        =<< lowerValueLetCast h castVar byteCountValue lowInt
        =<< lowerValueLetCast h ptrVar ptr LT.Pointer
        =<< return (LC.Let resultVar (LC.Realloc ptrValue castValue) cont)
    C.Memcpy dest src size -> do
      byteCountVarName <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "size"
      let byteCountValue = C.VarLocal byteCountVarName
      lowerValue h byteCountVarName size
        =<< lowerCompPrimitive h resultVar (memcpyExternal dest src byteCountValue) cont
    C.Magic der -> do
      case der of
        LM.Cast _ _ value -> do
          lowerValue h resultVar value cont
        LM.Store valueLowType _ value pointer -> do
          let valueLowType' = LT.fromBaseLowType valueLowType
          (valVar, val) <- liftIO $ newValueLocal h "val"
          (ptrVar, ptr) <- liftIO $ newValueLocal h "ptr"
          lowerValueLetCast h valVar value valueLowType'
            =<< lowerValueLetCast h ptrVar pointer LT.Pointer
            =<< return . LC.Let resultVar (LC.nop LC.Null)
            =<< return (LC.Cont (LC.Store valueLowType' val ptr) cont)
        LM.Load valueLowType pointer -> do
          let valueLowType' = LT.fromBaseLowType valueLowType
          (tmpVar, tmp) <- liftIO $ newValueLocal h "tmp"
          (ptrVar, ptrValue) <- liftIO $ newValueLocal h "ptr"
          lowerValueLetCast h ptrVar pointer LT.Pointer
            =<< return . LC.Let tmpVar (LC.Load ptrValue valueLowType')
            =<< uncast h resultVar tmp valueLowType' cont
        LM.Alloca t size -> do
          let t' = LT.fromBaseLowType t
          let indexType = LT.PrimNum $ PT.Int $ dataSizeToIntSize (baseSize h)
          (ptrVar, ptrValue) <- liftIO $ newValueLocal h "ptr"
          stackSlotID <- liftIO $ Gensym.newCount (gensymHandle h)
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
                =<< uncast h resultVar ptrValue LT.Pointer cont
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
                =<< uncast h resultVar ptrValue LT.Pointer cont
        LM.External domList cod name fixedArgs varArgAndTypeList -> do
          alreadyRegistered <- liftIO $ member h (DN.Ext name)
          unless alreadyRegistered $ do
            liftIO $ insDeclEnv' h (DN.Ext name) domList cod
          let (varArgs, varTypes) = unzip varArgAndTypeList
          let argCaster = map LT.fromBaseLowType $ domList ++ varTypes
          let suffix = if null varArgs then [] else [LT.VarArgs]
          let lowCod = F.fromForeignCodType cod
          let funcType = LT.Function (map LT.fromBaseLowType domList ++ suffix) lowCod
          let args = fixedArgs ++ varArgs
          case lowCod of
            LT.Void -> do
              (argVars, argValues) <- mapAndUnzipM (const $ liftIO $ newValueLocal h "arg") args
              lowerAndCastValues h (zip argVars (zip args argCaster))
                =<< return . LC.Let resultVar (LC.nop LC.Null)
                =<< return (LC.Cont (LC.MagicCall funcType (LC.VarExternal name) $ zip argCaster argValues) cont)
            _ -> do
              (tmpVar, tmpValue) <- liftIO $ newValueLocal h "tmp"
              (argVars, argValues) <- mapAndUnzipM (const $ liftIO $ newValueLocal h "arg") args
              lowerAndCastValues h (zip argVars (zip args argCaster))
                =<< return . LC.Let tmpVar (LC.MagicCall funcType (LC.VarExternal name) $ zip argCaster argValues)
                =<< uncast h resultVar tmpValue lowCod cont
        LM.Global name t -> do
          let t' = LT.fromBaseLowType t
          uncast h resultVar (LC.VarExternal name) t' cont
        LM.OpaqueValue e ->
          lowerValue h resultVar e cont
        LM.CallType func arg1 arg2 -> do
          (funcVar, funcValue) <- liftIO $ newValueLocal h "func"
          (arg1Var, arg1Value) <- liftIO $ newValueLocal h "arg1"
          (arg2Var, arg2Value) <- liftIO $ newValueLocal h "arg2"
          let arg1Type = LT.Pointer
          let arg2Type = LT.Pointer
          let resultType = LT.Pointer
          lowerValue h funcVar func
            =<< lowerValue h arg1Var arg1
            =<< lowerValue h arg2Var arg2
            =<< return . LC.Let resultVar (LC.Call resultType funcValue [(arg1Type, arg1Value), (arg2Type, arg2Value)])
            =<< return cont

lowerCompPrimOp :: Handle -> Ident -> PrimOp -> [C.Value] -> LC.Comp -> App LC.Comp
lowerCompPrimOp h resultVar op vs cont = do
  let (domList, cod) = getTypeInfo op
  (argVars, args) <- mapAndUnzipM (const $ liftIO $ newValueLocal h "arg") vs
  (tmpVar, tmp) <- liftIO $ newValueLocal h "tmp"
  lowerValueLetCastPrimArgs h (zip argVars (zip vs domList))
    =<< return . LC.Let tmpVar (LC.PrimOp op args)
    =<< uncast h resultVar tmp (LT.PrimNum cod) cont

lowerValueLetCastPrimArgs :: Handle -> [(Ident, (C.Value, PT.PrimType))] -> LC.Comp -> App LC.Comp
lowerValueLetCastPrimArgs h xdts cont =
  case xdts of
    [] ->
      return cont
    ((x, (d, t)) : rest) -> do
      lowerValueLetCast h x d (LT.PrimNum t)
        =<< lowerValueLetCastPrimArgs h rest cont

floatSizeToIntSize :: FloatSize -> IntSize
floatSizeToIntSize floatSize =
  case floatSize of
    FloatSize16 ->
      IntSize16
    FloatSize32 ->
      IntSize32
    FloatSize64 ->
      IntSize64

cast :: Handle -> Ident -> LC.Value -> LT.LowType -> LC.Comp -> App LC.Comp
cast h var v lowType cont = do
  case lowType of
    LT.PrimNum (PT.Int _) -> do
      return $ LC.Let var (LC.PointerToInt v lowType) cont
    LT.PrimNum (PT.Float size) -> do
      let floatType = LT.PrimNum $ PT.Float size
      let intType = LT.PrimNum $ PT.Int $ floatSizeToIntSize size
      (tmp, tmpVar) <- liftIO $ newValueLocal h "tmp"
      return $
        LC.Let tmp (LC.PointerToInt v intType) $
          LC.Let var (LC.Bitcast tmpVar intType floatType) cont
    _ -> do
      return $ LC.Let var (LC.Bitcast v LT.Pointer lowType) cont

uncast :: Handle -> Ident -> LC.Value -> LT.LowType -> LC.Comp -> App LC.Comp
uncast h var castedValue lowType cont = do
  case lowType of
    LT.PrimNum (PT.Int _) ->
      return $ LC.Let var (LC.IntToPointer castedValue lowType) cont
    LT.PrimNum (PT.Float i) -> do
      let floatType = LT.PrimNum $ PT.Float i
      let intType = LT.PrimNum $ PT.Int $ floatSizeToIntSize i
      (tmp, tmpVar) <- liftIO $ newValueLocal h "tmp"
      return $
        LC.Let tmp (LC.Bitcast castedValue floatType intType) $
          LC.Let var (LC.IntToPointer tmpVar intType) cont
    _ ->
      return $ LC.Let var (LC.Bitcast castedValue lowType LT.Pointer) cont

allocateBasePointer :: Handle -> Ident -> AggType -> LC.Comp -> App LC.Comp
allocateBasePointer h resultVar aggType cont = do
  let lt = toLowType aggType
  case lt of
    LT.Array 0 _ ->
      return $ LC.Let resultVar (LC.nop LC.Null) cont
    LT.Struct [] ->
      return $ LC.Let resultVar (LC.nop LC.Null) cont
    _ -> do
      allocID <- liftIO $ Gensym.newCount (gensymHandle h)
      let knownByteCount = aggTypeByteSize h aggType
      return $ LC.Let resultVar (LC.Alloc (Left knownByteCount) allocID) cont

createAggData ::
  Handle ->
  Ident ->
  AggType -> -- the type of the base pointer
  [(C.Value, LT.LowType)] ->
  LC.Comp ->
  App LC.Comp
createAggData h resultVar aggType dts cont = do
  (xs, vs) <- mapAndUnzipM (const $ liftIO $ newValueLocal h "item") dts
  let baseType = toLowType aggType
  allocateBasePointer h resultVar aggType
    =<< return . getElemPtrList (LC.VarLocal resultVar) xs baseType
    =<< storeElements h (LC.VarLocal resultVar) (zip vs dts) cont

storeElements ::
  Handle ->
  LC.Value -> -- base pointer
  [(LC.Value, (C.Value, LT.LowType))] ->
  LC.Comp ->
  App LC.Comp
storeElements h basePointer values cont =
  case values of
    [] ->
      return cont
    (elemPtr, (value, valueType)) : ids -> do
      (castVar, castValue) <- liftIO $ newValueLocal h "base"
      lowerValueLetCast h castVar value valueType
        =<< return . store valueType castValue elemPtr
        =<< storeElements h basePointer ids cont

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
      (castPtrVar, castPtrValue) <- liftIO $ newValueLocal h "castptr"
      uncast h castPtrVar valuePointer valueType
        =<< load h targetVar valueType castPtrValue
        =<< loadElements h basePointer rest cont

lowerValue :: Handle -> Ident -> C.Value -> LC.Comp -> App LC.Comp
lowerValue h resultVar v cont =
  case v of
    C.VarGlobal globalName argNum cod -> do
      liftIO $ insertReferencedName h globalName
      lowNameSet <- liftIO $ getDefinedNameSet h
      unless (S.member globalName lowNameSet) $ do
        liftIO $ insDeclEnv h (DN.In globalName) argNum cod
      uncast h resultVar (LC.VarGlobal globalName) LT.Pointer cont
    C.VarLocal y ->
      return $ LC.Let resultVar (LC.nop $ LC.VarLocal y) cont
    C.VarStaticText text -> do
      let i8s = encode $ T.unpack text
      let len = length i8s
      i <- liftIO $ Gensym.newCount (gensymHandle h)
      let name = "text;" <> T.pack (show i)
      let encodedText = foldMap (\w -> "\\" <> word8HexFixed w) i8s
      liftIO $ insertStaticText h name encodedText len
      uncast h resultVar (LC.VarTextName name) LT.Pointer cont
    C.SigmaIntro size ds -> do
      let arrayType = AggTypeArray size LT.Pointer
      createAggData h resultVar arrayType (map (,LT.Pointer) ds) cont
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
freeIfNecessary h shouldDeallocate pointer len cont = do
  if shouldDeallocate
    then do
      freeID <- Gensym.newCount (gensymHandle h)
      return $ LC.Cont (LC.Free pointer (Just (wordCountToByteSize h len)) freeID) cont
    else do
      return cont

wordCountToByteSize :: Handle -> Int -> Int
wordCountToByteSize h wordCount =
  wordCount * (DS.reify (baseSize h) `div` 8)

-- returns Nothing iff the branch list is empty
newValueLocal :: Handle -> T.Text -> IO (Ident, LC.Value)
newValueLocal h name = do
  x <- Gensym.newIdentFromText (gensymHandle h) name
  return (x, LC.VarLocal x)

insDeclEnv :: Handle -> DN.DeclarationName -> AN.ArgNum -> FCT.ForeignCodType BLT.BaseLowType -> IO ()
insDeclEnv h k argNum cod = do
  modifyIORef' (declEnv h) $ Map.insert k (BLT.toVoidPtrSeq argNum, cod)

insDeclEnv' :: Handle -> DN.DeclarationName -> [BLT.BaseLowType] -> FCT.ForeignCodType BLT.BaseLowType -> IO ()
insDeclEnv' h k domList cod = do
  modifyIORef' (declEnv h) $ Map.insert k (domList, cod)

member :: Handle -> DN.DeclarationName -> IO Bool
member h k = do
  denv <- readIORef (declEnv h)
  return $ Map.member k denv

insertStaticText :: Handle -> T.Text -> Builder -> Int -> IO ()
insertStaticText h name text len =
  modifyIORef' (staticTextList h) $ (:) (name, (text, len))

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

getElemPtrList :: LC.Value -> [Ident] -> LT.LowType -> LC.Comp -> LC.Comp
getElemPtrList basePointer vars baseType cont = do
  let f (var, i) = getElemPtr var basePointer baseType [0, toInteger i]
  foldr f cont (zip vars [0 :: Int ..])

data AggType
  = AggTypeArray Int LT.LowType
  | AggTypeStruct [LT.LowType]

toLowType :: AggType -> LT.LowType
toLowType aggType =
  case aggType of
    AggTypeArray i t ->
      LT.Array i t
    AggTypeStruct ts ->
      LT.Struct ts

aggTypeByteSize :: Handle -> AggType -> Integer
aggTypeByteSize h aggType =
  lowTypeByteSize h (toLowType aggType)

lowTypeByteSize :: Handle -> LT.LowType -> Integer
lowTypeByteSize h =
  lowTypeToByteSize (baseSize h)

commConv :: Ident -> LC.Comp -> LC.Comp -> LC.Comp
commConv x lowComp cont2 =
  case lowComp of
    LC.Return d ->
      LC.Let x (LC.nop d) cont2
    LC.ReturnVoid ->
      LC.Unreachable -- shouldn't occur
    LC.Let y op cont1 -> do
      let cont = commConv x cont1 cont2
      LC.Let y op cont
    LC.Cont op cont1 -> do
      let cont = commConv x cont1 cont2
      LC.Cont op cont
    LC.Switch d t defaultCase caseList phiVars cont -> do
      let cont' = commConv x cont cont2
      LC.Switch d t defaultCase caseList phiVars cont'
    LC.TailCall codType d ds ->
      LC.Let x (LC.Call codType d ds) cont2
    LC.Unreachable ->
      LC.Unreachable
    LC.Phi _ ->
      LC.Unreachable -- shouldn't occur

defaultForeignList :: A.Arch -> AllocatorSpec -> [F.Foreign]
defaultForeignList arch spec =
  [ F.Foreign internalHint (EN.ExternalName $ callocName spec) [getWordType arch, getWordType arch] (FCT.Cod BLT.Pointer),
    F.Foreign internalHint (EN.ExternalName $ mallocName spec) [getWordType arch] (FCT.Cod BLT.Pointer),
    F.Foreign internalHint (EN.ExternalName $ reallocName spec) [BLT.Pointer, getWordType arch] (FCT.Cod BLT.Pointer),
    F.Foreign internalHint (EN.ExternalName $ freeName spec) [BLT.Pointer] FCT.Void
  ]

getWordType :: A.Arch -> BLT.BaseLowType
getWordType arch =
  BLT.PrimNum $ BPT.Int $ BPT.Explicit $ dataSizeToIntSize $ A.dataSizeOf arch

memcpyExternal :: C.Value -> C.Value -> C.Value -> C.Primitive
memcpyExternal dest src byteCount = do
  let ptr = BLT.Pointer
  let int1 = BLT.PrimNum $ BPT.Int $ BPT.Explicit IntSize1
  let int64 = BLT.PrimNum $ BPT.Int $ BPT.Explicit IntSize64
  C.Magic $ LM.External [ptr, ptr, int64, int1] FCT.Void EN.memcpy [dest, src, byteCount, C.intValue0] []
