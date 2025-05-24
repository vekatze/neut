{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Monad law, left identity" #-}

module Kernel.Lower.Move.Lower
  ( Handle,
    new,
    lower,
    lowerEntryPoint,
  )
where

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
import Error.Rule.EIO (EIO)
import Gensym.Move.Gensym qualified as Gensym
import Gensym.Rule.Handle qualified as Gensym
import Kernel.Clarify.Move.Internal.Handle.CompDef qualified as CompDef
import Kernel.Common.Move.CreateGlobalHandle qualified as Global
import Kernel.Common.Move.Handle.Global.Env qualified as Env
import Kernel.Common.Move.Handle.Global.Platform qualified as Platform
import Kernel.Common.Rule.Arch
import Kernel.Common.Rule.Arch qualified as A
import Kernel.Common.Rule.Const
import Kernel.Common.Rule.Handle.Global.Env qualified as Env
import Kernel.Common.Rule.Target
import Kernel.Lower.Rule.Cancel
import Language.Common.Move.CreateSymbol qualified as Gensym
import Language.Common.Rule.ArgNum qualified as AN
import Language.Common.Rule.BaseLowType qualified as BLT
import Language.Common.Rule.BasePrimType qualified as BPT
import Language.Common.Rule.DataSize qualified as DS
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.ExternalName qualified as EN
import Language.Common.Rule.Foreign qualified as F
import Language.Common.Rule.ForeignCodType qualified as F
import Language.Common.Rule.ForeignCodType qualified as FCT
import Language.Common.Rule.Ident
import Language.Common.Rule.LowType qualified as LT
import Language.Common.Rule.LowType.FromBaseLowType qualified as LT
import Language.Common.Rule.Magic qualified as M
import Language.Common.Rule.PrimNumSize
import Language.Common.Rule.PrimNumSize.ToInt
import Language.Common.Rule.PrimOp
import Language.Common.Rule.PrimType qualified as PT
import Language.Comp.Move.Reduce qualified as Reduce
import Language.Comp.Move.Subst qualified as Subst
import Language.Comp.Rule.Comp qualified as C
import Language.Comp.Rule.EnumCase qualified as EC
import Language.LowComp.Rule.DeclarationName qualified as DN
import Language.LowComp.Rule.LowComp qualified as LC
import Logger.Rule.Hint (internalHint)

data Handle = Handle
  { arch :: Arch,
    baseSize :: Int,
    gensymHandle :: Gensym.Handle,
    envHandle :: Env.Handle,
    reduceHandle :: Reduce.Handle,
    substHandle :: Subst.Handle,
    declEnv :: IORef DN.DeclEnv,
    staticTextList :: IORef [(T.Text, (Builder, Int))],
    definedNameSet :: IORef (S.Set DD.DefiniteDescription)
  }

new :: Global.Handle -> IO Handle
new (Global.Handle {..}) = do
  let arch = Platform.getArch platformHandle
  let baseSize = Platform.getDataSizeValue platformHandle
  let substHandle = Subst.new gensymHandle
  defMap <- CompDef.get compDefHandle
  let reduceHandle = Reduce.new substHandle gensymHandle defMap
  declEnv <- liftIO $ newIORef $ makeBaseDeclEnv arch
  staticTextList <- liftIO $ newIORef []
  definedNameSet <- liftIO $ newIORef S.empty
  return $ Handle {..}

makeBaseDeclEnv :: Arch -> DN.DeclEnv
makeBaseDeclEnv arch = do
  Map.fromList $ flip map (defaultForeignList arch) $ \(F.Foreign _ name domList cod) -> do
    (DN.Ext name, (domList, cod))

lower :: Handle -> [C.CompStmt] -> EIO LC.LowCode
lower h stmtList = do
  liftIO $ registerInternalNames h stmtList
  liftIO $ insDeclEnv h (DN.In DD.imm) AN.argNumS4
  liftIO $ insDeclEnv h (DN.In DD.cls) AN.argNumS4
  stmtList' <- catMaybes <$> mapM (lowerStmt h) stmtList
  LC.LowCodeNormal <$> liftIO (summarize h stmtList')

lowerEntryPoint :: Handle -> MainTarget -> [C.CompStmt] -> EIO LC.LowCode
lowerEntryPoint h target stmtList = do
  liftIO $ registerInternalNames h stmtList
  mainDD <- Env.getMainDefiniteDescriptionByTarget (envHandle h) target
  liftIO $ insDeclEnv h (DN.In mainDD) AN.zero
  mainDef <- liftIO $ constructMainTerm h mainDD
  stmtList' <- catMaybes <$> mapM (lowerStmt h) stmtList
  LC.LowCodeMain mainDef <$> liftIO (summarize h stmtList')

summarize :: Handle -> [LC.Def] -> IO LC.LowCodeInfo
summarize h stmtList = do
  declEnv <- readIORef $ declEnv h
  staticTextList <- readIORef $ staticTextList h
  return (declEnv, stmtList, staticTextList)

lowerStmt :: Handle -> C.CompStmt -> EIO (Maybe (DD.DefiniteDescription, ([Ident], LC.Comp)))
lowerStmt h stmt = do
  case stmt of
    C.Def name _ args e -> do
      e' <- lowerComp h e >>= liftIO . return . cancel
      return $ Just (name, (args, e'))
    C.Foreign {} -> do
      return Nothing

registerInternalNames :: Handle -> [C.CompStmt] -> IO ()
registerInternalNames h stmtList =
  forM_ stmtList $ \stmt -> do
    case stmt of
      C.Def name _ _ _ -> do
        modifyIORef' (definedNameSet h) $ S.insert name
      C.Foreign foreignList ->
        forM_ foreignList $ \(F.Foreign _ name domList cod) -> do
          insDeclEnv' h (DN.Ext name) domList cod

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
  return ([argc, argv], mainTerm)

lowerComp :: Handle -> C.Comp -> EIO LC.Comp
lowerComp h term =
  case term of
    C.Primitive theta -> do
      (resultVar, resultValue) <- liftIO $ newValueLocal h "result"
      lowerCompPrimitive h resultVar theta (LC.Return resultValue)
    C.PiElimDownElim v ds -> do
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
    C.UpElim _ x e1 e2 -> do
      e1' <- lowerComp h e1
      e2' <- lowerComp h e2
      return $ commConv x e1' e2'
    C.EnumElim fvInfo v defaultBranch branchList -> do
      let sub = IntMap.fromList fvInfo
      defaultBranch' <- liftIO $ Subst.subst (substHandle h) sub defaultBranch >>= Reduce.reduce (reduceHandle h)
      let (keys, clauses) = unzip branchList
      clauses' <- liftIO $ mapM (Subst.subst (substHandle h) sub >=> Reduce.reduce (reduceHandle h)) clauses
      let branchList' = zip keys clauses'
      case (defaultBranch', clauses') of
        (C.Unreachable, [clause]) ->
          lowerComp h clause
        (_, []) ->
          lowerComp h defaultBranch'
        _ -> do
          (defaultCase, caseList) <- constructSwitch h defaultBranch' branchList'
          let t = LT.PrimNum $ PT.Int $ IntSize (baseSize h)
          (castVar, castValue) <- liftIO $ newValueLocal h "cast"
          (phiVar, phi) <- liftIO $ newValueLocal h "phi"
          lowerValueLetCast h castVar v t
            =<< return (LC.Switch (castValue, t) defaultCase caseList (phiVar, LC.Return phi))
    C.Free x size cont -> do
      freeID <- liftIO $ Gensym.newCount (gensymHandle h)
      (ptrVar, ptr) <- liftIO $ newValueLocal h "ptr"
      lowerValue h ptrVar x
        =<< return . LC.Cont (LC.Free ptr size freeID)
        =<< lowerComp h cont
    C.Unreachable ->
      return LC.Unreachable

lowerCompPrimitive :: Handle -> Ident -> C.Primitive -> LC.Comp -> EIO LC.Comp
lowerCompPrimitive h resultVar codeOp cont =
  case codeOp of
    C.PrimOp op vs ->
      lowerCompPrimOp h resultVar op vs cont
    C.Magic der -> do
      case der of
        M.Cast _ _ value -> do
          lowerValue h resultVar value cont
        M.Store valueLowType _ value pointer -> do
          let valueLowType' = LT.fromBaseLowType valueLowType
          (valVar, val) <- liftIO $ newValueLocal h "val"
          (ptrVar, ptr) <- liftIO $ newValueLocal h "ptr"
          lowerValueLetCast h valVar value valueLowType'
            =<< lowerValueLetCast h ptrVar pointer LT.Pointer
            =<< return . LC.Let resultVar (LC.nop LC.Null)
            =<< return (LC.Cont (LC.Store valueLowType' val ptr) cont)
        M.Load valueLowType pointer -> do
          let valueLowType' = LT.fromBaseLowType valueLowType
          (tmpVar, tmp) <- liftIO $ newValueLocal h "tmp"
          (ptrVar, ptrValue) <- liftIO $ newValueLocal h "ptr"
          lowerValueLetCast h ptrVar pointer LT.Pointer
            =<< return . LC.Let tmpVar (LC.Load ptrValue valueLowType')
            =<< uncast h resultVar tmp valueLowType' cont
        M.Alloca t size -> do
          let t' = LT.fromBaseLowType t
          let indexType = LT.PrimNum $ PT.Int $ IntSize (baseSize h)
          (sizeVar, sizeValue) <- liftIO $ newValueLocal h "size"
          (ptrVar, ptrValue) <- liftIO $ newValueLocal h "ptr"
          lowerValueLetCast h sizeVar size indexType
            =<< return . LC.Let ptrVar (LC.StackAlloc t' indexType sizeValue)
            =<< uncast h resultVar ptrValue LT.Pointer cont
        M.External domList cod name fixedArgs varArgAndTypeList -> do
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
        M.Global name t -> do
          let t' = LT.fromBaseLowType t
          uncast h resultVar (LC.VarExternal name) t' cont
        M.OpaqueValue e ->
          lowerValue h resultVar e cont

lowerCompPrimOp :: Handle -> Ident -> PrimOp -> [C.Value] -> LC.Comp -> EIO LC.Comp
lowerCompPrimOp h resultVar op vs cont = do
  let (domList, cod) = getTypeInfo op
  (argVars, args) <- mapAndUnzipM (const $ liftIO $ newValueLocal h "arg") vs
  (tmpVar, tmp) <- liftIO $ newValueLocal h "tmp"
  lowerValueLetCastPrimArgs h (zip argVars (zip vs domList))
    =<< return . LC.Let tmpVar (LC.PrimOp op args)
    =<< uncast h resultVar tmp (LT.PrimNum cod) cont

lowerValueLetCastPrimArgs :: Handle -> [(Ident, (C.Value, PT.PrimType))] -> LC.Comp -> EIO LC.Comp
lowerValueLetCastPrimArgs h xdts cont =
  case xdts of
    [] ->
      return cont
    ((x, (d, t)) : rest) -> do
      lowerValueLetCast h x d (LT.PrimNum t)
        =<< lowerValueLetCastPrimArgs h rest cont

cast :: Handle -> Ident -> LC.Value -> LT.LowType -> LC.Comp -> EIO LC.Comp
cast h var v lowType cont = do
  case lowType of
    LT.PrimNum (PT.Int _) -> do
      return $ LC.Let var (LC.PointerToInt v LT.Pointer lowType) cont
    LT.PrimNum (PT.Float size) -> do
      let floatType = LT.PrimNum $ PT.Float size
      let intType = LT.PrimNum $ PT.Int $ IntSize $ floatSizeToInt size
      (tmp, tmpVar) <- liftIO $ newValueLocal h "tmp"
      return $
        LC.Let tmp (LC.PointerToInt v LT.Pointer intType) $
          LC.Let var (LC.Bitcast tmpVar intType floatType) cont
    _ -> do
      return $ LC.Let var (LC.Bitcast v LT.Pointer lowType) cont

uncast :: Handle -> Ident -> LC.Value -> LT.LowType -> LC.Comp -> EIO LC.Comp
uncast h var castedValue lowType cont = do
  case lowType of
    LT.PrimNum (PT.Int _) ->
      return $ LC.Let var (LC.IntToPointer castedValue lowType LT.Pointer) cont
    LT.PrimNum (PT.Float i) -> do
      let floatType = LT.PrimNum $ PT.Float i
      let intType = LT.PrimNum $ PT.Int $ IntSize $ floatSizeToInt i
      (tmp, tmpVar) <- liftIO $ newValueLocal h "tmp"
      return $
        LC.Let tmp (LC.Bitcast castedValue floatType intType) $
          LC.Let var (LC.IntToPointer tmpVar intType LT.Pointer) cont
    _ ->
      return $ LC.Let var (LC.Bitcast castedValue lowType LT.Pointer) cont

allocateBasePointer :: Handle -> Ident -> AggType -> LC.Comp -> EIO LC.Comp
allocateBasePointer h resultVar aggType cont = do
  let lt = toLowType aggType
  case lt of
    LT.Array 0 _ ->
      return $ LC.Let resultVar (LC.nop LC.Null) cont
    LT.Struct [] ->
      return $ LC.Let resultVar (LC.nop LC.Null) cont
    _ -> do
      let (elemType, len) = getSizeInfoOf aggType
      (sizeVar, sizeValue) <- liftIO $ newValueLocal h "result"
      (castVar, castValue) <- liftIO $ newValueLocal h "result"
      allocID <- liftIO $ Gensym.newCount (gensymHandle h)
      let lowInt = LT.PrimNum $ PT.Int $ IntSize (baseSize h)
      return . getElemPtr sizeVar LC.Null elemType [toInteger len]
        =<< cast h castVar sizeValue lowInt
        =<< return (LC.Let resultVar (LC.Alloc castValue len allocID) cont)

createAggData ::
  Handle ->
  Ident ->
  AggType -> -- the type of the base pointer
  [(C.Value, LT.LowType)] ->
  LC.Comp ->
  EIO LC.Comp
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
  EIO LC.Comp
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

load :: Handle -> Ident -> LT.LowType -> LC.Value -> LC.Comp -> EIO LC.Comp
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
  EIO LC.Comp
loadElements h basePointer values cont =
  case values of
    [] -> do
      return cont
    (targetVar, (valuePointer, valueType)) : rest -> do
      (castPtrVar, castPtrValue) <- liftIO $ newValueLocal h "castptr"
      uncast h castPtrVar valuePointer valueType
        =<< load h targetVar valueType castPtrValue
        =<< loadElements h basePointer rest cont

lowerValue :: Handle -> Ident -> C.Value -> LC.Comp -> EIO LC.Comp
lowerValue h resultVar v cont =
  case v of
    C.VarGlobal globalName argNum -> do
      lowNameSet <- liftIO $ getDefinedNameSet h
      unless (S.member globalName lowNameSet) $ do
        liftIO $ insDeclEnv h (DN.In globalName) argNum
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
    C.SigmaIntro ds -> do
      let arrayType = AggTypeArray (length ds) LT.Pointer
      createAggData h resultVar arrayType (map (,LT.Pointer) ds) cont
    C.Int size l -> do
      uncast h resultVar (LC.Int l) (LT.PrimNum $ PT.Int size) cont
    C.Float size f -> do
      uncast h resultVar (LC.Float size f) (LT.PrimNum $ PT.Float size) cont

lowerValues :: Handle -> [(Ident, C.Value)] -> LC.Comp -> EIO LC.Comp
lowerValues h xvs cont =
  case xvs of
    [] ->
      return cont
    (x, v) : rest -> do
      lowerValue h x v
        =<< lowerValues h rest cont

lowerAndCastValues :: Handle -> [(Ident, (C.Value, LT.LowType))] -> LC.Comp -> EIO LC.Comp
lowerAndCastValues h xvs cont =
  case xvs of
    [] ->
      return cont
    (x, (v, t)) : rest -> do
      lowerValueLetCast h x v t
        =<< lowerAndCastValues h rest cont

lowerValueLetCast :: Handle -> Ident -> C.Value -> LT.LowType -> LC.Comp -> EIO LC.Comp
lowerValueLetCast h resultVar v lowType cont = do
  (tmpVar, tmpValue) <- liftIO $ newValueLocal h "tmp"
  lowerValue h tmpVar v
    =<< cast h resultVar tmpValue lowType cont

freeIfNecessary :: Handle -> Bool -> LC.Value -> Int -> LC.Comp -> IO LC.Comp
freeIfNecessary h shouldDeallocate pointer len cont = do
  if shouldDeallocate
    then do
      freeID <- Gensym.newCount (gensymHandle h)
      return $ LC.Cont (LC.Free pointer len freeID) cont
    else do
      return cont

-- returns Nothing iff the branch list is empty
constructSwitch :: Handle -> C.Comp -> [(EC.EnumCase, C.Comp)] -> EIO (LC.Comp, [(Integer, LC.Comp)])
constructSwitch h defaultBranch switch =
  case switch of
    [] -> do
      defaultBranch' <- lowerComp h defaultBranch
      return (defaultBranch', [])
    (EC.Int i, code) : rest -> do
      code' <- lowerComp h code
      (defaultBranch', caseList) <- constructSwitch h defaultBranch rest
      return (defaultBranch', (i, code') : caseList)

newValueLocal :: Handle -> T.Text -> IO (Ident, LC.Value)
newValueLocal h name = do
  x <- Gensym.newIdentFromText (gensymHandle h) name
  return (x, LC.VarLocal x)

insDeclEnv :: Handle -> DN.DeclarationName -> AN.ArgNum -> IO ()
insDeclEnv h k argNum = do
  modifyIORef' (declEnv h) $ Map.insert k (BLT.toVoidPtrSeq argNum, FCT.Void)

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

getElemPtr :: Ident -> LC.Value -> LT.LowType -> [Integer] -> LC.Comp -> LC.Comp
getElemPtr var value valueType indexList cont = do
  let indexList' = map (\i -> (LC.Int i, LT.PrimNum $ PT.Int intSize32)) indexList
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

getSizeInfoOf :: AggType -> (LT.LowType, Int)
getSizeInfoOf aggType =
  case aggType of
    AggTypeArray len t ->
      (t, len)
    AggTypeStruct ts ->
      (LT.Struct ts, 1)

commConv :: Ident -> LC.Comp -> LC.Comp -> LC.Comp
commConv x lowComp cont2 =
  case lowComp of
    LC.Return d ->
      LC.Let x (LC.nop d) cont2 -- nop
    LC.Let y op cont1 -> do
      let cont = commConv x cont1 cont2
      LC.Let y op cont
    LC.Cont op cont1 -> do
      let cont = commConv x cont1 cont2
      LC.Cont op cont
    LC.Switch (d, t) defaultCase caseList (phiVar, cont) -> do
      let cont' = commConv x cont cont2
      LC.Switch (d, t) defaultCase caseList (phiVar, cont')
    LC.TailCall codType d ds ->
      LC.Let x (LC.Call codType d ds) cont2
    LC.Unreachable ->
      LC.Unreachable

defaultForeignList :: A.Arch -> [F.Foreign]
defaultForeignList arch =
  [ F.Foreign internalHint EN.malloc [getWordType arch] (FCT.Cod BLT.Pointer),
    F.Foreign internalHint EN.free [BLT.Pointer] FCT.Void
  ]

getWordType :: A.Arch -> BLT.BaseLowType
getWordType arch =
  BLT.PrimNum $ BPT.Int $ BPT.Explicit $ IntSize $ DS.reify $ A.dataSizeOf arch
