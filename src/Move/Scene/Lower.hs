{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Monad law, left identity" #-}

module Move.Scene.Lower
  ( lower,
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
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Context.Gensym qualified as Gensym
import Move.Context.Locator qualified as Locator
import Move.Scene.Cancel
import Move.Scene.Comp.Reduce qualified as C
import Move.Scene.Comp.Subst qualified as Subst
import Rule.ArgNum qualified as AN
import Rule.BaseLowType qualified as BLT
import Rule.BaseName qualified as BN
import Rule.Comp qualified as C
import Rule.Const
import Rule.DeclarationName qualified as DN
import Rule.DefiniteDescription qualified as DD
import Rule.EnumCase qualified as EC
import Rule.ExternalName qualified as EN
import Rule.Foreign qualified as F
import Rule.ForeignCodType qualified as F
import Rule.ForeignCodType qualified as FCT
import Rule.Ident
import Rule.LowComp qualified as LC
import Rule.LowType qualified as LT
import Rule.LowType.FromBaseLowType qualified as LT
import Rule.Magic qualified as M
import Rule.PrimNumSize
import Rule.PrimNumSize.ToInt
import Rule.PrimOp
import Rule.PrimType qualified as PT
import Rule.Target

type Lower = WriterT Cont App

newtype Cont = Cont (LC.Comp -> App LC.Comp)

instance Semigroup Cont where
  Cont newCont <> Cont oldCont =
    Cont $ oldCont >=> newCont

instance Monoid Cont where
  mempty =
    Cont return

data Handle
  = Handle
  { declEnv :: IORef DN.DeclEnv,
    staticTextList :: IORef [(DD.DefiniteDescription, (Builder, Int))],
    definedNameSet :: IORef (S.Set DD.DefiniteDescription)
  }

extend :: (LC.Comp -> App LC.Comp) -> Lower ()
extend =
  tell . Cont

runLower :: Lower LC.Value -> App LC.Comp
runLower m = do
  (a, Cont b) <- runWriterT m
  b $ LC.Return a

runLowerComp :: Lower LC.Comp -> App LC.Comp
runLowerComp m = do
  (a, Cont b) <- runWriterT m
  b a

new :: [C.CompStmt] -> App Handle
new stmtList = do
  declEnv <- liftIO $ newIORef Map.empty
  staticTextList <- liftIO $ newIORef []
  definedNameSet <- liftIO $ newIORef S.empty
  let h = Handle {..}
  arch <- toApp $ Env.getArch Nothing
  forM_ (F.defaultForeignList arch) $ \(F.Foreign _ name domList cod) -> do
    liftIO $ insDeclEnv' h (DN.Ext name) domList cod
  registerInternalNames h stmtList
  return h

lower ::
  [C.CompStmt] ->
  App LC.LowCode
lower stmtList = do
  h <- new stmtList
  liftIO $ insDeclEnv h (DN.In DD.imm) AN.argNumS4
  liftIO $ insDeclEnv h (DN.In DD.cls) AN.argNumS4
  stmtList' <- catMaybes <$> mapM (lowerStmt h) stmtList
  LC.LowCodeNormal <$> summarize h stmtList'

lowerEntryPoint :: MainTarget -> [C.CompStmt] -> App LC.LowCode
lowerEntryPoint target stmtList = do
  h <- new stmtList
  locatorHandle <- Locator.new
  mainDD <- toApp $ Locator.getMainDefiniteDescriptionByTarget locatorHandle target
  liftIO $ insDeclEnv h (DN.In mainDD) AN.zero
  mainDef <- constructMainTerm mainDD
  stmtList' <- catMaybes <$> mapM (lowerStmt h) stmtList
  LC.LowCodeMain mainDef <$> summarize h stmtList'

summarize :: Handle -> [LC.Def] -> App LC.LowCodeInfo
summarize h stmtList = do
  declEnv <- liftIO $ readIORef $ declEnv h
  staticTextList <- liftIO $ readIORef $ staticTextList h
  return (declEnv, stmtList, staticTextList)

lowerStmt :: Handle -> C.CompStmt -> App (Maybe (DD.DefiniteDescription, ([Ident], LC.Comp)))
lowerStmt h stmt = do
  case stmt of
    C.Def name _ args e -> do
      e' <- lowerComp h e >>= liftIO . return . cancel
      return $ Just (name, (args, e'))
    C.Foreign {} -> do
      return Nothing

registerInternalNames :: Handle -> [C.CompStmt] -> App ()
registerInternalNames h stmtList =
  forM_ stmtList $ \stmt -> do
    case stmt of
      C.Def name _ _ _ -> do
        liftIO $ modifyIORef' (definedNameSet h) $ S.insert name
      C.Foreign foreignList ->
        forM_ foreignList $ \(F.Foreign _ name domList cod) -> do
          liftIO $ insDeclEnv' h (DN.Ext name) domList cod

constructMainTerm :: DD.DefiniteDescription -> App LC.DefContent
constructMainTerm mainName = do
  argc <- Gensym.newIdentFromText "argc"
  argv <- Gensym.newIdentFromText "argv"
  let argcGlobal = LC.VarExternal (EN.ExternalName unsafeArgcName)
  let argvGlobal = LC.VarExternal (EN.ExternalName unsafeArgvName)
  let mainTerm =
        LC.Cont (LC.Store LT.Pointer (LC.VarLocal argc) argcGlobal) $
          LC.Cont (LC.Store LT.Pointer (LC.VarLocal argv) argvGlobal) $
            LC.Cont (LC.Call LT.Pointer (LC.VarGlobal mainName) []) $
              LC.Return (LC.Int 0)
  return ([argc, argv], mainTerm)

lowerComp :: Handle -> C.Comp -> App LC.Comp
lowerComp h term =
  case term of
    C.Primitive theta ->
      runLower $ lowerCompPrimitive h theta
    C.PiElimDownElim v ds -> do
      (funcVar, func) <- newValueLocal "func"
      (castFuncVar, castFunc) <- newValueLocal "func"
      (argVars, argValues) <- mapAndUnzipM (const $ newValueLocal "arg") ds
      lowerValue' h funcVar v
        =<< lowerValues h (zip argVars ds)
        =<< cast' castFuncVar func LT.Pointer
        =<< return (LC.TailCall LT.Pointer castFunc (map (LT.Pointer,) argValues))
    C.SigmaElim shouldDeallocate xs v e -> do
      (sigmaVar, sigma) <- newValueLocal "sigma"
      (elemVars, elems) <- mapAndUnzipM (const $ newValueLocal "elem") xs
      let numOfElems = length xs
      let baseType = LT.Array numOfElems LT.Pointer
      lowerValue' h sigmaVar v
        =<< return . getElemPtrList' sigma elemVars baseType
        =<< loadElements' sigma (zip xs (map (,LT.Pointer) elems))
        =<< freeOrNop shouldDeallocate sigma (length xs)
        =<< lowerComp h e
    C.UpIntro d -> do
      (result, resultVar) <- newValueLocal "result"
      lowerValue' h result d (LC.Return resultVar)
    C.UpElim _ x e1 e2 -> do
      e1' <- lowerComp h e1
      e2' <- lowerComp h e2
      return $ commConv x e1' e2'
    C.EnumElim fvInfo v defaultBranch branchList -> do
      let sub = IntMap.fromList fvInfo
      hred <- C.new
      hsub <- Subst.new
      defaultBranch' <- liftIO $ Subst.subst hsub sub defaultBranch >>= C.reduce hred
      let (keys, clauses) = unzip branchList
      clauses' <- liftIO $ mapM (Subst.subst hsub sub >=> C.reduce hred) clauses
      let branchList' = zip keys clauses'
      case (defaultBranch', clauses') of
        (C.Unreachable, [clause]) ->
          lowerComp h clause
        (_, []) ->
          lowerComp h defaultBranch'
        _ -> do
          (defaultCase, caseList) <- constructSwitch h defaultBranch' branchList'
          runLowerComp $ do
            baseSize <- lift $ toApp Env.getBaseSize'
            let t = LT.PrimNum $ PT.Int $ IntSize baseSize
            castedValue <- lowerValueLetCast h v t
            (phi, phiVar) <- lift $ newValueLocal "phi"
            return $ LC.Switch (castedValue, t) defaultCase caseList (phi, LC.Return phiVar)
    C.Free x size cont -> do
      cont' <- lowerComp h cont
      runLowerComp $ do
        x' <- lowerValue h x
        freeID <- lift Gensym.newCount
        return $ LC.Cont (LC.Free x' size freeID) cont'
    C.Unreachable ->
      return LC.Unreachable

store :: LT.LowType -> LC.Value -> LC.Value -> Lower ()
store lowType value pointer =
  reflectCont $ LC.Store lowType value pointer

lowerCompPrimitive :: Handle -> C.Primitive -> Lower LC.Value
lowerCompPrimitive h codeOp =
  case codeOp of
    C.PrimOp op vs ->
      lowerCompPrimOp h op vs
    C.Magic der -> do
      case der of
        M.Cast _ _ value -> do
          lowerValue h value
        M.Store valueLowType _ value pointer -> do
          let valueLowType' = LT.fromBaseLowType valueLowType
          valVar <- lowerValueLetCast h value valueLowType'
          ptrVar <- lowerValueLetCast h pointer LT.Pointer
          extend $ return . LC.Cont (LC.Store valueLowType' valVar ptrVar)
          return LC.Null
        M.Load valueLowType pointer -> do
          let valueLowType' = LT.fromBaseLowType valueLowType
          castedPointer <- lowerValueLetCast h pointer LT.Pointer
          result <- reflect $ LC.Load castedPointer valueLowType'
          uncast result valueLowType'
        M.Alloca t size -> do
          let t' = LT.fromBaseLowType t
          baseSize <- lift $ toApp Env.getBaseSize'
          let indexType = LT.PrimNum $ PT.Int $ IntSize baseSize
          castedSize <- lowerValueLetCast h size indexType
          result <- reflect $ LC.StackAlloc t' indexType castedSize
          uncast result LT.Pointer
        M.External domList cod name args varArgAndTypeList -> do
          alreadyRegistered <- lift $ member h (DN.Ext name)
          unless alreadyRegistered $ do
            liftIO $ insDeclEnv' h (DN.Ext name) domList cod
          let (varArgs, varTypes) = unzip varArgAndTypeList
          let argCaster = map LT.fromBaseLowType $ domList ++ varTypes
          castedArgs <- zipWithM (lowerValueLetCast h) (args ++ varArgs) argCaster
          let suffix = if null varArgs then [] else [LT.VarArgs]
          let lowCod = F.fromForeignCodType cod
          let funcType = LT.Function (map LT.fromBaseLowType domList ++ suffix) lowCod
          case lowCod of
            LT.Void -> do
              reflectCont $ LC.MagicCall funcType (LC.VarExternal name) $ zip argCaster castedArgs
              return LC.Null
            _ -> do
              result <- reflect $ LC.MagicCall funcType (LC.VarExternal name) $ zip argCaster castedArgs
              uncast result lowCod
        M.Global name t -> do
          let t' = LT.fromBaseLowType t
          uncast (LC.VarExternal name) t'
        M.OpaqueValue e ->
          lowerValue h e

lowerCompPrimOp :: Handle -> PrimOp -> [C.Value] -> Lower LC.Value
lowerCompPrimOp h op vs = do
  let (domList, cod) = getTypeInfo op
  argVarList <- lowerValueLetCastPrimArgs h $ zip vs domList
  result <- reflect $ LC.PrimOp op argVarList
  uncast result $ LT.PrimNum cod

lowerValueLetCastPrimArgs :: Handle -> [(C.Value, PT.PrimType)] -> Lower [LC.Value]
lowerValueLetCastPrimArgs h dts =
  case dts of
    [] ->
      return []
    ((d, t) : rest) -> do
      argVar <- lowerValueLetCast h d $ LT.PrimNum t
      argVarList <- lowerValueLetCastPrimArgs h rest
      return $ argVar : argVarList

cast :: LC.Value -> LT.LowType -> Lower LC.Value
cast v lowType = do
  (result, resultVar) <- lift $ newValueLocal "result"
  case lowType of
    LT.PrimNum (PT.Int _) -> do
      extend $ return . LC.Let result (LC.PointerToInt v LT.Pointer lowType)
    LT.PrimNum (PT.Float size) -> do
      let floatType = LT.PrimNum $ PT.Float size
      let intType = LT.PrimNum $ PT.Int $ IntSize $ floatSizeToInt size
      (tmp, tmpVar) <- lift $ newValueLocal "tmp"
      extend $
        return
          . LC.Let tmp (LC.PointerToInt v LT.Pointer intType)
          . LC.Let result (LC.Bitcast tmpVar intType floatType)
    _ -> do
      extend $ return . LC.Let result (LC.Bitcast v LT.Pointer lowType)
  return resultVar

uncast :: LC.Value -> LT.LowType -> Lower LC.Value
uncast castedValue lowType = do
  (result, resultVar) <- lift $ newValueLocal "uncast"
  case lowType of
    LT.PrimNum (PT.Int _) ->
      extend $ return . LC.Let result (LC.IntToPointer castedValue lowType LT.Pointer)
    LT.PrimNum (PT.Float i) -> do
      let floatType = LT.PrimNum $ PT.Float i
      let intType = LT.PrimNum $ PT.Int $ IntSize $ floatSizeToInt i
      (tmp, tmpVar) <- lift $ newValueLocal "tmp"
      extend $
        return
          . LC.Let tmp (LC.Bitcast castedValue floatType intType)
          . LC.Let result (LC.IntToPointer tmpVar intType LT.Pointer)
    _ ->
      extend $ return . LC.Let result (LC.Bitcast castedValue lowType LT.Pointer)
  return resultVar

lowerValueLetCast :: Handle -> C.Value -> LT.LowType -> Lower LC.Value
lowerValueLetCast h v lowType = do
  v' <- lowerValue h v
  cast v' lowType

lowerValue :: Handle -> C.Value -> Lower LC.Value
lowerValue h v =
  case v of
    C.VarGlobal globalName argNum -> do
      lowNameSet <- liftIO $ getDefinedNameSet h
      unless (S.member globalName lowNameSet) $ do
        liftIO $ insDeclEnv h (DN.In globalName) argNum
      uncast (LC.VarGlobal globalName) LT.Pointer
    C.VarLocal y ->
      return $ LC.VarLocal y
    C.VarStaticText text -> do
      let i8s = encode $ T.unpack text
      let len = length i8s
      i <- lift Gensym.newCount
      locatorHandle <- lift Locator.new
      name <- lift $ liftIO $ Locator.attachCurrentLocator locatorHandle $ BN.textName i
      let encodedText = foldMap (\w -> "\\" <> word8HexFixed w) i8s
      liftIO $ insertStaticText h name encodedText len
      uncast (LC.VarGlobal name) LT.Pointer
    C.SigmaIntro ds -> do
      let arrayType = AggTypeArray (length ds) LT.Pointer
      createAggData h arrayType $ map (,LT.Pointer) ds
    C.Int size l -> do
      uncast (LC.Int l) $ LT.PrimNum $ PT.Int size
    C.Float size f -> do
      uncast (LC.Float size f) $ LT.PrimNum $ PT.Float size

getElemPtr :: LC.Value -> LT.LowType -> [Integer] -> Lower LC.Value
getElemPtr value valueType indexList = do
  let indexList' = map (\i -> (LC.Int i, LT.PrimNum $ PT.Int intSize32)) indexList
  reflect $ LC.GetElementPtr (value, valueType) indexList'

reflect :: LC.Op -> Lower LC.Value
reflect op = do
  (result, resultVar) <- lift $ newValueLocal "result"
  extend $ return . LC.Let result op
  return resultVar

reflectCont :: LC.Op -> Lower ()
reflectCont op = do
  extend $ return . LC.Cont op

-- returns Nothing iff the branch list is empty
constructSwitch :: Handle -> C.Comp -> [(EC.EnumCase, C.Comp)] -> App (LC.Comp, [(Integer, LC.Comp)])
constructSwitch h defaultBranch switch =
  case switch of
    [] -> do
      defaultBranch' <- lowerComp h defaultBranch
      return (defaultBranch', [])
    (EC.Int i, code) : rest -> do
      code' <- lowerComp h code
      (defaultBranch', caseList) <- constructSwitch h defaultBranch rest
      return (defaultBranch', (i, code') : caseList)

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

createAggData ::
  Handle ->
  AggType -> -- the type of the base pointer
  [(C.Value, LT.LowType)] ->
  Lower LC.Value
createAggData h aggType dts = do
  basePointer <- allocateBasePointer aggType
  let baseType = toLowType aggType
  valuePointerList <- getElemPtrList basePointer baseType (length dts)
  storeElements h basePointer $ zip valuePointerList dts
  return basePointer

getSizeInfoOf :: AggType -> (LT.LowType, Int)
getSizeInfoOf aggType =
  case aggType of
    AggTypeArray len t ->
      (t, len)
    AggTypeStruct ts ->
      (LT.Struct ts, 1)

allocateBasePointer :: AggType -> Lower LC.Value
allocateBasePointer aggType = do
  let lt = toLowType aggType
  case lt of
    LT.Array 0 _ ->
      return LC.Null
    LT.Struct [] ->
      return LC.Null
    _ -> do
      let (elemType, len) = getSizeInfoOf aggType
      sizePointer <- getElemPtr LC.Null elemType [toInteger len]
      allocID <- lift Gensym.newCount
      baseSize <- lift $ toApp Env.getBaseSize'
      let lowInt = LT.PrimNum $ PT.Int $ IntSize baseSize
      size <- cast sizePointer lowInt
      reflect $ LC.Alloc size len allocID

storeElements ::
  Handle ->
  LC.Value -> -- base pointer
  [(LC.Value, (C.Value, LT.LowType))] -> -- [(the index of an element, the element to be stored)]
  Lower ()
storeElements h basePointer values =
  case values of
    [] ->
      return ()
    (elemPtr, (value, valueType)) : ids -> do
      castedValue <- lowerValueLetCast h value valueType
      store valueType castedValue elemPtr
      storeElements h basePointer ids

getElemPtrList :: LC.Value -> LT.LowType -> Int -> Lower [LC.Value]
getElemPtrList basePointer baseType numOfElems =
  forM [0 .. numOfElems - 1] $ \i -> getElemPtr basePointer baseType [0, toInteger i]

newValueLocal :: T.Text -> App (Ident, LC.Value)
newValueLocal name = do
  x <- Gensym.newIdentFromText name
  return (x, LC.VarLocal x)

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

insDeclEnv :: Handle -> DN.DeclarationName -> AN.ArgNum -> IO ()
insDeclEnv h k argNum = do
  modifyIORef' (declEnv h) $ Map.insert k (BLT.toVoidPtrSeq argNum, FCT.Void)

insDeclEnv' :: Handle -> DN.DeclarationName -> [BLT.BaseLowType] -> FCT.ForeignCodType BLT.BaseLowType -> IO ()
insDeclEnv' h k domList cod = do
  modifyIORef' (declEnv h) $ Map.insert k (domList, cod)

member :: Handle -> DN.DeclarationName -> App Bool
member h k = do
  denv <- liftIO $ readIORef (declEnv h)
  return $ Map.member k denv

insertStaticText :: Handle -> DD.DefiniteDescription -> Builder -> Int -> IO ()
insertStaticText h name text len =
  modifyIORef' (staticTextList h) $ (:) (name, (text, len))

getDefinedNameSet :: Handle -> IO (S.Set DD.DefiniteDescription)
getDefinedNameSet h = do
  readIORef (definedNameSet h)

--
--

getElemPtr' :: Ident -> LC.Value -> LT.LowType -> [Integer] -> LC.Comp -> LC.Comp
getElemPtr' var value valueType indexList cont = do
  let indexList' = map (\i -> (LC.Int i, LT.PrimNum $ PT.Int intSize32)) indexList
  LC.Let var (LC.GetElementPtr (value, valueType) indexList') cont

getElemPtrList' :: LC.Value -> [Ident] -> LT.LowType -> LC.Comp -> LC.Comp
getElemPtrList' basePointer vars baseType cont = do
  let f c (var, i) = getElemPtr' var basePointer baseType [0, toInteger i] c
  foldl f cont (zip vars [0 :: Int ..])

cast' :: Ident -> LC.Value -> LT.LowType -> LC.Comp -> App LC.Comp
cast' var v lowType cont = do
  case lowType of
    LT.PrimNum (PT.Int _) -> do
      return $ LC.Let var (LC.PointerToInt v LT.Pointer lowType) cont
    LT.PrimNum (PT.Float size) -> do
      let floatType = LT.PrimNum $ PT.Float size
      let intType = LT.PrimNum $ PT.Int $ IntSize $ floatSizeToInt size
      (tmp, tmpVar) <- newValueLocal "tmp"
      return $
        LC.Let tmp (LC.PointerToInt v LT.Pointer intType) $
          LC.Let var (LC.Bitcast tmpVar intType floatType) cont
    _ -> do
      return $ LC.Let var (LC.Bitcast v LT.Pointer lowType) cont

uncast' :: Ident -> LC.Value -> LT.LowType -> LC.Comp -> App LC.Comp
uncast' var castedValue lowType cont = do
  case lowType of
    LT.PrimNum (PT.Int _) ->
      return $ LC.Let var (LC.IntToPointer castedValue lowType LT.Pointer) cont
    LT.PrimNum (PT.Float i) -> do
      let floatType = LT.PrimNum $ PT.Float i
      let intType = LT.PrimNum $ PT.Int $ IntSize $ floatSizeToInt i
      (tmp, tmpVar) <- newValueLocal "tmp"
      return $
        LC.Let tmp (LC.Bitcast castedValue floatType intType) $
          LC.Let var (LC.IntToPointer tmpVar intType LT.Pointer) cont
    _ ->
      return $ LC.Let var (LC.Bitcast castedValue lowType LT.Pointer) cont

allocateBasePointer' :: Ident -> AggType -> LC.Comp -> App LC.Comp
allocateBasePointer' resultVar aggType cont = do
  let lt = toLowType aggType
  case lt of
    LT.Array 0 _ ->
      return $ LC.Let resultVar (LC.nop LC.Null) cont
    LT.Struct [] ->
      return $ LC.Let resultVar (LC.nop LC.Null) cont
    _ -> do
      let (elemType, len) = getSizeInfoOf aggType
      (sizeVar, sizeValue) <- newValueLocal "result"
      (castVar, castValue) <- newValueLocal "result"
      allocID <- Gensym.newCount
      baseSize <- toApp Env.getBaseSize'
      let lowInt = LT.PrimNum $ PT.Int $ IntSize baseSize
      return . getElemPtr' sizeVar LC.Null elemType [toInteger len]
        =<< cast' castVar sizeValue lowInt
        =<< return (LC.Let resultVar (LC.Alloc castValue len allocID) cont)

-- getElemPtrList' :: LC.Value -> [Ident] -> LT.LowType -> LC.Comp -> LC.Comp
createAggData' ::
  Handle ->
  Ident ->
  AggType -> -- the type of the base pointer
  [(C.Value, LT.LowType)] ->
  LC.Comp ->
  App LC.Comp
createAggData' h resultVar aggType dts cont = do
  (xs, vs) <- mapAndUnzipM (const $ newValueLocal "item") dts
  let baseType = toLowType aggType
  allocateBasePointer' resultVar aggType
    =<< return . getElemPtrList' (LC.VarLocal resultVar) xs baseType
    =<< storeElements' h (LC.VarLocal resultVar) (zip vs dts) cont

storeElements' ::
  Handle ->
  LC.Value -> -- base pointer
  [(LC.Value, (C.Value, LT.LowType))] -> -- [(the index of an element, the element to be stored)]
  LC.Comp ->
  App LC.Comp
storeElements' h basePointer values cont =
  case values of
    [] ->
      return cont
    (elemPtr, (value, valueType)) : ids -> do
      (castVar, castValue) <- newValueLocal "base"
      lowerValueLetCast' h castVar value valueType
        =<< store' valueType castValue elemPtr
        =<< storeElements' h basePointer ids cont

store' :: LT.LowType -> LC.Value -> LC.Value -> LC.Comp -> App LC.Comp
store' lowType value pointer cont =
  return $ LC.Cont (LC.Store lowType value pointer) cont

-- load :: LT.LowType -> LC.Value -> Lower LC.Value
-- load elemType pointer = do
--   tmp <- reflect $ LC.Bitcast pointer LT.Pointer LT.Pointer
--   loaded <- reflect $ LC.Load tmp elemType
--   uncast loaded elemType
load' :: Ident -> LT.LowType -> LC.Value -> LC.Comp -> App LC.Comp
load' resultVar elemType pointer cont = do
  (tmpVar, tmpValue) <- newValueLocal "tmp"
  (loadedVar, loadedValue) <- newValueLocal "loaded"
  return . LC.Let tmpVar (LC.Bitcast pointer LT.Pointer LT.Pointer)
    =<< return . LC.Let loadedVar (LC.Load tmpValue elemType)
    =<< uncast' resultVar loadedValue elemType cont

-- tmp <- reflect $ LC.Bitcast pointer LT.Pointer LT.Pointer
-- loaded <- reflect $ LC.Load tmp elemType
-- uncast loaded elemType

loadElements' ::
  LC.Value -> -- base pointer
  [(Ident, (LC.Value, LT.LowType))] ->
  LC.Comp ->
  App LC.Comp
loadElements' basePointer values cont =
  case values of
    [] -> do
      return cont
    (targetVar, (valuePointer, valueType)) : rest -> do
      (castPtrVar, castPtrValue) <- newValueLocal "castptr"
      uncast' castPtrVar valuePointer valueType
        =<< load' targetVar valueType castPtrValue
        =<< loadElements' basePointer rest cont

-- uncastedValuePointer <- uncast valuePointer valueType
-- x <- load valueType uncastedValuePointer
-- xs <- loadElements basePointer xis
-- return $ x : xs

lowerValue' :: Handle -> Ident -> C.Value -> LC.Comp -> App LC.Comp
lowerValue' h resultVar v cont =
  case v of
    C.VarGlobal globalName argNum -> do
      lowNameSet <- liftIO $ getDefinedNameSet h
      unless (S.member globalName lowNameSet) $ do
        liftIO $ insDeclEnv h (DN.In globalName) argNum
      uncast' resultVar (LC.VarGlobal globalName) LT.Pointer cont
    C.VarLocal y ->
      return $ LC.Let resultVar (LC.nop $ LC.VarLocal y) cont
    C.VarStaticText text -> do
      let i8s = encode $ T.unpack text
      let len = length i8s
      i <- Gensym.newCount
      locatorHandle <- Locator.new
      name <- lift $ liftIO $ Locator.attachCurrentLocator locatorHandle $ BN.textName i
      let encodedText = foldMap (\w -> "\\" <> word8HexFixed w) i8s
      liftIO $ insertStaticText h name encodedText len
      uncast' resultVar (LC.VarGlobal name) LT.Pointer cont
    C.SigmaIntro ds -> do
      let arrayType = AggTypeArray (length ds) LT.Pointer
      createAggData' h resultVar arrayType (map (,LT.Pointer) ds) cont
    C.Int size l -> do
      uncast' resultVar (LC.Int l) (LT.PrimNum $ PT.Int size) cont
    C.Float size f -> do
      uncast' resultVar (LC.Float size f) (LT.PrimNum $ PT.Float size) cont

lowerValues :: Handle -> [(Ident, C.Value)] -> LC.Comp -> App LC.Comp
lowerValues h xvs cont =
  case xvs of
    [] ->
      return cont
    (x, v) : rest -> do
      lowerValue' h x v
        =<< lowerValues h rest cont

lowerValueLetCast' :: Handle -> Ident -> C.Value -> LT.LowType -> LC.Comp -> App LC.Comp
lowerValueLetCast' h resultVar v lowType cont = do
  (tmpVar, tmpValue) <- newValueLocal "tmp"
  lowerValue' h tmpVar v
    =<< cast' resultVar tmpValue lowType cont

freeOrNop :: Bool -> LC.Value -> Int -> LC.Comp -> App LC.Comp
freeOrNop shouldDeallocate pointer len cont = do
  if shouldDeallocate
    then do
      freeID <- Gensym.newCount
      return $ LC.Cont (LC.Free pointer len freeID) cont
    else do
      return cont
