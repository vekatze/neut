{-# LANGUAGE TupleSections #-}

module Scene.Lower
  ( lowerMain,
    lowerOther,
  )
where

import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Writer.Lazy
import qualified Data.HashMap.Lazy as Map
import Data.IORef
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.Basic
import Entity.Comp
import Entity.Global
import Entity.Log
import Entity.LowComp
import Entity.LowType
import Entity.Magic
import Entity.PrimNum
import Entity.PrimNumSize
import Entity.PrimNumSize.ToInt
import Entity.PrimOp

type Lower = WriterT Cont IO

newtype Cont = Cont (LowComp -> IO LowComp)

instance Semigroup Cont where
  Cont newCont <> Cont oldCont =
    Cont $ newCont <=< oldCont

instance Monoid Cont where
  mempty =
    Cont return

extend :: (LowComp -> IO LowComp) -> Lower ()
extend =
  tell . Cont

runLower :: Lower LowValue -> IO LowComp
runLower m = do
  (a, Cont b) <- runWriterT m
  b $ LowCompReturn a

runLowerComp :: Lower LowComp -> IO LowComp
runLowerComp m = do
  (a, Cont b) <- runWriterT m
  b a

lowerMain :: ([CompDef], Comp) -> IO LowComp
lowerMain (defList, mainTerm) = do
  initialize $ cartImmName : cartClsName : map fst defList
  registerCartesian cartImmName
  registerCartesian cartClsName
  forM_ defList $ \(name, (_, args, e)) ->
    lowerComp e >>= insLowDefEnv name args
  mainTerm'' <- lowerComp mainTerm
  -- the result of "main" must be i64, not i8*
  (result, resultVar) <- newValueVarLocalWith "result"
  castResult <- runLower $ lowerValueLetCast resultVar (LowTypePrimNum $ PrimNumInt $ IntSize 64)
  -- let result: i8* := (main-term) in {cast result to i64}
  commConv result mainTerm'' castResult

lowerOther :: [CompDef] -> IO ()
lowerOther defList = do
  initialize $ map fst defList
  insDeclEnv cartImmName [(), ()]
  insDeclEnv cartClsName [(), ()]
  insDeclEnv cartCellName [(), ()]
  lowDeclEnv <- readIORef lowDeclEnvRef
  forM_ defList $ \(name, (_, args, e)) ->
    unless (Map.member name lowDeclEnv) $
      lowerComp e >>= insLowDefEnv name args

initialize :: [T.Text] -> IO ()
initialize nameList = do
  writeIORef lowDeclEnvRef initialLowDeclEnv
  writeIORef lowDefEnvRef Map.empty
  writeIORef lowNameSetRef $ S.fromList nameList

lowerComp :: Comp -> IO LowComp
lowerComp term =
  case term of
    CompPrimitive theta ->
      runLower $ lowerCompPrimitive theta
    CompPiElimDownElim v ds -> do
      runLowerComp $ do
        v' <- lowerValue v
        ds' <- mapM lowerValue ds
        v'' <- cast v' $ toFunPtrType ds
        return $ LowCompCall v'' ds'
    CompSigmaElim isNoetic xs v e -> do
      let baseType = LowTypePointer $ LowTypeArray (length xs) voidPtr
      runLowerComp $ do
        basePointer <- lowerValue v
        castedBasePointer <- cast basePointer baseType
        ds <- loadElements castedBasePointer baseType $ take (length xs) $ zip [0 ..] (repeat voidPtr)
        unless isNoetic $ free castedBasePointer baseType
        forM_ (zip xs ds) $ \(x, d) -> do
          extend $ return . LowCompLet x (LowOpBitcast d voidPtr voidPtr)
        liftIO $ lowerComp e
    CompUpIntro d ->
      runLower $ lowerValue d
    CompUpElim x e1 e2 -> do
      e1' <- lowerComp e1
      e2' <- lowerComp e2
      commConv x e1' e2'
    CompEnumElim v branchList -> do
      m <- constructSwitch branchList
      case m of
        Nothing ->
          return LowCompUnreachable
        Just (defaultCase, caseList) -> do
          runLowerComp $ do
            let t = LowTypePrimNum $ PrimNumInt $ IntSize 64
            castedValue <- lowerValueLetCast v t
            return $ LowCompSwitch (castedValue, t) defaultCase caseList
    CompArrayAccess elemType v index -> do
      let elemType' = LowTypePrimNum elemType
      let elemSize = LowValueInt (primNumToSizeInByte elemType)
      runLower $ do
        indexVar <- lowerValueLetCast index i64
        arrayVar <- lowerValue v
        castedArrayVar <- cast arrayVar i64
        startIndex <- load (LowTypePrimNum $ PrimNumInt $ IntSize 64) arrayVar
        castedStartIndex <- cast startIndex i64
        realIndex <- arith "add" [indexVar, castedStartIndex]
        arrayOffset <- arith "mul" [elemSize, realIndex]
        realOffset <- arith "add" [LowValueInt 16, arrayOffset]
        elemAddress <- arith "add" [castedArrayVar, realOffset]
        uncastedElemAddress <- uncast elemAddress i64
        load elemType' uncastedElemAddress

i64 :: LowType
i64 = LowTypePrimNum $ PrimNumInt $ IntSize 64

i64p :: PrimNum
i64p = PrimNumInt $ IntSize 64

load :: LowType -> LowValue -> Lower LowValue
load elemType pointer = do
  tmp <- reflect $ LowOpBitcast pointer voidPtr (LowTypePointer elemType)
  loaded <- reflect $ LowOpLoad tmp elemType
  uncast loaded elemType

store :: LowType -> LowValue -> LowValue -> Lower ()
store lowType value pointer =
  reflectCont $ LowOpStore lowType value pointer

arith :: T.Text -> [LowValue] -> Lower LowValue
arith op args = do
  reflect $ LowOpPrimOp (PrimOp op [i64p, i64p] i64p) args

primNumToSizeInByte :: PrimNum -> Integer
primNumToSizeInByte primNum =
  case primNum of
    PrimNumInt size ->
      toInteger $ intSizeToInt size `div` 8
    PrimNumFloat size ->
      toInteger $ floatSizeToInt size `div` 8

loadElements ::
  LowValue -> -- base pointer
  LowType -> -- the type of base pointer
  [(Int, LowType)] -> -- [(the index of an element, the variable to keep the loaded content)]
  Lower [LowValue]
loadElements basePointer baseType values =
  case values of
    [] -> do
      return []
    (valueIndex, valueType) : xis -> do
      valuePointer <- getElemPtr basePointer baseType [0, toInteger valueIndex]
      uncastedValuePointer <- uncast valuePointer (LowTypePointer valueType) -- fixme: uncast this
      x <- load valueType uncastedValuePointer
      xs <- loadElements basePointer baseType xis
      return $ x : xs

free :: LowValue -> LowType -> Lower ()
free pointer pointerType = do
  uncastedPointer <- uncast pointer pointerType
  j <- liftIO newCount
  reflectCont $ LowOpFree uncastedPointer pointerType j

lowerCompPrimitive :: Primitive -> Lower LowValue
lowerCompPrimitive codeOp =
  case codeOp of
    PrimitivePrimOp op vs ->
      lowerCompPrimOp op vs
    PrimitiveMagic der -> do
      case der of
        MagicCast _ _ value -> do
          lowerValue value
        MagicStore valueLowType pointer value -> do
          ptrVar <- lowerValueLetCast pointer (LowTypePointer valueLowType)
          valVar <- lowerValueLetCast value valueLowType
          extend $ return . LowCompCont (LowOpStore valueLowType valVar ptrVar)
          return LowValueNull
        MagicLoad valueLowType pointer -> do
          castedPointer <- lowerValueLetCast pointer (LowTypePointer valueLowType)
          result <- reflect $ LowOpLoad castedPointer valueLowType
          uncast result valueLowType
        MagicSyscall i args -> do
          args' <- mapM lowerValue args
          reflect $ LowOpSyscall i args'
        MagicExternal name args -> do
          args' <- mapM lowerValue args
          liftIO $ insDeclEnv name args'
          reflect $ LowOpCall (LowValueVarGlobal name) args'

lowerCompPrimOp :: PrimOp -> [Value] -> Lower LowValue
lowerCompPrimOp op@(PrimOp _ domList cod) vs = do
  argVarList <- lowerValueLetCastPrimArgs $ zip vs domList
  result <- reflect $ LowOpPrimOp op argVarList
  uncast result $ LowTypePrimNum cod

lowerValueLetCastPrimArgs :: [(Value, PrimNum)] -> Lower [LowValue]
lowerValueLetCastPrimArgs dts =
  case dts of
    [] ->
      return []
    ((d, t) : rest) -> do
      argVar <- lowerValueLetCast d $ LowTypePrimNum t
      argVarList <- lowerValueLetCastPrimArgs rest
      return $ argVar : argVarList

cast :: LowValue -> LowType -> Lower LowValue
cast v lowType = do
  (result, resultVar) <- liftIO $ newValueLocal "result"
  case lowType of
    LowTypePrimNum (PrimNumInt _) -> do
      extend $ return . LowCompLet result (LowOpPointerToInt v voidPtr lowType)
    LowTypePrimNum (PrimNumFloat size) -> do
      let floatType = LowTypePrimNum $ PrimNumFloat size
      let intType = LowTypePrimNum $ PrimNumInt $ IntSize $ floatSizeToInt size
      (tmp, tmpVar) <- liftIO $ newValueLocal "tmp"
      extend $
        return
          . LowCompLet tmp (LowOpPointerToInt v voidPtr intType)
          . LowCompLet result (LowOpBitcast tmpVar intType floatType)
    _ -> do
      extend $ return . LowCompLet result (LowOpBitcast v voidPtr lowType)
  return resultVar

uncast :: LowValue -> LowType -> Lower LowValue
uncast castedValue lowType = do
  (result, resultVar) <- liftIO $ newValueLocal "uncast"
  case lowType of
    LowTypePrimNum (PrimNumInt _) ->
      extend $ return . LowCompLet result (LowOpIntToPointer castedValue lowType voidPtr)
    LowTypePrimNum (PrimNumFloat i) -> do
      let floatType = LowTypePrimNum $ PrimNumFloat i
      let intType = LowTypePrimNum $ PrimNumInt $ IntSize $ floatSizeToInt i
      (tmp, tmpVar) <- liftIO $ newValueLocal "tmp"
      extend $
        return
          . LowCompLet tmp (LowOpBitcast castedValue floatType intType)
          . LowCompLet result (LowOpIntToPointer tmpVar intType voidPtr)
    _ ->
      extend $ return . LowCompLet result (LowOpBitcast castedValue lowType voidPtr)
  return resultVar

lowerValueLetCast :: Value -> LowType -> Lower LowValue
lowerValueLetCast v lowType = do
  v' <- lowerValue v
  cast v' lowType

lowerValue :: Value -> Lower LowValue
lowerValue v =
  case v of
    ValueVarGlobal y -> do
      compDefEnv <- liftIO $ readIORef compDefEnvRef
      case Map.lookup y compDefEnv of
        Nothing ->
          raiseCritical' $ "no such global variable is defined: " <> y
        Just (_, args, _) -> do
          liftIO $ insDeclEnvIfNecessary y args
          uncast (LowValueVarGlobal y) (toFunPtrType args)
    ValueVarLocal y ->
      return $ LowValueVarLocal y
    ValueVarLocalIdeal y ->
      return $ LowValueVarLocal y
    ValueSigmaIntro ds -> do
      let arrayType = AggPtrTypeArray (length ds) voidPtr
      createAggData arrayType $ zip ds (repeat voidPtr)
    ValueInt size l -> do
      uncast (LowValueInt l) $ LowTypePrimNum $ PrimNumInt size
    ValueFloat size f ->
      uncast (LowValueFloat size f) $ LowTypePrimNum $ PrimNumFloat size
    ValueEnumIntro l -> do
      i <- liftIO $ toInteger <$> enumValueToInteger l
      uncast (LowValueInt i) $ LowTypePrimNum $ PrimNumInt $ IntSize 64
    ValueArrayIntro elemType vs -> do
      let lenValue = LowValueInt (toInteger $ length vs)
      let elemType' = LowTypePrimNum elemType
      let pointerType = LowTypePointer $ LowTypeStruct [i64, i64, LowTypeArray (length vs) elemType']
      let elemInfoList = zip [0 ..] $ map (,elemType') vs
      let arrayType = LowTypePointer $ LowTypeArray (length vs) elemType'
      arrayLength <- arith "mul" [LowValueInt (primNumToSizeInByte elemType), lenValue]
      realLength <- arith "add" [LowValueInt 16, arrayLength]
      uncastedRealLength <- uncast realLength i64
      pointer <- malloc uncastedRealLength
      castedPointer <- cast pointer pointerType
      startPointer <- getElemPtr castedPointer pointerType [0, 0]
      endPointer <- getElemPtr castedPointer pointerType [0, 1]
      arrayPointer <- getElemPtr castedPointer pointerType [0, 2]
      store i64 (LowValueInt 0) startPointer
      store i64 lenValue endPointer
      storeElements arrayPointer arrayType elemInfoList
      return pointer

malloc :: LowValue -> Lower LowValue
malloc size = do
  reflect $ LowOpCall (LowValueVarGlobal "malloc") [size]

getElemPtr :: LowValue -> LowType -> [Integer] -> Lower LowValue
getElemPtr value valueType indexList = do
  let indexList' = map (\i -> (LowValueInt i, LowTypePrimNum $ PrimNumInt $ IntSize 32)) indexList
  reflect $ LowOpGetElementPtr (value, valueType) indexList'

reflect :: LowOp -> Lower LowValue
reflect op = do
  (result, resultVar) <- liftIO $ newValueLocal "result"
  extend $ return . LowCompLet result op
  return resultVar

reflectCont :: LowOp -> Lower ()
reflectCont op = do
  extend $ return . LowCompCont op

insDeclEnvIfNecessary :: T.Text -> [a] -> IO ()
insDeclEnvIfNecessary symbol args = do
  lowNameSet <- readIORef lowNameSetRef
  if S.member symbol lowNameSet
    then return ()
    else insDeclEnv symbol args

-- returns Nothing iff the branch list is empty
constructSwitch :: [(CompEnumCase, Comp)] -> IO (Maybe (LowComp, [(Int, LowComp)]))
constructSwitch switch =
  case switch of
    [] ->
      return Nothing
    (_ :< EnumCaseDefault, code) : _ -> do
      code' <- lowerComp code
      return $ Just (code', [])
    [(m :< _, code)] -> do
      constructSwitch [(m :< EnumCaseDefault, code)]
    (m :< EnumCaseLabel l, code) : rest -> do
      i <- enumValueToInteger l
      constructSwitch $ (m :< EnumCaseInt i, code) : rest
    (_ :< EnumCaseInt i, code) : rest -> do
      code' <- lowerComp code
      mSwitch <- constructSwitch rest
      return $ do
        (defaultCase, caseList) <- mSwitch
        return (defaultCase, (i, code') : caseList)

data AggPtrType
  = AggPtrTypeArray Int LowType
  | AggPtrTypeStruct [LowType]

toLowType :: AggPtrType -> LowType
toLowType aggPtrType =
  case aggPtrType of
    AggPtrTypeArray i t ->
      LowTypePointer $ LowTypeArray i t
    AggPtrTypeStruct ts ->
      LowTypePointer $ LowTypeStruct ts

createAggData ::
  AggPtrType -> -- the type of the base pointer
  [(Value, LowType)] ->
  Lower LowValue
createAggData aggPtrType dts = do
  basePointer <- allocateBasePointer aggPtrType
  castedBasePointer <- cast basePointer $ toLowType aggPtrType
  storeElements castedBasePointer (toLowType aggPtrType) $ zip [0 ..] dts
  return basePointer

getSizeInfoOf :: AggPtrType -> (LowType, Int)
getSizeInfoOf aggPtrType =
  case aggPtrType of
    AggPtrTypeArray len t ->
      (t, len)
    AggPtrTypeStruct ts ->
      (LowTypeStruct ts, 1)

allocateBasePointer :: AggPtrType -> Lower LowValue
allocateBasePointer aggPtrType = do
  let (elemType, len) = getSizeInfoOf aggPtrType
  sizePointer <- getElemPtr LowValueNull (LowTypePointer elemType) [toInteger len]
  c <- uncast sizePointer (LowTypePointer elemType)
  reflect $ LowOpAlloc c $ toLowType aggPtrType

storeElements ::
  LowValue -> -- base pointer
  LowType -> -- the type of base pointer (like [n x u8]*, {i8*, i8*}*, etc.)
  [(Integer, (Value, LowType))] -> -- [(the index of an element, the element to be stored)]
  Lower ()
storeElements basePointer baseType values =
  case values of
    [] ->
      return ()
    (valueIndex, (value, valueType)) : ids -> do
      castedValue <- lowerValueLetCast value valueType
      elemPtr <- getElemPtr basePointer baseType [0, valueIndex]
      store valueType castedValue elemPtr
      storeElements basePointer baseType ids

toFunPtrType :: [a] -> LowType
toFunPtrType xs =
  LowTypePointer (LowTypeFunction (map (const voidPtr) xs) voidPtr)

newValueLocal :: T.Text -> IO (Ident, LowValue)
newValueLocal name = do
  x <- newIdentFromText name
  return (x, LowValueVarLocal x)

enumValueToInteger :: T.Text -> IO Int
enumValueToInteger label = do
  revEnumEnv <- readIORef revEnumEnvRef
  case Map.lookup label revEnumEnv of
    Just (_, i) ->
      return i
    _ -> do
      print revEnumEnv
      raiseCritical' $ "no such enum is defined: " <> label

insLowDefEnv :: T.Text -> [Ident] -> LowComp -> IO ()
insLowDefEnv funName args e =
  modifyIORef' lowDefEnvRef $ Map.insert funName (args, e)

commConv :: Ident -> LowComp -> LowComp -> IO LowComp
commConv x llvm cont2 =
  case llvm of
    LowCompReturn d ->
      return $ LowCompLet x (LowOpBitcast d voidPtr voidPtr) cont2 -- nop
    LowCompLet y op cont1 -> do
      cont <- commConv x cont1 cont2
      return $ LowCompLet y op cont
    LowCompCont op cont1 -> do
      cont <- commConv x cont1 cont2
      return $ LowCompCont op cont
    LowCompSwitch (d, t) defaultCase caseList -> do
      let (ds, es) = unzip caseList
      es' <- mapM (\e -> commConv x e cont2) es
      let caseList' = zip ds es'
      defaultCase' <- commConv x defaultCase cont2
      return $ LowCompSwitch (d, t) defaultCase' caseList'
    LowCompCall d ds ->
      return $ LowCompLet x (LowOpCall d ds) cont2
    LowCompUnreachable ->
      return LowCompUnreachable

insDeclEnv :: T.Text -> [a] -> IO ()
insDeclEnv name args = do
  lowDeclEnv <- readIORef lowDeclEnvRef
  unless (name `Map.member` lowDeclEnv) $ do
    let dom = map (const voidPtr) args
    let cod = voidPtr
    modifyIORef' lowDeclEnvRef $ Map.insert name (dom, cod)

registerCartesian :: T.Text -> IO ()
registerCartesian name = do
  compDefEnv <- readIORef compDefEnvRef
  case Map.lookup name compDefEnv of
    Just (_, args, e) ->
      lowerComp e >>= insLowDefEnv name args
    _ ->
      return ()
