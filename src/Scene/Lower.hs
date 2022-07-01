{-# LANGUAGE TupleSections #-}

module Scene.Lower
  ( lowerMain,
    lowerOther,
  )
where

import Context.App
import qualified Context.Gensym as Gensym
import qualified Context.Throw as Throw
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Writer.Lazy
import Data.Function
import qualified Data.HashMap.Lazy as Map
import Data.IORef
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.Comp
import Entity.EnumCase
import Entity.Global
import Entity.Ident
import Entity.LowComp
import Entity.LowType
import Entity.Magic
import Entity.PrimNum
import Entity.PrimNumSize
import Entity.PrimNumSize.ToInt
import Entity.PrimOp

-- fixme: remove WriterT
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

lowerMain :: Axis -> ([CompDef], Comp) -> IO LowComp
lowerMain axis (defList, mainTerm) = do
  initialize $ cartImmName : cartClsName : map fst defList
  registerCartesian axis cartImmName
  registerCartesian axis cartClsName
  forM_ defList $ \(name, (_, args, e)) ->
    lowerComp axis e >>= insLowDefEnv name args
  mainTerm'' <- lowerComp axis mainTerm
  -- the result of "main" must be i64, not i8*
  (result, resultVar) <- Gensym.newValueVarLocalWith (axis & gensym) "result"
  castResult <- runLower $ lowerValueLetCast axis resultVar (LowTypePrimNum $ PrimNumInt $ IntSize 64)
  -- let result: i8* := (main-term) in {cast result to i64}
  commConv result mainTerm'' castResult

lowerOther :: Axis -> [CompDef] -> IO ()
lowerOther axis defList = do
  initialize $ map fst defList
  insDeclEnv cartImmName [(), ()]
  insDeclEnv cartClsName [(), ()]
  insDeclEnv cartCellName [(), ()]
  lowDeclEnv <- readIORef lowDeclEnvRef
  forM_ defList $ \(name, (_, args, e)) ->
    unless (Map.member name lowDeclEnv) $
      lowerComp axis e >>= insLowDefEnv name args

initialize :: [T.Text] -> IO ()
initialize nameList = do
  writeIORef lowDeclEnvRef initialLowDeclEnv
  writeIORef lowDefEnvRef Map.empty
  writeIORef lowNameSetRef $ S.fromList nameList

lowerComp :: Axis -> Comp -> IO LowComp
lowerComp axis term =
  case term of
    CompPrimitive theta ->
      runLower $ lowerCompPrimitive axis theta
    CompPiElimDownElim v ds -> do
      runLowerComp $ do
        v' <- lowerValue axis v
        ds' <- mapM (lowerValue axis) ds
        v'' <- cast axis v' $ toFunPtrType ds
        return $ LowCompCall v'' ds'
    CompSigmaElim isNoetic xs v e -> do
      let baseType = LowTypePointer $ LowTypeArray (length xs) voidPtr
      runLowerComp $ do
        basePointer <- lowerValue axis v
        castedBasePointer <- cast axis basePointer baseType
        ds <- loadElements axis castedBasePointer baseType $ take (length xs) $ zip [0 ..] (repeat voidPtr)
        unless isNoetic $ free axis castedBasePointer baseType
        forM_ (zip xs ds) $ \(x, d) -> do
          extend $ return . LowCompLet x (LowOpBitcast d voidPtr voidPtr)
        liftIO $ lowerComp axis e
    CompUpIntro d ->
      runLower $ lowerValue axis d
    CompUpElim x e1 e2 -> do
      e1' <- lowerComp axis e1
      e2' <- lowerComp axis e2
      commConv x e1' e2'
    CompEnumElim v branchList -> do
      m <- constructSwitch axis branchList
      case m of
        Nothing ->
          return LowCompUnreachable
        Just (defaultCase, caseList) -> do
          runLowerComp $ do
            let t = LowTypePrimNum $ PrimNumInt $ IntSize 64
            castedValue <- lowerValueLetCast axis v t
            return $ LowCompSwitch (castedValue, t) defaultCase caseList
    CompArrayAccess elemType v index -> do
      let elemType' = LowTypePrimNum elemType
      let elemSize = LowValueInt (primNumToSizeInByte elemType)
      runLower $ do
        indexVar <- lowerValueLetCast axis index i64
        arrayVar <- lowerValue axis v
        castedArrayVar <- cast axis arrayVar i64
        startIndex <- load axis (LowTypePrimNum $ PrimNumInt $ IntSize 64) arrayVar
        castedStartIndex <- cast axis startIndex i64
        realIndex <- arith axis "add" [indexVar, castedStartIndex]
        arrayOffset <- arith axis "mul" [elemSize, realIndex]
        realOffset <- arith axis "add" [LowValueInt 16, arrayOffset]
        elemAddress <- arith axis "add" [castedArrayVar, realOffset]
        uncastedElemAddress <- uncast axis elemAddress i64
        load axis elemType' uncastedElemAddress

i64 :: LowType
i64 = LowTypePrimNum $ PrimNumInt $ IntSize 64

i64p :: PrimNum
i64p = PrimNumInt $ IntSize 64

load :: Axis -> LowType -> LowValue -> Lower LowValue
load axis elemType pointer = do
  tmp <- reflect axis $ LowOpBitcast pointer voidPtr (LowTypePointer elemType)
  loaded <- reflect axis $ LowOpLoad tmp elemType
  uncast axis loaded elemType

store :: LowType -> LowValue -> LowValue -> Lower ()
store lowType value pointer =
  reflectCont $ LowOpStore lowType value pointer

arith :: Axis -> T.Text -> [LowValue] -> Lower LowValue
arith axis op args = do
  reflect axis $ LowOpPrimOp (PrimOp op [i64p, i64p] i64p) args

primNumToSizeInByte :: PrimNum -> Integer
primNumToSizeInByte primNum =
  case primNum of
    PrimNumInt size ->
      toInteger $ intSizeToInt size `div` 8
    PrimNumFloat size ->
      toInteger $ floatSizeToInt size `div` 8

loadElements ::
  Axis ->
  LowValue -> -- base pointer
  LowType -> -- the type of base pointer
  [(Int, LowType)] -> -- [(the index of an element, the variable to keep the loaded content)]
  Lower [LowValue]
loadElements axis basePointer baseType values =
  case values of
    [] -> do
      return []
    (valueIndex, valueType) : xis -> do
      valuePointer <- getElemPtr axis basePointer baseType [0, toInteger valueIndex]
      uncastedValuePointer <- uncast axis valuePointer (LowTypePointer valueType) -- fixme: uncast this
      x <- load axis valueType uncastedValuePointer
      xs <- loadElements axis basePointer baseType xis
      return $ x : xs

free :: Axis -> LowValue -> LowType -> Lower ()
free axis pointer pointerType = do
  uncastedPointer <- uncast axis pointer pointerType
  j <- liftIO $ Gensym.newCount (axis & gensym)
  reflectCont $ LowOpFree uncastedPointer pointerType j

lowerCompPrimitive :: Axis -> Primitive -> Lower LowValue
lowerCompPrimitive axis codeOp =
  case codeOp of
    PrimitivePrimOp op vs ->
      lowerCompPrimOp axis op vs
    PrimitiveMagic der -> do
      case der of
        MagicCast _ _ value -> do
          lowerValue axis value
        MagicStore valueLowType pointer value -> do
          ptrVar <- lowerValueLetCast axis pointer (LowTypePointer valueLowType)
          valVar <- lowerValueLetCast axis value valueLowType
          extend $ return . LowCompCont (LowOpStore valueLowType valVar ptrVar)
          return LowValueNull
        MagicLoad valueLowType pointer -> do
          castedPointer <- lowerValueLetCast axis pointer (LowTypePointer valueLowType)
          result <- reflect axis $ LowOpLoad castedPointer valueLowType
          uncast axis result valueLowType
        MagicSyscall i args -> do
          args' <- mapM (lowerValue axis) args
          reflect axis $ LowOpSyscall i args'
        MagicExternal name args -> do
          args' <- mapM (lowerValue axis) args
          liftIO $ insDeclEnv name args'
          reflect axis $ LowOpCall (LowValueVarGlobal name) args'

lowerCompPrimOp :: Axis -> PrimOp -> [Value] -> Lower LowValue
lowerCompPrimOp axis op@(PrimOp _ domList cod) vs = do
  argVarList <- lowerValueLetCastPrimArgs axis $ zip vs domList
  result <- reflect axis $ LowOpPrimOp op argVarList
  uncast axis result $ LowTypePrimNum cod

lowerValueLetCastPrimArgs :: Axis -> [(Value, PrimNum)] -> Lower [LowValue]
lowerValueLetCastPrimArgs axis dts =
  case dts of
    [] ->
      return []
    ((d, t) : rest) -> do
      argVar <- lowerValueLetCast axis d $ LowTypePrimNum t
      argVarList <- lowerValueLetCastPrimArgs axis rest
      return $ argVar : argVarList

cast :: Axis -> LowValue -> LowType -> Lower LowValue
cast axis v lowType = do
  (result, resultVar) <- liftIO $ newValueLocal axis "result"
  case lowType of
    LowTypePrimNum (PrimNumInt _) -> do
      extend $ return . LowCompLet result (LowOpPointerToInt v voidPtr lowType)
    LowTypePrimNum (PrimNumFloat size) -> do
      let floatType = LowTypePrimNum $ PrimNumFloat size
      let intType = LowTypePrimNum $ PrimNumInt $ IntSize $ floatSizeToInt size
      (tmp, tmpVar) <- liftIO $ newValueLocal axis "tmp"
      extend $
        return
          . LowCompLet tmp (LowOpPointerToInt v voidPtr intType)
          . LowCompLet result (LowOpBitcast tmpVar intType floatType)
    _ -> do
      extend $ return . LowCompLet result (LowOpBitcast v voidPtr lowType)
  return resultVar

uncast :: Axis -> LowValue -> LowType -> Lower LowValue
uncast axis castedValue lowType = do
  (result, resultVar) <- liftIO $ newValueLocal axis "uncast"
  case lowType of
    LowTypePrimNum (PrimNumInt _) ->
      extend $ return . LowCompLet result (LowOpIntToPointer castedValue lowType voidPtr)
    LowTypePrimNum (PrimNumFloat i) -> do
      let floatType = LowTypePrimNum $ PrimNumFloat i
      let intType = LowTypePrimNum $ PrimNumInt $ IntSize $ floatSizeToInt i
      (tmp, tmpVar) <- liftIO $ newValueLocal axis "tmp"
      extend $
        return
          . LowCompLet tmp (LowOpBitcast castedValue floatType intType)
          . LowCompLet result (LowOpIntToPointer tmpVar intType voidPtr)
    _ ->
      extend $ return . LowCompLet result (LowOpBitcast castedValue lowType voidPtr)
  return resultVar

lowerValueLetCast :: Axis -> Value -> LowType -> Lower LowValue
lowerValueLetCast axis v lowType = do
  v' <- lowerValue axis v
  cast axis v' lowType

lowerValue :: Axis -> Value -> Lower LowValue
lowerValue axis v =
  case v of
    ValueVarGlobal y -> do
      compDefEnv <- liftIO $ readIORef compDefEnvRef
      case Map.lookup y compDefEnv of
        Nothing ->
          liftIO $ axis & throw & Throw.raiseCritical' $ "no such global variable is defined: " <> y
        Just (_, args, _) -> do
          liftIO $ insDeclEnvIfNecessary y args
          uncast axis (LowValueVarGlobal y) (toFunPtrType args)
    ValueVarLocal y ->
      return $ LowValueVarLocal y
    ValueVarLocalIdeal y ->
      return $ LowValueVarLocal y
    ValueSigmaIntro ds -> do
      let arrayType = AggPtrTypeArray (length ds) voidPtr
      createAggData axis arrayType $ zip ds (repeat voidPtr)
    ValueInt size l -> do
      uncast axis (LowValueInt l) $ LowTypePrimNum $ PrimNumInt size
    ValueFloat size f ->
      uncast axis (LowValueFloat size f) $ LowTypePrimNum $ PrimNumFloat size
    ValueEnumIntro l -> do
      i <- liftIO $ toInteger <$> enumValueToInteger axis l
      uncast axis (LowValueInt i) $ LowTypePrimNum $ PrimNumInt $ IntSize 64
    ValueArrayIntro elemType vs -> do
      let lenValue = LowValueInt (toInteger $ length vs)
      let elemType' = LowTypePrimNum elemType
      let pointerType = LowTypePointer $ LowTypeStruct [i64, i64, LowTypeArray (length vs) elemType']
      let elemInfoList = zip [0 ..] $ map (,elemType') vs
      let arrayType = LowTypePointer $ LowTypeArray (length vs) elemType'
      arrayLength <- arith axis "mul" [LowValueInt (primNumToSizeInByte elemType), lenValue]
      realLength <- arith axis "add" [LowValueInt 16, arrayLength]
      uncastedRealLength <- uncast axis realLength i64
      pointer <- malloc axis uncastedRealLength
      castedPointer <- cast axis pointer pointerType
      startPointer <- getElemPtr axis castedPointer pointerType [0, 0]
      endPointer <- getElemPtr axis castedPointer pointerType [0, 1]
      arrayPointer <- getElemPtr axis castedPointer pointerType [0, 2]
      store i64 (LowValueInt 0) startPointer
      store i64 lenValue endPointer
      storeElements axis arrayPointer arrayType elemInfoList
      return pointer

malloc :: Axis -> LowValue -> Lower LowValue
malloc axis size = do
  reflect axis $ LowOpCall (LowValueVarGlobal "malloc") [size]

getElemPtr :: Axis -> LowValue -> LowType -> [Integer] -> Lower LowValue
getElemPtr axis value valueType indexList = do
  let indexList' = map (\i -> (LowValueInt i, LowTypePrimNum $ PrimNumInt $ IntSize 32)) indexList
  reflect axis $ LowOpGetElementPtr (value, valueType) indexList'

reflect :: Axis -> LowOp -> Lower LowValue
reflect axis op = do
  (result, resultVar) <- liftIO $ newValueLocal axis "result"
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
constructSwitch :: Axis -> [(CompEnumCase, Comp)] -> IO (Maybe (LowComp, [(Int, LowComp)]))
constructSwitch axis switch =
  case switch of
    [] ->
      return Nothing
    (_ :< EnumCaseDefault, code) : _ -> do
      code' <- lowerComp axis code
      return $ Just (code', [])
    [(m :< _, code)] -> do
      constructSwitch axis [(m :< EnumCaseDefault, code)]
    (m :< EnumCaseLabel l, code) : rest -> do
      i <- enumValueToInteger axis l
      constructSwitch axis $ (m :< EnumCaseInt i, code) : rest
    (_ :< EnumCaseInt i, code) : rest -> do
      code' <- lowerComp axis code
      mSwitch <- constructSwitch axis rest
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
  Axis ->
  AggPtrType -> -- the type of the base pointer
  [(Value, LowType)] ->
  Lower LowValue
createAggData axis aggPtrType dts = do
  basePointer <- allocateBasePointer axis aggPtrType
  castedBasePointer <- cast axis basePointer $ toLowType aggPtrType
  storeElements axis castedBasePointer (toLowType aggPtrType) $ zip [0 ..] dts
  return basePointer

getSizeInfoOf :: AggPtrType -> (LowType, Int)
getSizeInfoOf aggPtrType =
  case aggPtrType of
    AggPtrTypeArray len t ->
      (t, len)
    AggPtrTypeStruct ts ->
      (LowTypeStruct ts, 1)

allocateBasePointer :: Axis -> AggPtrType -> Lower LowValue
allocateBasePointer axis aggPtrType = do
  let (elemType, len) = getSizeInfoOf aggPtrType
  sizePointer <- getElemPtr axis LowValueNull (LowTypePointer elemType) [toInteger len]
  c <- uncast axis sizePointer (LowTypePointer elemType)
  reflect axis $ LowOpAlloc c $ toLowType aggPtrType

storeElements ::
  Axis ->
  LowValue -> -- base pointer
  LowType -> -- the type of base pointer (like [n x u8]*, {i8*, i8*}*, etc.)
  [(Integer, (Value, LowType))] -> -- [(the index of an element, the element to be stored)]
  Lower ()
storeElements axis basePointer baseType values =
  case values of
    [] ->
      return ()
    (valueIndex, (value, valueType)) : ids -> do
      castedValue <- lowerValueLetCast axis value valueType
      elemPtr <- getElemPtr axis basePointer baseType [0, valueIndex]
      store valueType castedValue elemPtr
      storeElements axis basePointer baseType ids

toFunPtrType :: [a] -> LowType
toFunPtrType xs =
  LowTypePointer (LowTypeFunction (map (const voidPtr) xs) voidPtr)

newValueLocal :: Axis -> T.Text -> IO (Ident, LowValue)
newValueLocal axis name = do
  x <- Gensym.newIdentFromText (axis & gensym) name
  return (x, LowValueVarLocal x)

enumValueToInteger :: Axis -> T.Text -> IO Int
enumValueToInteger axis label = do
  revEnumEnv <- readIORef revEnumEnvRef
  case Map.lookup label revEnumEnv of
    Just (_, i) ->
      return i
    _ -> do
      print revEnumEnv
      axis & throw & Throw.raiseCritical' $ "no such enum is defined: " <> label

insLowDefEnv :: T.Text -> [Ident] -> LowComp -> IO ()
insLowDefEnv funName args e =
  modifyIORef' lowDefEnvRef $ Map.insert funName (args, e)

commConv :: Ident -> LowComp -> LowComp -> IO LowComp
commConv x lowComp cont2 =
  case lowComp of
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

registerCartesian :: Axis -> T.Text -> IO ()
registerCartesian axis name = do
  compDefEnv <- readIORef compDefEnvRef
  case Map.lookup name compDefEnv of
    Just (_, args, e) ->
      lowerComp axis e >>= insLowDefEnv name args
    _ ->
      return ()
