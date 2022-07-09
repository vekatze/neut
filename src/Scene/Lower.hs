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
import qualified Data.HashMap.Strict as Map
import Data.IORef
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.Comp
import Entity.Const
import qualified Entity.Discriminant as D
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

lowerMain :: Context -> ([CompDef], Comp) -> IO LowComp
lowerMain ctx (defList, mainTerm) = do
  initialize $ cartImmName : cartClsName : map fst defList
  registerCartesian ctx cartImmName
  registerCartesian ctx cartClsName
  forM_ defList $ \(name, (_, args, e)) ->
    lowerComp ctx e >>= insLowDefEnv name args
  mainTerm'' <- lowerComp ctx mainTerm
  -- the result of "main" must be i64, not i8*
  (result, resultVar) <- Gensym.newValueVarLocalWith (gensym ctx) "result"
  castResult <- runLower $ lowerValueLetCast ctx resultVar (LowTypePrimNum $ PrimNumInt $ IntSize 64)
  -- let result: i8* := (main-term) in {cast result to i64}
  commConv result mainTerm'' castResult

lowerOther :: Context -> [CompDef] -> IO ()
lowerOther ctx defList = do
  initialize $ map fst defList
  insDeclEnv cartImmName [(), ()]
  insDeclEnv cartClsName [(), ()]
  insDeclEnv cartCellName [(), ()]
  lowDeclEnv <- readIORef lowDeclEnvRef
  forM_ defList $ \(name, (_, args, e)) ->
    unless (Map.member name lowDeclEnv) $
      lowerComp ctx e >>= insLowDefEnv name args

initialize :: [T.Text] -> IO ()
initialize nameList = do
  writeIORef lowDeclEnvRef initialLowDeclEnv
  writeIORef lowDefEnvRef Map.empty
  writeIORef lowNameSetRef $ S.fromList nameList

lowerComp :: Context -> Comp -> IO LowComp
lowerComp ctx term =
  case term of
    CompPrimitive theta ->
      runLower $ lowerCompPrimitive ctx theta
    CompPiElimDownElim v ds -> do
      runLowerComp $ do
        v' <- lowerValue ctx v
        ds' <- mapM (lowerValue ctx) ds
        v'' <- cast ctx v' $ toFunPtrType ds
        return $ LowCompCall v'' ds'
    CompSigmaElim isNoetic xs v e -> do
      let baseType = LowTypePointer $ LowTypeArray (length xs) voidPtr
      runLowerComp $ do
        basePointer <- lowerValue ctx v
        castedBasePointer <- cast ctx basePointer baseType
        ds <- loadElements ctx castedBasePointer baseType $ take (length xs) $ zip [0 ..] (repeat voidPtr)
        unless isNoetic $ free ctx castedBasePointer baseType
        forM_ (zip xs ds) $ \(x, d) -> do
          extend $ return . LowCompLet x (LowOpBitcast d voidPtr voidPtr)
        liftIO $ lowerComp ctx e
    CompUpIntro d ->
      runLower $ lowerValue ctx d
    CompUpElim x e1 e2 -> do
      e1' <- lowerComp ctx e1
      e2' <- lowerComp ctx e2
      commConv x e1' e2'
    CompEnumElim v branchList -> do
      m <- constructSwitch ctx branchList
      case m of
        Nothing ->
          return LowCompUnreachable
        Just (defaultCase, caseList) -> do
          runLowerComp $ do
            let t = LowTypePrimNum $ PrimNumInt $ IntSize 64
            castedValue <- lowerValueLetCast ctx v t
            return $ LowCompSwitch (castedValue, t) defaultCase caseList
    CompArrayAccess elemType v index -> do
      let elemType' = LowTypePrimNum elemType
      let elemSize = LowValueInt (primNumToSizeInByte elemType)
      runLower $ do
        indexVar <- lowerValueLetCast ctx index i64
        arrayVar <- lowerValue ctx v
        castedArrayVar <- cast ctx arrayVar i64
        startIndex <- load ctx (LowTypePrimNum $ PrimNumInt $ IntSize 64) arrayVar
        castedStartIndex <- cast ctx startIndex i64
        realIndex <- arith ctx "add" [indexVar, castedStartIndex]
        arrayOffset <- arith ctx "mul" [elemSize, realIndex]
        realOffset <- arith ctx "add" [LowValueInt 16, arrayOffset]
        elemAddress <- arith ctx "add" [castedArrayVar, realOffset]
        uncastedElemAddress <- uncast ctx elemAddress i64
        load ctx elemType' uncastedElemAddress

i64 :: LowType
i64 = LowTypePrimNum $ PrimNumInt $ IntSize 64

i64p :: PrimNum
i64p = PrimNumInt $ IntSize 64

load :: Context -> LowType -> LowValue -> Lower LowValue
load ctx elemType pointer = do
  tmp <- reflect ctx $ LowOpBitcast pointer voidPtr (LowTypePointer elemType)
  loaded <- reflect ctx $ LowOpLoad tmp elemType
  uncast ctx loaded elemType

store :: LowType -> LowValue -> LowValue -> Lower ()
store lowType value pointer =
  reflectCont $ LowOpStore lowType value pointer

arith :: Context -> T.Text -> [LowValue] -> Lower LowValue
arith ctx op args = do
  reflect ctx $ LowOpPrimOp (PrimOp op [i64p, i64p] i64p) args

primNumToSizeInByte :: PrimNum -> Integer
primNumToSizeInByte primNum =
  case primNum of
    PrimNumInt size ->
      toInteger $ intSizeToInt size `div` 8
    PrimNumFloat size ->
      toInteger $ floatSizeToInt size `div` 8

loadElements ::
  Context ->
  LowValue -> -- base pointer
  LowType -> -- the type of base pointer
  [(Int, LowType)] -> -- [(the index of an element, the variable to keep the loaded content)]
  Lower [LowValue]
loadElements ctx basePointer baseType values =
  case values of
    [] -> do
      return []
    (valueIndex, valueType) : xis -> do
      valuePointer <- getElemPtr ctx basePointer baseType [0, toInteger valueIndex]
      uncastedValuePointer <- uncast ctx valuePointer (LowTypePointer valueType) -- fixme: uncast this
      x <- load ctx valueType uncastedValuePointer
      xs <- loadElements ctx basePointer baseType xis
      return $ x : xs

free :: Context -> LowValue -> LowType -> Lower ()
free ctx pointer pointerType = do
  uncastedPointer <- uncast ctx pointer pointerType
  j <- liftIO $ Gensym.newCount (gensym ctx)
  reflectCont $ LowOpFree uncastedPointer pointerType j

lowerCompPrimitive :: Context -> Primitive -> Lower LowValue
lowerCompPrimitive ctx codeOp =
  case codeOp of
    PrimitivePrimOp op vs ->
      lowerCompPrimOp ctx op vs
    PrimitiveMagic der -> do
      case der of
        MagicCast _ _ value -> do
          lowerValue ctx value
        MagicStore valueLowType pointer value -> do
          ptrVar <- lowerValueLetCast ctx pointer (LowTypePointer valueLowType)
          valVar <- lowerValueLetCast ctx value valueLowType
          extend $ return . LowCompCont (LowOpStore valueLowType valVar ptrVar)
          return LowValueNull
        MagicLoad valueLowType pointer -> do
          castedPointer <- lowerValueLetCast ctx pointer (LowTypePointer valueLowType)
          result <- reflect ctx $ LowOpLoad castedPointer valueLowType
          uncast ctx result valueLowType
        MagicSyscall i args -> do
          args' <- mapM (lowerValue ctx) args
          reflect ctx $ LowOpSyscall i args'
        MagicExternal name args -> do
          args' <- mapM (lowerValue ctx) args
          liftIO $ insDeclEnv name args'
          reflect ctx $ LowOpCall (LowValueVarGlobal name) args'

lowerCompPrimOp :: Context -> PrimOp -> [Value] -> Lower LowValue
lowerCompPrimOp ctx op@(PrimOp _ domList cod) vs = do
  argVarList <- lowerValueLetCastPrimArgs ctx $ zip vs domList
  result <- reflect ctx $ LowOpPrimOp op argVarList
  uncast ctx result $ LowTypePrimNum cod

lowerValueLetCastPrimArgs :: Context -> [(Value, PrimNum)] -> Lower [LowValue]
lowerValueLetCastPrimArgs ctx dts =
  case dts of
    [] ->
      return []
    ((d, t) : rest) -> do
      argVar <- lowerValueLetCast ctx d $ LowTypePrimNum t
      argVarList <- lowerValueLetCastPrimArgs ctx rest
      return $ argVar : argVarList

cast :: Context -> LowValue -> LowType -> Lower LowValue
cast ctx v lowType = do
  (result, resultVar) <- liftIO $ newValueLocal ctx "result"
  case lowType of
    LowTypePrimNum (PrimNumInt _) -> do
      extend $ return . LowCompLet result (LowOpPointerToInt v voidPtr lowType)
    LowTypePrimNum (PrimNumFloat size) -> do
      let floatType = LowTypePrimNum $ PrimNumFloat size
      let intType = LowTypePrimNum $ PrimNumInt $ IntSize $ floatSizeToInt size
      (tmp, tmpVar) <- liftIO $ newValueLocal ctx "tmp"
      extend $
        return
          . LowCompLet tmp (LowOpPointerToInt v voidPtr intType)
          . LowCompLet result (LowOpBitcast tmpVar intType floatType)
    _ -> do
      extend $ return . LowCompLet result (LowOpBitcast v voidPtr lowType)
  return resultVar

uncast :: Context -> LowValue -> LowType -> Lower LowValue
uncast ctx castedValue lowType = do
  (result, resultVar) <- liftIO $ newValueLocal ctx "uncast"
  case lowType of
    LowTypePrimNum (PrimNumInt _) ->
      extend $ return . LowCompLet result (LowOpIntToPointer castedValue lowType voidPtr)
    LowTypePrimNum (PrimNumFloat i) -> do
      let floatType = LowTypePrimNum $ PrimNumFloat i
      let intType = LowTypePrimNum $ PrimNumInt $ IntSize $ floatSizeToInt i
      (tmp, tmpVar) <- liftIO $ newValueLocal ctx "tmp"
      extend $
        return
          . LowCompLet tmp (LowOpBitcast castedValue floatType intType)
          . LowCompLet result (LowOpIntToPointer tmpVar intType voidPtr)
    _ ->
      extend $ return . LowCompLet result (LowOpBitcast castedValue lowType voidPtr)
  return resultVar

lowerValueLetCast :: Context -> Value -> LowType -> Lower LowValue
lowerValueLetCast ctx v lowType = do
  v' <- lowerValue ctx v
  cast ctx v' lowType

lowerValue :: Context -> Value -> Lower LowValue
lowerValue ctx v =
  case v of
    ValueVarGlobal y -> do
      compDefEnv <- liftIO $ readIORef compDefEnvRef
      case Map.lookup y compDefEnv of
        Nothing ->
          liftIO $ Throw.raiseCritical' (throw ctx) $ "no such global variable is defined: " <> y
        Just (_, args, _) -> do
          liftIO $ insDeclEnvIfNecessary y args
          uncast ctx (LowValueVarGlobal y) (toFunPtrType args)
    ValueVarLocal y ->
      return $ LowValueVarLocal y
    ValueVarLocalIdeal y ->
      return $ LowValueVarLocal y
    ValueSigmaIntro ds -> do
      let arrayType = AggPtrTypeArray (length ds) voidPtr
      createAggData ctx arrayType $ zip ds (repeat voidPtr)
    ValueInt size l -> do
      uncast ctx (LowValueInt l) $ LowTypePrimNum $ PrimNumInt size
    ValueFloat size f ->
      uncast ctx (LowValueFloat size f) $ LowTypePrimNum $ PrimNumFloat size
    ValueEnumIntro (_, d) _ -> do
      uncast ctx (LowValueInt $ D.reify d) $ LowTypePrimNum $ PrimNumInt $ IntSize 64
    ValueArrayIntro elemType vs -> do
      let lenValue = LowValueInt (toInteger $ length vs)
      let elemType' = LowTypePrimNum elemType
      let pointerType = LowTypePointer $ LowTypeStruct [i64, i64, LowTypeArray (length vs) elemType']
      let elemInfoList = zip [0 ..] $ map (,elemType') vs
      let arrayType = LowTypePointer $ LowTypeArray (length vs) elemType'
      arrayLength <- arith ctx "mul" [LowValueInt (primNumToSizeInByte elemType), lenValue]
      realLength <- arith ctx "add" [LowValueInt 16, arrayLength]
      uncastedRealLength <- uncast ctx realLength i64
      pointer <- malloc ctx uncastedRealLength
      castedPointer <- cast ctx pointer pointerType
      startPointer <- getElemPtr ctx castedPointer pointerType [0, 0]
      endPointer <- getElemPtr ctx castedPointer pointerType [0, 1]
      arrayPointer <- getElemPtr ctx castedPointer pointerType [0, 2]
      store i64 (LowValueInt 0) startPointer
      store i64 lenValue endPointer
      storeElements ctx arrayPointer arrayType elemInfoList
      return pointer

malloc :: Context -> LowValue -> Lower LowValue
malloc ctx size = do
  reflect ctx $ LowOpCall (LowValueVarGlobal "malloc") [size]

getElemPtr :: Context -> LowValue -> LowType -> [Integer] -> Lower LowValue
getElemPtr ctx value valueType indexList = do
  let indexList' = map (\i -> (LowValueInt i, LowTypePrimNum $ PrimNumInt $ IntSize 32)) indexList
  reflect ctx $ LowOpGetElementPtr (value, valueType) indexList'

reflect :: Context -> LowOp -> Lower LowValue
reflect ctx op = do
  (result, resultVar) <- liftIO $ newValueLocal ctx "result"
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
constructSwitch :: Context -> [(CompEnumCase, Comp)] -> IO (Maybe (LowComp, [(Integer, LowComp)]))
constructSwitch ctx switch =
  case switch of
    [] ->
      return Nothing
    (_ :< EnumCaseDefault, code) : _ -> do
      code' <- lowerComp ctx code
      return $ Just (code', [])
    [(m :< _, code)] -> do
      constructSwitch ctx [(m :< EnumCaseDefault, code)]
    (m :< EnumCaseLabel (_, d) _, code) : rest -> do
      constructSwitch ctx $ (m :< EnumCaseInt (D.reify d), code) : rest
    (_ :< EnumCaseInt i, code) : rest -> do
      code' <- lowerComp ctx code
      mSwitch <- constructSwitch ctx rest
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
  Context ->
  AggPtrType -> -- the type of the base pointer
  [(Value, LowType)] ->
  Lower LowValue
createAggData ctx aggPtrType dts = do
  basePointer <- allocateBasePointer ctx aggPtrType
  castedBasePointer <- cast ctx basePointer $ toLowType aggPtrType
  storeElements ctx castedBasePointer (toLowType aggPtrType) $ zip [0 ..] dts
  return basePointer

getSizeInfoOf :: AggPtrType -> (LowType, Int)
getSizeInfoOf aggPtrType =
  case aggPtrType of
    AggPtrTypeArray len t ->
      (t, len)
    AggPtrTypeStruct ts ->
      (LowTypeStruct ts, 1)

allocateBasePointer :: Context -> AggPtrType -> Lower LowValue
allocateBasePointer ctx aggPtrType = do
  let (elemType, len) = getSizeInfoOf aggPtrType
  sizePointer <- getElemPtr ctx LowValueNull (LowTypePointer elemType) [toInteger len]
  c <- uncast ctx sizePointer (LowTypePointer elemType)
  reflect ctx $ LowOpAlloc c $ toLowType aggPtrType

storeElements ::
  Context ->
  LowValue -> -- base pointer
  LowType -> -- the type of base pointer (like [n x u8]*, {i8*, i8*}*, etc.)
  [(Integer, (Value, LowType))] -> -- [(the index of an element, the element to be stored)]
  Lower ()
storeElements ctx basePointer baseType values =
  case values of
    [] ->
      return ()
    (valueIndex, (value, valueType)) : ids -> do
      castedValue <- lowerValueLetCast ctx value valueType
      elemPtr <- getElemPtr ctx basePointer baseType [0, valueIndex]
      store valueType castedValue elemPtr
      storeElements ctx basePointer baseType ids

toFunPtrType :: [a] -> LowType
toFunPtrType xs =
  LowTypePointer (LowTypeFunction (map (const voidPtr) xs) voidPtr)

newValueLocal :: Context -> T.Text -> IO (Ident, LowValue)
newValueLocal ctx name = do
  x <- Gensym.newIdentFromText (gensym ctx) name
  return (x, LowValueVarLocal x)

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

registerCartesian :: Context -> T.Text -> IO ()
registerCartesian ctx name = do
  compDefEnv <- readIORef compDefEnvRef
  case Map.lookup name compDefEnv of
    Just (_, args, e) ->
      lowerComp ctx e >>= insLowDefEnv name args
    _ ->
      return ()
