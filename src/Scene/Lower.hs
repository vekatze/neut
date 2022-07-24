module Scene.Lower
  ( lower,
  )
where

import qualified Context.App as App
import qualified Context.Gensym as Gensym
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Writer.Lazy
import qualified Data.HashMap.Strict as Map
import Data.IORef
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Entity.Arity as A
import Entity.Comp
import qualified Entity.DeclarationName as DN
import qualified Entity.DefiniteDescription as DD
import qualified Entity.Discriminant as D
import Entity.EnumCase
import qualified Entity.ExternalName as EN
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

data Context = Context
  { base :: App.Context,
    declEnvRef :: IORef DN.DeclEnv
  }

specialize :: App.Context -> IO Context
specialize ctx = do
  _declEnvRef <- newIORef initialLowDeclEnv
  return $
    Context
      { base = ctx,
        declEnvRef = _declEnvRef
      }

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

lower :: App.Context -> ([CompDef], Maybe Comp) -> IO (DN.DeclEnv, [LowDef], Maybe LowComp)
lower ctx (defList, mMainTerm) = do
  lowerCtx <- specialize ctx
  initialize $ map fst defList
  case mMainTerm of
    Just mainTerm -> do
      defList' <- forM defList $ \(name, (_, args, e)) -> do
        e' <- lowerComp lowerCtx e
        return (name, (args, e'))
      mainTerm'' <- lowerComp lowerCtx mainTerm
      -- the result of "main" must be i64, not i8*
      (result, resultVar) <- Gensym.newValueVarLocalWith (App.gensym ctx) "result"
      castResult <- runLower $ lowerValueLetCast lowerCtx resultVar (LowTypePrimNum $ PrimNumInt $ IntSize 64)
      -- let result: i8* := (main-term) in {cast result to i64}
      mainTerm''' <- Just <$> commConv result mainTerm'' castResult
      declEnv <- getDeclEnv lowerCtx
      return (declEnv, defList', mainTerm''')
    Nothing -> do
      insDeclEnv lowerCtx (DN.In DD.imm) A.arityS4
      insDeclEnv lowerCtx (DN.In DD.cls) A.arityS4
      insDeclEnv lowerCtx (DN.In DD.cell) A.arityS4
      defList' <- forM defList $ \(name, (_, args, e)) -> do
        e' <- lowerComp lowerCtx e
        return (name, (args, e'))
      declEnv <- getDeclEnv lowerCtx
      return (declEnv, defList', Nothing)

initialize :: [DD.DefiniteDescription] -> IO ()
initialize nameList = do
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
        v'' <- cast (base ctx) v' $ toFunPtrType ds
        return $ LowCompCall v'' ds'
    CompSigmaElim isNoetic xs v e -> do
      let baseType = LowTypePointer $ LowTypeArray (length xs) voidPtr
      runLowerComp $ do
        basePointer <- lowerValue ctx v
        castedBasePointer <- cast (base ctx) basePointer baseType
        ds <- loadElements (base ctx) castedBasePointer baseType $ take (length xs) $ zip [0 ..] (repeat voidPtr)
        unless isNoetic $ free (base ctx) castedBasePointer baseType
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
        castedArrayVar <- cast (base ctx) arrayVar i64
        startIndex <- load (base ctx) (LowTypePrimNum $ PrimNumInt $ IntSize 64) arrayVar
        castedStartIndex <- cast (base ctx) startIndex i64
        realIndex <- arith (base ctx) "add" [indexVar, castedStartIndex]
        arrayOffset <- arith (base ctx) "mul" [elemSize, realIndex]
        realOffset <- arith (base ctx) "add" [LowValueInt 16, arrayOffset]
        elemAddress <- arith (base ctx) "add" [castedArrayVar, realOffset]
        uncastedElemAddress <- uncast (base ctx) elemAddress i64
        load (base ctx) elemType' uncastedElemAddress

i64 :: LowType
i64 = LowTypePrimNum $ PrimNumInt $ IntSize 64

i64p :: PrimNum
i64p = PrimNumInt $ IntSize 64

load :: App.Context -> LowType -> LowValue -> Lower LowValue
load ctx elemType pointer = do
  tmp <- reflect ctx $ LowOpBitcast pointer voidPtr (LowTypePointer elemType)
  loaded <- reflect ctx $ LowOpLoad tmp elemType
  uncast ctx loaded elemType

store :: LowType -> LowValue -> LowValue -> Lower ()
store lowType value pointer =
  reflectCont $ LowOpStore lowType value pointer

arith :: App.Context -> T.Text -> [LowValue] -> Lower LowValue
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
  App.Context ->
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

free :: App.Context -> LowValue -> LowType -> Lower ()
free ctx pointer pointerType = do
  uncastedPointer <- uncast ctx pointer pointerType
  j <- liftIO $ Gensym.newCount (App.gensym ctx)
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
          result <- reflect (base ctx) $ LowOpLoad castedPointer valueLowType
          uncast (base ctx) result valueLowType
        MagicSyscall i args -> do
          args' <- mapM (lowerValue ctx) args
          reflect (base ctx) $ LowOpSyscall i args'
        MagicExternal name args -> do
          args' <- mapM (lowerValue ctx) args
          liftIO $ insDeclEnv ctx (DN.Ext name) $ A.fromInt $ length args'
          reflect (base ctx) $ LowOpCall (LowValueVarExternal name) args'

lowerCompPrimOp :: Context -> PrimOp -> [Value] -> Lower LowValue
lowerCompPrimOp ctx op@(PrimOp _ domList cod) vs = do
  argVarList <- lowerValueLetCastPrimArgs ctx $ zip vs domList
  result <- reflect (base ctx) $ LowOpPrimOp op argVarList
  uncast (base ctx) result $ LowTypePrimNum cod

lowerValueLetCastPrimArgs :: Context -> [(Value, PrimNum)] -> Lower [LowValue]
lowerValueLetCastPrimArgs ctx dts =
  case dts of
    [] ->
      return []
    ((d, t) : rest) -> do
      argVar <- lowerValueLetCast ctx d $ LowTypePrimNum t
      argVarList <- lowerValueLetCastPrimArgs ctx rest
      return $ argVar : argVarList

cast :: App.Context -> LowValue -> LowType -> Lower LowValue
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

uncast :: App.Context -> LowValue -> LowType -> Lower LowValue
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
  cast (base ctx) v' lowType

lowerValue :: Context -> Value -> Lower LowValue
lowerValue ctx v =
  case v of
    ValueVarGlobal globalName arity -> do
      lowNameSet <- liftIO $ readIORef lowNameSetRef
      unless (S.member globalName lowNameSet) $
        liftIO $ insDeclEnv ctx (DN.In globalName) arity
      uncast (base ctx) (LowValueVarGlobal globalName) (toFunPtrType' arity)
    ValueVarLocal y ->
      return $ LowValueVarLocal y
    ValueVarLocalIdeal y ->
      return $ LowValueVarLocal y
    ValueSigmaIntro ds -> do
      let arrayType = AggPtrTypeArray (length ds) voidPtr
      createAggData ctx arrayType $ zip ds (repeat voidPtr)
    ValueInt size l -> do
      uncast (base ctx) (LowValueInt l) $ LowTypePrimNum $ PrimNumInt size
    ValueFloat size f ->
      uncast (base ctx) (LowValueFloat size f) $ LowTypePrimNum $ PrimNumFloat size
    ValueEnumIntro (EnumLabel _ d _) -> do
      uncast (base ctx) (LowValueInt $ D.reify d) $ LowTypePrimNum $ PrimNumInt $ IntSize 64
    ValueArrayIntro elemType vs -> do
      let lenValue = LowValueInt (toInteger $ length vs)
      let elemType' = LowTypePrimNum elemType
      let pointerType = LowTypePointer $ LowTypeStruct [i64, i64, LowTypeArray (length vs) elemType']
      let elemInfoList = zip [0 ..] $ map (,elemType') vs
      let arrayType = LowTypePointer $ LowTypeArray (length vs) elemType'
      arrayLength <- arith (base ctx) "mul" [LowValueInt (primNumToSizeInByte elemType), lenValue]
      realLength <- arith (base ctx) "add" [LowValueInt 16, arrayLength]
      uncastedRealLength <- uncast (base ctx) realLength i64
      pointer <- malloc (base ctx) uncastedRealLength
      castedPointer <- cast (base ctx) pointer pointerType
      startPointer <- getElemPtr (base ctx) castedPointer pointerType [0, 0]
      endPointer <- getElemPtr (base ctx) castedPointer pointerType [0, 1]
      arrayPointer <- getElemPtr (base ctx) castedPointer pointerType [0, 2]
      store i64 (LowValueInt 0) startPointer
      store i64 lenValue endPointer
      storeElements ctx arrayPointer arrayType elemInfoList
      return pointer

malloc :: App.Context -> LowValue -> Lower LowValue
malloc ctx size = do
  reflect ctx $ LowOpCall (LowValueVarExternal EN.malloc) [size]

getElemPtr :: App.Context -> LowValue -> LowType -> [Integer] -> Lower LowValue
getElemPtr ctx value valueType indexList = do
  let indexList' = map (\i -> (LowValueInt i, LowTypePrimNum $ PrimNumInt $ IntSize 32)) indexList
  reflect ctx $ LowOpGetElementPtr (value, valueType) indexList'

reflect :: App.Context -> LowOp -> Lower LowValue
reflect ctx op = do
  (result, resultVar) <- liftIO $ newValueLocal ctx "result"
  extend $ return . LowCompLet result op
  return resultVar

reflectCont :: LowOp -> Lower ()
reflectCont op = do
  extend $ return . LowCompCont op

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
    (m :< EnumCaseLabel (EnumLabel _ d _), code) : rest -> do
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
  basePointer <- allocateBasePointer (base ctx) aggPtrType
  castedBasePointer <- cast (base ctx) basePointer $ toLowType aggPtrType
  storeElements ctx castedBasePointer (toLowType aggPtrType) $ zip [0 ..] dts
  return basePointer

getSizeInfoOf :: AggPtrType -> (LowType, Int)
getSizeInfoOf aggPtrType =
  case aggPtrType of
    AggPtrTypeArray len t ->
      (t, len)
    AggPtrTypeStruct ts ->
      (LowTypeStruct ts, 1)

allocateBasePointer :: App.Context -> AggPtrType -> Lower LowValue
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
      elemPtr <- getElemPtr (base ctx) basePointer baseType [0, valueIndex]
      store valueType castedValue elemPtr
      storeElements ctx basePointer baseType ids

toFunPtrType :: [a] -> LowType
toFunPtrType xs =
  LowTypePointer (LowTypeFunction (map (const voidPtr) xs) voidPtr)

toFunPtrType' :: A.Arity -> LowType
toFunPtrType' arity =
  LowTypePointer (LowTypeFunction (toVoidPtrSeq arity) voidPtr)

newValueLocal :: App.Context -> T.Text -> IO (Ident, LowValue)
newValueLocal ctx name = do
  x <- Gensym.newIdentFromText (App.gensym ctx) name
  return (x, LowValueVarLocal x)

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

insDeclEnv :: Context -> DN.DeclarationName -> A.Arity -> IO ()
insDeclEnv ctx name arity =
  modifyIORef' (declEnvRef ctx) $ Map.insert name (toVoidPtrSeq arity, voidPtr)

getDeclEnv :: Context -> IO DN.DeclEnv
getDeclEnv ctx =
  readIORef (declEnvRef ctx)

toVoidPtrSeq :: A.Arity -> [LowType]
toVoidPtrSeq arity =
  map (const voidPtr) [1 .. A.reify arity]

initialLowDeclEnv :: DN.DeclEnv
initialLowDeclEnv =
  Map.fromList
    [ (DN.malloc, ([voidPtr], voidPtr)),
      (DN.free, ([voidPtr], voidPtr))
    ]
