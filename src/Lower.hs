{-# LANGUAGE TupleSections #-}

module Lower
  ( lowerMain,
    lowerOther,
  )
where

import Control.Comonad.Cofree (Cofree (..))
import Control.Monad (forM_, unless, (<=<))
import Control.Monad.Writer.Lazy
  ( MonadIO (liftIO),
    MonadWriter (tell),
    WriterT (runWriterT),
  )
import Data.Basic (CompEnumCase, EnumCaseF (..), Ident (..), asText)
import Data.Comp (Comp (..), CompDef, Primitive (..), Value (..))
import Data.Global
  ( cartClsName,
    cartImmName,
    compDefEnvRef,
    initialLowDeclEnv,
    lowDeclEnvRef,
    lowDefEnvRef,
    lowNameSetRef,
    newCount,
    newIdentFromIdent,
    newIdentFromText,
    newValueVarLocalWith,
    revEnumEnvRef,
  )
import qualified Data.HashMap.Lazy as Map
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.Log (raiseCritical')
import Data.LowComp
  ( LowComp (..),
    LowOp (..),
    LowValue (..),
    SizeInfo,
  )
import Data.LowType
  ( LowType (..),
    Magic (..),
    PrimNum (..),
    PrimOp (..),
    primNumToLowType,
    sizeAsInt,
    voidPtr,
  )
import qualified Data.Set as S
import qualified Data.Text as T

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
  castResult <- lowerValueLetCast resultVar (LowTypeInt 64) >>= uncurry knot
  -- let result: i8* := (main-term) in {cast result to i64}
  commConv result mainTerm'' castResult

lowerOther :: [CompDef] -> IO ()
lowerOther defList = do
  initialize $ map fst defList
  insDeclEnv cartImmName [(), ()]
  insDeclEnv cartClsName [(), ()]
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
      lowerCompPrimitive theta
    CompPiElimDownElim v ds -> do
      (xs, vs) <- unzip <$> mapM (const $ newValueLocal "arg") ds
      (fun, castThen) <- lowerValueLetCast v $ toFunPtrType ds
      castThenCall <- castThen $ LowCompCall fun vs
      lowerValueLet' (zip xs ds) castThenCall
    CompSigmaElim isNoetic xs v e -> do
      let basePtrType = LowTypePointer $ LowTypeArray (length xs) voidPtr -- base pointer type  ([(length xs) x ARRAY_ELEM_TYPE])
      let idxList = map (\i -> (LowValueInt i, LowTypeInt 32)) [0 ..]
      ys <- mapM newIdentFromIdent xs
      let xts' = zip xs (repeat voidPtr)
      loadContent isNoetic v basePtrType (zip idxList (zip ys xts')) e
    CompUpIntro d -> do
      (result, resultVar) <- newValueLocal "result"
      lowerValueLet result d $ LowCompReturn resultVar
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
          let t = LowTypeInt 64
          (castedValue, castThen) <- lowerValueLetCast v t
          castThen $ LowCompSwitch (castedValue, t) defaultCase caseList
    CompArrayAccess elemType v index -> do
      let elemSize = LowValueInt (primNumToSizeInByte elemType)
      runLower $ do
        indexVar <- lowerValueLetCast' index i64
        arrayVar <- lowerValueLetCast' v i64
        arrayOffset <- arith "mul" [elemSize, indexVar]
        realOffset <- arith "add" [LowValueInt 8, arrayOffset]
        elemAddress <- arith "add" [arrayVar, realOffset]
        uncastedElemAddress <- autoUncast elemAddress i64
        load (primNumToLowType elemType) uncastedElemAddress

i64 :: LowType
i64 = LowTypeInt 64

load :: LowType -> LowValue -> Lower LowValue
load elemType pointer = do
  tmp <- reflect $ LowOpBitcast pointer voidPtr (LowTypePointer elemType)
  loaded <- reflect $ LowOpLoad tmp elemType
  autoUncast loaded elemType

store :: LowType -> LowValue -> LowValue -> Lower ()
store lowType value pointer =
  reflectCont $ LowOpStore lowType value pointer

arith :: T.Text -> [LowValue] -> Lower LowValue
arith op args = do
  reflect $ LowOpPrimOp (PrimOp op [i64, i64] i64) args

primNumToSizeInByte :: PrimNum -> Integer
primNumToSizeInByte primNum =
  case primNum of
    PrimNumInt size ->
      toInteger $ size `div` 8
    PrimNumFloat size ->
      toInteger $ sizeAsInt size `div` 8

uncastList :: [(Ident, (Ident, LowType))] -> Comp -> IO LowComp
uncastList args e =
  case args of
    [] ->
      lowerComp e
    ((y, (x, et)) : yxs) -> do
      e' <- uncastList yxs e
      uncastLet x (LowValueVarLocal y) et e'

knot :: LowValue -> (LowComp -> IO LowComp) -> IO LowComp
knot value cont = do
  cont $ LowCompReturn value

loadContent ::
  Bool -> -- noetic-or-not
  Value -> -- base pointer
  LowType -> -- the type of base pointer
  [((LowValue, LowType), (Ident, (Ident, LowType)))] -> -- [(the index of an element, the variable to load the element)]
  Comp -> -- continuation
  IO LowComp
loadContent isNoetic v bt iyxs cont =
  case iyxs of
    [] ->
      lowerComp cont
    _ -> do
      let ixs = map (\(i, (y, (_, k))) -> (i, (y, k))) iyxs
      (bp, castThen) <- lowerValueLetCast v bt
      let yxs = map snd iyxs
      uncastThenCont <- uncastList yxs cont
      extractThenFreeThenUncastThenCont <- loadContent' isNoetic bp bt ixs uncastThenCont
      castThen extractThenFreeThenUncastThenCont

loadContent' ::
  Bool -> -- noetic-or-not
  LowValue -> -- base pointer
  LowType -> -- the type of base pointer
  [((LowValue, LowType), (Ident, LowType))] -> -- [(the index of an element, the variable to keep the loaded content)]
  LowComp -> -- continuation
  IO LowComp
loadContent' isNoetic bp bt values cont =
  case values of
    []
      | isNoetic ->
        return cont
      | otherwise -> do
        (uncastedBasePointer, uncastBasePointerThen) <- uncast bp bt
        j <- newCount
        return $ uncastBasePointerThen $ LowCompCont (LowOpFree uncastedBasePointer bt j) cont
    (i, (x, et)) : xis -> do
      cont' <- loadContent' isNoetic bp bt xis cont
      (posName, pos) <- newValueLocal' (Just $ asText x)
      return $
        LowCompLet
          posName
          (LowOpGetElementPtr (bp, bt) [(LowValueInt 0, LowTypeInt 32), i])
          $ LowCompLet x (LowOpLoad pos et) cont'

lowerCompPrimitive :: Primitive -> IO LowComp
lowerCompPrimitive codeOp =
  case codeOp of
    PrimitivePrimOp op vs ->
      lowerCompPrimOp op vs
    PrimitiveMagic der -> do
      case der of
        MagicCast _ _ value -> do
          (x, v) <- newValueLocal "cast-arg"
          lowerValueLet x value $ LowCompReturn v
        MagicStore valueLowType pointer value -> do
          runLower $ do
            ptrVar <- lowerValueLetCast' pointer (LowTypePointer valueLowType)
            valVar <- lowerValueLetCast' value valueLowType
            extend $ return . LowCompCont (LowOpStore valueLowType valVar ptrVar)
            return LowValueNull
        MagicLoad valueLowType pointer -> do
          runLower $ do
            castedPointer <- lowerValueLetCast' pointer (LowTypePointer valueLowType)
            result <- reflect $ LowOpLoad castedPointer valueLowType
            autoUncast result valueLowType
        MagicSyscall i args -> do
          (xs, vs) <- unzip <$> mapM (const $ newValueLocal "sys-call-arg") args
          (result, resultVar) <- newValueLocal "result"
          lowerValueLet' (zip xs args) $
            LowCompLet result (LowOpSyscall i vs) $
              LowCompReturn resultVar
        MagicExternal name args -> do
          (xs, vs) <- unzip <$> mapM (const $ newValueLocal "ext-call-arg") args
          insDeclEnv name vs
          lowerValueLet' (zip xs args) $ LowCompCall (LowValueVarGlobal name) vs
        MagicCreateArray elemType args -> do
          let arrayType = AggPtrTypeArray (length args) elemType
          let argTypeList = zip args (repeat elemType)
          (result, resultVar) <- newValueLocal "result"
          storeContent result arrayType argTypeList (LowCompReturn resultVar)

lowerCompPrimOp :: PrimOp -> [Value] -> IO LowComp
lowerCompPrimOp op@(PrimOp _ domList cod) vs = do
  (argVarList, castArgsThen) <- lowerValueLetCastPrimArgs $ zip vs domList
  (result, resultVar) <- newValueLocal "result"
  (uncastedResult, uncastResultThen) <- uncast resultVar cod
  castArgsThen $
    LowCompLet result (LowOpPrimOp op argVarList) $
      uncastResultThen $
        LowCompReturn uncastedResult

lowerValueLetCastPrimArgs :: [(Value, LowType)] -> IO ([LowValue], LowComp -> IO LowComp)
lowerValueLetCastPrimArgs dts =
  case dts of
    [] ->
      return ([], return)
    ((d, t) : rest) -> do
      (argVarList, cont) <- lowerValueLetCastPrimArgs rest
      (argVar, castThen) <- lowerValueLetCast d t
      return (argVar : argVarList, castThen <=< cont)

cast :: LowValue -> LowType -> IO (LowValue, LowComp -> LowComp)
cast v lowType = do
  (result, resultVar) <- newValueLocal "result"
  case lowType of
    LowTypeInt _ -> do
      return
        ( resultVar,
          LowCompLet
            result
            (LowOpPointerToInt v voidPtr lowType)
        )
    LowTypeFloat size -> do
      let floatType = LowTypeFloat size
      let intType = LowTypeInt $ sizeAsInt size
      (tmp, tmpVar) <- newValueLocal "foo"
      return
        ( resultVar,
          LowCompLet tmp (LowOpPointerToInt v voidPtr intType)
            . LowCompLet result (LowOpBitcast tmpVar intType floatType)
        )
    _ -> do
      return
        ( resultVar,
          LowCompLet result (LowOpBitcast v voidPtr lowType)
        )

autoCast :: LowValue -> LowType -> Lower LowValue
autoCast v lowType = do
  (castedValue, castThen) <- liftIO $ cast v lowType
  extend $ return . castThen
  return castedValue

uncast :: LowValue -> LowType -> IO (LowValue, LowComp -> LowComp)
uncast castedValue lowType = do
  (result, resultVar) <- newValueLocal "uncast"
  case lowType of
    LowTypeInt _ ->
      return
        ( resultVar,
          LowCompLet result (LowOpIntToPointer castedValue lowType voidPtr)
        )
    LowTypeFloat i -> do
      let floatType = LowTypeFloat i
      let intType = LowTypeInt $ sizeAsInt i
      (tmp, tmpVar) <- newValueLocal "tmp"
      return
        ( resultVar,
          LowCompLet tmp (LowOpBitcast castedValue floatType intType)
            . LowCompLet result (LowOpIntToPointer tmpVar intType voidPtr)
        )
    _ -> do
      return
        ( resultVar,
          LowCompLet result (LowOpBitcast castedValue lowType voidPtr)
        )

autoUncast :: LowValue -> LowType -> Lower LowValue
autoUncast castedValue lowType = do
  (uncastedValue, uncastThen) <- liftIO $ uncast castedValue lowType
  extend $ return . uncastThen
  return uncastedValue

uncastLet :: Ident -> LowValue -> LowType -> LowComp -> IO LowComp
uncastLet x d lowType cont = do
  (uncastedValue, uncastValueThen) <- uncast d lowType
  return $
    uncastValueThen $ LowCompLet x (LowOpBitcast uncastedValue voidPtr voidPtr) cont

lowerValueLetCast :: Value -> LowType -> IO (LowValue, LowComp -> IO LowComp)
lowerValueLetCast v lowType = do
  (tmp, tmpVar) <- newValueLocal "tmp"
  (castedValue, castThen) <- cast tmpVar lowType
  return
    ( castedValue,
      lowerValueLet tmp v . castThen
    )

lowerValueLetCast' :: Value -> LowType -> Lower LowValue
lowerValueLetCast' v lowType = do
  (tmp, tmpVar) <- liftIO $ newValueLocal "tmp"
  (castedValue, castThen) <- liftIO $ cast tmpVar lowType
  extend $ lowerValueLet tmp v . castThen
  return castedValue

-- `lowerValueLet x d cont` binds the data `d` to the variable `x`, and computes the
-- continuation `cont`.
lowerValueLet :: Ident -> Value -> LowComp -> IO LowComp
lowerValueLet x lowerValue cont =
  case lowerValue of
    ValueVarGlobal y -> do
      compDefEnv <- readIORef compDefEnvRef
      case Map.lookup y compDefEnv of
        Nothing ->
          raiseCritical' $ "no such global variable is defined: " <> y
        Just (_, args, _) -> do
          insDeclEnvIfNecessary y args
          uncastLet x (LowValueVarGlobal y) (toFunPtrType args) cont
    ValueVarLocal y ->
      uncastLet x (LowValueVarLocal y) voidPtr cont
    ValueVarLocalIdeal y ->
      uncastLet x (LowValueVarLocal y) voidPtr cont
    ValueSigmaIntro ds -> do
      let arrayType = AggPtrTypeArray (length ds) voidPtr
      let dts = zip ds (repeat voidPtr)
      storeContent x arrayType dts cont
    ValueInt size l ->
      uncastLet x (LowValueInt l) (LowTypeInt size) cont
    ValueFloat size f ->
      uncastLet x (LowValueFloat size f) (LowTypeFloat size) cont
    ValueEnumIntro l -> do
      i <- toInteger <$> enumValueToInteger l
      uncastLet x (LowValueInt i) (LowTypeInt 64) cont
    ValueArrayIntro elemType vs -> do
      let lenValue = LowValueInt (toInteger $ length vs)
      let elemType' = primNumToLowType elemType
      let pointerType = LowTypePointer $ LowTypeStruct [i64, LowTypeArray (length vs) elemType']
      let elemInfoList = zip [0 ..] $ map (,elemType') vs
      let arrayType = LowTypePointer $ LowTypeArray (length vs) elemType'
      runLower $ do
        arrayLength <- arith "mul" [LowValueInt (primNumToSizeInByte elemType), lenValue]
        realLength <- arith "add" [LowValueInt 8, arrayLength]
        uncastedRealLength <- autoUncast realLength i64
        pointer <- malloc uncastedRealLength
        castedPointer <- autoCast pointer pointerType
        lengthPointer <- getElemPtr castedPointer pointerType [0, 0]
        arrayPointer <- getElemPtr castedPointer pointerType [0, 1]
        store i64 lenValue lengthPointer
        extend $ storeContent' arrayPointer arrayType elemInfoList
        return pointer

malloc :: LowValue -> Lower LowValue
malloc size = do
  reflect $ LowOpCall (LowValueVarGlobal "malloc") [size]

getElemPtr :: LowValue -> LowType -> [Integer] -> Lower LowValue
getElemPtr value valueType indexList = do
  let indexList' = map (\i -> (LowValueInt i, LowTypeInt 32)) indexList
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

lowerValueLet' :: [(Ident, Value)] -> LowComp -> IO LowComp
lowerValueLet' binder cont =
  case binder of
    [] ->
      return cont
    (x, d) : rest -> do
      cont' <- lowerValueLet' rest cont
      lowerValueLet x d cont'

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

storeContent ::
  Ident ->
  AggPtrType ->
  [(Value, LowType)] ->
  LowComp ->
  IO LowComp
storeContent reg aggPtrType dts cont = do
  let lowType = toLowType aggPtrType
  (castedValue, castThen) <- lowerValueLetCast (ValueVarLocal reg) lowType
  storeThenCont <- storeContent' castedValue lowType (zip [0 ..] dts) cont
  castThenStoreThenCont <- castThen storeThenCont
  case aggPtrType of
    AggPtrTypeStruct ts ->
      storeContent'' reg (LowTypeStruct ts) lowType 1 castThenStoreThenCont
    AggPtrTypeArray len t ->
      storeContent'' reg t lowType len castThenStoreThenCont

storeContent' ::
  LowValue -> -- base pointer
  LowType -> -- the type of base pointer (like [n x u8]*, {i8*, i8*}*, etc.)
  [(Integer, (Value, LowType))] -> -- [(the index of an element, the element to be stored)]
  LowComp -> -- continuation
  IO LowComp
storeContent' bp bt values cont =
  case values of
    [] ->
      return cont
    (i, (d, et)) : ids -> do
      cont' <- storeContent' bp bt ids cont
      (locName, loc) <- newValueLocal "location"
      (castedValue, castThen) <- lowerValueLetCast d et
      let it = indexTypeOf bt
      castThen $
        LowCompLet
          locName
          (LowOpGetElementPtr (bp, bt) [(LowValueInt 0, LowTypeInt 32), (LowValueInt i, it)])
          $ LowCompCont (LowOpStore et castedValue loc) cont'

storeContent'' :: Ident -> LowType -> SizeInfo -> Int -> LowComp -> IO LowComp
storeContent'' reg elemType sizeInfo len cont = do
  (tmp, tmpVar) <- newValueLocal $ "sizeof-" <> asText reg
  (c, cVar) <- newValueLocal $ "sizeof-" <> asText reg
  uncastThenAllocThenCont <- uncastLet c tmpVar (LowTypePointer elemType) (LowCompLet reg (LowOpAlloc cVar sizeInfo) cont)
  return $
    LowCompLet
      tmp
      ( LowOpGetElementPtr
          (LowValueNull, LowTypePointer elemType)
          [(LowValueInt (toInteger len), LowTypeInt 64)]
      )
      uncastThenAllocThenCont

indexTypeOf :: LowType -> LowType
indexTypeOf lowType =
  case lowType of
    LowTypePointer (LowTypeStruct _) ->
      LowTypeInt 32
    _ ->
      LowTypeInt 64

toFunPtrType :: [a] -> LowType
toFunPtrType xs =
  LowTypePointer (LowTypeFunction (map (const voidPtr) xs) voidPtr)

newValueLocal :: T.Text -> IO (Ident, LowValue)
newValueLocal name = do
  x <- newIdentFromText name
  return (x, LowValueVarLocal x)

newValueLocal' :: Maybe T.Text -> IO (Ident, LowValue)
newValueLocal' mName = do
  x <- newNameWith mName
  return (x, LowValueVarLocal x)

newNameWith :: Maybe T.Text -> IO Ident
newNameWith mName =
  case mName of
    Nothing ->
      newIdentFromText "var"
    Just name ->
      newIdentFromText name

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
