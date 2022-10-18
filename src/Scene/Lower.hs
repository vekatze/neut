module Scene.Lower
  ( lower,
    Context (..),
  )
where

import qualified Context.Gensym as Gensym
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Writer.Lazy
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Entity.Arity as A
import qualified Entity.Comp as C
import qualified Entity.DeclarationName as DN
import qualified Entity.DefiniteDescription as DD
import qualified Entity.Discriminant as D
import Entity.EnumCase
import Entity.Ident
import Entity.LowComp
import Entity.LowType
import Entity.Magic
import Entity.PrimNum
import Entity.PrimNumSize
import Entity.PrimNumSize.ToInt
import Entity.PrimOp

-- fixme: remove WriterT
type Lower m = WriterT (Cont m) m

newtype Cont m = Cont (LowComp -> m LowComp)

instance Monad m => Semigroup (Cont m) where
  Cont newCont <> Cont oldCont =
    Cont $ newCont <=< oldCont

instance Monad m => Monoid (Cont m) where
  mempty =
    Cont return

class (Gensym.Context m) => Context m where
  initialize :: [DD.DefiniteDescription] -> m ()
  getDeclEnv :: m DN.DeclEnv
  insDeclEnv :: DN.DeclarationName -> A.Arity -> m ()
  getDefinedNameSet :: m (S.Set DD.DefiniteDescription)

extend :: Monad m => (LowComp -> m LowComp) -> Lower m ()
extend =
  tell . Cont

runLower :: Monad m => Lower m LowValue -> m LowComp
runLower m = do
  (a, Cont b) <- runWriterT m
  b $ LowCompReturn a

runLowerComp :: Monad m => Lower m LowComp -> m LowComp
runLowerComp m = do
  (a, Cont b) <- runWriterT m
  b a

lower :: Context m => ([C.CompDef], Maybe C.Comp) -> m (DN.DeclEnv, [LowDef], Maybe LowComp)
lower (defList, mMainTerm) = do
  initialize $ map fst defList
  case mMainTerm of
    Just mainTerm -> do
      defList' <- forM defList $ \(name, (_, args, e)) -> do
        e' <- lowerComp e
        return (name, (args, e'))
      mainTerm'' <- lowerComp mainTerm
      -- the result of "main" must be i64, not i8*
      (result, resultVar) <- Gensym.newValueVarLocalWith "result"
      castResult <- runLower $ lowerValueLetCast resultVar (LowTypePrimNum $ PrimNumInt $ IntSize 64)
      -- let result: i8* := (main-term) in {cast result to i64}
      mainTerm''' <- Just <$> commConv result mainTerm'' castResult
      declEnv <- getDeclEnv
      return (declEnv, defList', mainTerm''')
    Nothing -> do
      insDeclEnv (DN.In DD.imm) A.arityS4
      insDeclEnv (DN.In DD.cls) A.arityS4
      insDeclEnv (DN.In DD.cell) A.arityS4
      defList' <- forM defList $ \(name, (_, args, e)) -> do
        e' <- lowerComp e
        return (name, (args, e'))
      declEnv <- getDeclEnv
      return (declEnv, defList', Nothing)

lowerComp :: Context m => C.Comp -> m LowComp
lowerComp term =
  case term of
    C.Primitive theta ->
      runLower $ lowerCompPrimitive theta
    C.PiElimDownElim v ds -> do
      runLowerComp $ do
        v' <- lowerValue v
        ds' <- mapM lowerValue ds
        v'' <- cast v' $ toFunPtrType ds
        return $ LowCompCall v'' ds'
    C.SigmaElim shouldDeallocate xs v e -> do
      let baseType = LowTypePointer $ LowTypeArray (length xs) voidPtr
      runLowerComp $ do
        basePointer <- lowerValue v
        castedBasePointer <- cast basePointer baseType
        ds <- loadElements castedBasePointer baseType $ take (length xs) $ zip [0 ..] (repeat voidPtr)
        when shouldDeallocate $ do
          free castedBasePointer baseType
        forM_ (zip xs ds) $ \(x, d) -> do
          extend $ return . LowCompLet x (LowOpBitcast d voidPtr voidPtr)
        lift $ lowerComp e
    C.UpIntro d ->
      runLower $ lowerValue d
    C.UpElim x e1 e2 -> do
      e1' <- lowerComp e1
      e2' <- lowerComp e2
      commConv x e1' e2'
    C.EnumElim v branchList -> do
      m <- constructSwitch branchList
      case m of
        Nothing ->
          return LowCompUnreachable
        Just (defaultCase, caseList) -> do
          runLowerComp $ do
            let t = LowTypePrimNum $ PrimNumInt $ IntSize 64
            castedValue <- lowerValueLetCast v t
            return $ LowCompSwitch (castedValue, t) defaultCase caseList

load :: Context m => LowType -> LowValue -> Lower m LowValue
load elemType pointer = do
  tmp <- reflect $ LowOpBitcast pointer voidPtr (LowTypePointer elemType)
  loaded <- reflect $ LowOpLoad tmp elemType
  uncast loaded elemType

store :: Monad m => LowType -> LowValue -> LowValue -> Lower m ()
store lowType value pointer =
  reflectCont $ LowOpStore lowType value pointer

loadElements ::
  Context m =>
  LowValue -> -- base pointer
  LowType -> -- the type of base pointer
  [(Int, LowType)] -> -- [(the index of an element, the variable to keep the loaded content)]
  Lower m [LowValue]
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

free :: Context m => LowValue -> LowType -> Lower m ()
free pointer pointerType = do
  uncastedPointer <- uncast pointer pointerType
  j <- lift Gensym.newCount
  reflectCont $ LowOpFree uncastedPointer pointerType j

lowerCompPrimitive :: Context m => C.Primitive -> Lower m LowValue
lowerCompPrimitive codeOp =
  case codeOp of
    C.PrimOp op vs ->
      lowerCompPrimOp op vs
    C.Magic der -> do
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
          lift $ insDeclEnv (DN.Ext name) $ A.fromInt $ length args'
          reflect $ LowOpCall (LowValueVarExternal name) args'

lowerCompPrimOp :: Context m => PrimOp -> [C.Value] -> Lower m LowValue
lowerCompPrimOp op@(PrimOp _ domList cod) vs = do
  argVarList <- lowerValueLetCastPrimArgs $ zip vs domList
  result <- reflect $ LowOpPrimOp op argVarList
  uncast result $ LowTypePrimNum cod

lowerValueLetCastPrimArgs :: Context m => [(C.Value, PrimNum)] -> Lower m [LowValue]
lowerValueLetCastPrimArgs dts =
  case dts of
    [] ->
      return []
    ((d, t) : rest) -> do
      argVar <- lowerValueLetCast d $ LowTypePrimNum t
      argVarList <- lowerValueLetCastPrimArgs rest
      return $ argVar : argVarList

cast :: Context m => LowValue -> LowType -> Lower m LowValue
cast v lowType = do
  (result, resultVar) <- lift $ newValueLocal "result"
  case lowType of
    LowTypePrimNum (PrimNumInt _) -> do
      extend $ return . LowCompLet result (LowOpPointerToInt v voidPtr lowType)
    LowTypePrimNum (PrimNumFloat size) -> do
      let floatType = LowTypePrimNum $ PrimNumFloat size
      let intType = LowTypePrimNum $ PrimNumInt $ IntSize $ floatSizeToInt size
      (tmp, tmpVar) <- lift $ newValueLocal "tmp"
      extend $
        return
          . LowCompLet tmp (LowOpPointerToInt v voidPtr intType)
          . LowCompLet result (LowOpBitcast tmpVar intType floatType)
    _ -> do
      extend $ return . LowCompLet result (LowOpBitcast v voidPtr lowType)
  return resultVar

uncast :: Context m => LowValue -> LowType -> Lower m LowValue
uncast castedValue lowType = do
  (result, resultVar) <- lift $ newValueLocal "uncast"
  case lowType of
    LowTypePrimNum (PrimNumInt _) ->
      extend $ return . LowCompLet result (LowOpIntToPointer castedValue lowType voidPtr)
    LowTypePrimNum (PrimNumFloat i) -> do
      let floatType = LowTypePrimNum $ PrimNumFloat i
      let intType = LowTypePrimNum $ PrimNumInt $ IntSize $ floatSizeToInt i
      (tmp, tmpVar) <- lift $ newValueLocal "tmp"
      extend $
        return
          . LowCompLet tmp (LowOpBitcast castedValue floatType intType)
          . LowCompLet result (LowOpIntToPointer tmpVar intType voidPtr)
    _ ->
      extend $ return . LowCompLet result (LowOpBitcast castedValue lowType voidPtr)
  return resultVar

lowerValueLetCast :: Context m => C.Value -> LowType -> Lower m LowValue
lowerValueLetCast v lowType = do
  v' <- lowerValue v
  cast v' lowType

lowerValue :: Context m => C.Value -> Lower m LowValue
lowerValue v =
  case v of
    C.VarGlobal globalName arity -> do
      lowNameSet <- lift getDefinedNameSet
      unless (S.member globalName lowNameSet) $
        lift $
          insDeclEnv (DN.In globalName) arity
      uncast (LowValueVarGlobal globalName) (toFunPtrType' arity)
    C.VarLocal y ->
      return $ LowValueVarLocal y
    C.SigmaIntro ds -> do
      let arrayType = AggPtrTypeArray (length ds) voidPtr
      createAggData arrayType $ zip ds (repeat voidPtr)
    C.Int size l -> do
      uncast (LowValueInt l) $ LowTypePrimNum $ PrimNumInt size
    C.Float size f ->
      uncast (LowValueFloat size f) $ LowTypePrimNum $ PrimNumFloat size
    C.EnumIntro (EnumLabel _ d _) -> do
      uncast (LowValueInt $ D.reify d) $ LowTypePrimNum $ PrimNumInt $ IntSize 64

getElemPtr :: Context m => LowValue -> LowType -> [Integer] -> Lower m LowValue
getElemPtr value valueType indexList = do
  let indexList' = map (\i -> (LowValueInt i, LowTypePrimNum $ PrimNumInt $ IntSize 32)) indexList
  reflect $ LowOpGetElementPtr (value, valueType) indexList'

reflect :: Context m => LowOp -> Lower m LowValue
reflect op = do
  (result, resultVar) <- lift $ newValueLocal "result"
  extend $ return . LowCompLet result op
  return resultVar

reflectCont :: Monad m => LowOp -> Lower m ()
reflectCont op = do
  extend $ return . LowCompCont op

-- returns Nothing iff the branch list is empty
constructSwitch :: Context m => [(CompEnumCase, C.Comp)] -> m (Maybe (LowComp, [(Integer, LowComp)]))
constructSwitch switch =
  case switch of
    [] ->
      return Nothing
    (_ :< EnumCaseDefault, code) : _ -> do
      code' <- lowerComp code
      return $ Just (code', [])
    [(m :< _, code)] -> do
      constructSwitch [(m :< EnumCaseDefault, code)]
    (m :< EnumCaseLabel (EnumLabel _ d _), code) : rest -> do
      constructSwitch $ (m :< EnumCaseInt (D.reify d), code) : rest
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
  Context m =>
  AggPtrType -> -- the type of the base pointer
  [(C.Value, LowType)] ->
  Lower m LowValue
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

allocateBasePointer :: Context m => AggPtrType -> Lower m LowValue
allocateBasePointer aggPtrType = do
  let (elemType, len) = getSizeInfoOf aggPtrType
  sizePointer <- getElemPtr LowValueNull (LowTypePointer elemType) [toInteger len]
  c <- uncast sizePointer (LowTypePointer elemType)
  reflect $ LowOpAlloc c $ toLowType aggPtrType

storeElements ::
  Context m =>
  LowValue -> -- base pointer
  LowType -> -- the type of base pointer (like [n x u8]*, {i8*, i8*}*, etc.)
  [(Integer, (C.Value, LowType))] -> -- [(the index of an element, the element to be stored)]
  Lower m ()
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

toFunPtrType' :: A.Arity -> LowType
toFunPtrType' arity =
  LowTypePointer (LowTypeFunction (toVoidPtrSeq arity) voidPtr)

newValueLocal :: Context m => T.Text -> m (Ident, LowValue)
newValueLocal name = do
  x <- Gensym.newIdentFromText name
  return (x, LowValueVarLocal x)

commConv :: Monad m => Ident -> LowComp -> LowComp -> m LowComp
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
