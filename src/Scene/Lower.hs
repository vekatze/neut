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
import qualified Entity.EnumCase as EC
import Entity.Ident
import qualified Entity.LowComp as LC
import qualified Entity.LowType as LT
import Entity.Magic
import Entity.PrimNum
import Entity.PrimNumSize
import Entity.PrimNumSize.ToInt
import Entity.PrimOp

-- fixme: remove WriterT
type Lower m = WriterT (Cont m) m

newtype Cont m = Cont (LC.Comp -> m LC.Comp)

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

extend :: Monad m => (LC.Comp -> m LC.Comp) -> Lower m ()
extend =
  tell . Cont

runLower :: Monad m => Lower m LC.Value -> m LC.Comp
runLower m = do
  (a, Cont b) <- runWriterT m
  b $ LC.Return a

runLowerComp :: Monad m => Lower m LC.Comp -> m LC.Comp
runLowerComp m = do
  (a, Cont b) <- runWriterT m
  b a

lower :: Context m => ([C.CompDef], Maybe C.Comp) -> m (DN.DeclEnv, [LC.Def], Maybe LC.Comp)
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
      castResult <- runLower $ lowerValueLetCast resultVar (LT.PrimNum $ PrimNumInt $ IntSize 64)
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

lowerComp :: Context m => C.Comp -> m LC.Comp
lowerComp term =
  case term of
    C.Primitive theta ->
      runLower $ lowerCompPrimitive theta
    C.PiElimDownElim v ds -> do
      runLowerComp $ do
        v' <- lowerValue v
        ds' <- mapM lowerValue ds
        v'' <- cast v' $ toFunPtrType ds
        return $ LC.TailCall v'' ds'
    C.SigmaElim shouldDeallocate xs v e -> do
      let baseType = LT.Pointer $ LT.Array (length xs) LT.voidPtr
      runLowerComp $ do
        basePointer <- lowerValue v
        castedBasePointer <- cast basePointer baseType
        ds <- loadElements castedBasePointer baseType $ take (length xs) $ zip [0 ..] (repeat LT.voidPtr)
        when shouldDeallocate $ do
          free castedBasePointer baseType
        forM_ (zip xs ds) $ \(x, d) -> do
          extend $ return . LC.Let x (LC.Bitcast d LT.voidPtr LT.voidPtr)
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
          return LC.Unreachable
        Just (defaultCase, caseList) -> do
          runLowerComp $ do
            let t = LT.PrimNum $ PrimNumInt $ IntSize 64
            castedValue <- lowerValueLetCast v t
            return $ LC.Switch (castedValue, t) defaultCase caseList

load :: Context m => LT.LowType -> LC.Value -> Lower m LC.Value
load elemType pointer = do
  tmp <- reflect $ LC.Bitcast pointer LT.voidPtr (LT.Pointer elemType)
  loaded <- reflect $ LC.Load tmp elemType
  uncast loaded elemType

store :: Monad m => LT.LowType -> LC.Value -> LC.Value -> Lower m ()
store lowType value pointer =
  reflectCont $ LC.Store lowType value pointer

loadElements ::
  Context m =>
  LC.Value -> -- base pointer
  LT.LowType -> -- the type of base pointer
  [(Int, LT.LowType)] -> -- [(the index of an element, the variable to keep the loaded content)]
  Lower m [LC.Value]
loadElements basePointer baseType values =
  case values of
    [] -> do
      return []
    (valueIndex, valueType) : xis -> do
      valuePointer <- getElemPtr basePointer baseType [0, toInteger valueIndex]
      uncastedValuePointer <- uncast valuePointer (LT.Pointer valueType) -- fixme: uncast this
      x <- load valueType uncastedValuePointer
      xs <- loadElements basePointer baseType xis
      return $ x : xs

free :: Context m => LC.Value -> LT.LowType -> Lower m ()
free pointer pointerType = do
  uncastedPointer <- uncast pointer pointerType
  j <- lift Gensym.newCount
  reflectCont $ LC.Free uncastedPointer pointerType j

lowerCompPrimitive :: Context m => C.Primitive -> Lower m LC.Value
lowerCompPrimitive codeOp =
  case codeOp of
    C.PrimOp op vs ->
      lowerCompPrimOp op vs
    C.Magic der -> do
      case der of
        MagicCast _ _ value -> do
          lowerValue value
        MagicStore valueLowType pointer value -> do
          ptrVar <- lowerValueLetCast pointer (LT.Pointer valueLowType)
          valVar <- lowerValueLetCast value valueLowType
          extend $ return . LC.Cont (LC.Store valueLowType valVar ptrVar)
          return LC.Null
        MagicLoad valueLowType pointer -> do
          castedPointer <- lowerValueLetCast pointer (LT.Pointer valueLowType)
          result <- reflect $ LC.Load castedPointer valueLowType
          uncast result valueLowType
        MagicSyscall i args -> do
          args' <- mapM lowerValue args
          reflect $ LC.Syscall i args'
        MagicExternal name args -> do
          args' <- mapM lowerValue args
          lift $ insDeclEnv (DN.Ext name) $ A.fromInt $ length args'
          reflect $ LC.Call (LC.VarExternal name) args'

lowerCompPrimOp :: Context m => PrimOp -> [C.Value] -> Lower m LC.Value
lowerCompPrimOp op@(PrimOp _ domList cod) vs = do
  argVarList <- lowerValueLetCastPrimArgs $ zip vs domList
  result <- reflect $ LC.PrimOp op argVarList
  uncast result $ LT.PrimNum cod

lowerValueLetCastPrimArgs :: Context m => [(C.Value, PrimNum)] -> Lower m [LC.Value]
lowerValueLetCastPrimArgs dts =
  case dts of
    [] ->
      return []
    ((d, t) : rest) -> do
      argVar <- lowerValueLetCast d $ LT.PrimNum t
      argVarList <- lowerValueLetCastPrimArgs rest
      return $ argVar : argVarList

cast :: Context m => LC.Value -> LT.LowType -> Lower m LC.Value
cast v lowType = do
  (result, resultVar) <- lift $ newValueLocal "result"
  case lowType of
    LT.PrimNum (PrimNumInt _) -> do
      extend $ return . LC.Let result (LC.PointerToInt v LT.voidPtr lowType)
    LT.PrimNum (PrimNumFloat size) -> do
      let floatType = LT.PrimNum $ PrimNumFloat size
      let intType = LT.PrimNum $ PrimNumInt $ IntSize $ floatSizeToInt size
      (tmp, tmpVar) <- lift $ newValueLocal "tmp"
      extend $
        return
          . LC.Let tmp (LC.PointerToInt v LT.voidPtr intType)
          . LC.Let result (LC.Bitcast tmpVar intType floatType)
    _ -> do
      extend $ return . LC.Let result (LC.Bitcast v LT.voidPtr lowType)
  return resultVar

uncast :: Context m => LC.Value -> LT.LowType -> Lower m LC.Value
uncast castedValue lowType = do
  (result, resultVar) <- lift $ newValueLocal "uncast"
  case lowType of
    LT.PrimNum (PrimNumInt _) ->
      extend $ return . LC.Let result (LC.IntToPointer castedValue lowType LT.voidPtr)
    LT.PrimNum (PrimNumFloat i) -> do
      let floatType = LT.PrimNum $ PrimNumFloat i
      let intType = LT.PrimNum $ PrimNumInt $ IntSize $ floatSizeToInt i
      (tmp, tmpVar) <- lift $ newValueLocal "tmp"
      extend $
        return
          . LC.Let tmp (LC.Bitcast castedValue floatType intType)
          . LC.Let result (LC.IntToPointer tmpVar intType LT.voidPtr)
    _ ->
      extend $ return . LC.Let result (LC.Bitcast castedValue lowType LT.voidPtr)
  return resultVar

lowerValueLetCast :: Context m => C.Value -> LT.LowType -> Lower m LC.Value
lowerValueLetCast v lowType = do
  v' <- lowerValue v
  cast v' lowType

lowerValue :: Context m => C.Value -> Lower m LC.Value
lowerValue v =
  case v of
    C.VarGlobal globalName arity -> do
      lowNameSet <- lift getDefinedNameSet
      unless (S.member globalName lowNameSet) $
        lift $
          insDeclEnv (DN.In globalName) arity
      uncast (LC.VarGlobal globalName) (toFunPtrType' arity)
    C.VarLocal y ->
      return $ LC.VarLocal y
    C.SigmaIntro ds -> do
      let arrayType = AggPtrTypeArray (length ds) LT.voidPtr
      createAggData arrayType $ zip ds (repeat LT.voidPtr)
    C.Int size l -> do
      uncast (LC.Int l) $ LT.PrimNum $ PrimNumInt size
    C.Float size f ->
      uncast (LC.Float size f) $ LT.PrimNum $ PrimNumFloat size
    C.EnumIntro (EC.EnumLabel _ d _) -> do
      uncast (LC.Int $ D.reify d) $ LT.PrimNum $ PrimNumInt $ IntSize 64

getElemPtr :: Context m => LC.Value -> LT.LowType -> [Integer] -> Lower m LC.Value
getElemPtr value valueType indexList = do
  let indexList' = map (\i -> (LC.Int i, LT.PrimNum $ PrimNumInt $ IntSize 32)) indexList
  reflect $ LC.GetElementPtr (value, valueType) indexList'

reflect :: Context m => LC.Op -> Lower m LC.Value
reflect op = do
  (result, resultVar) <- lift $ newValueLocal "result"
  extend $ return . LC.Let result op
  return resultVar

reflectCont :: Monad m => LC.Op -> Lower m ()
reflectCont op = do
  extend $ return . LC.Cont op

-- returns Nothing iff the branch list is empty
constructSwitch :: Context m => [(EC.CompEnumCase, C.Comp)] -> m (Maybe (LC.Comp, [(Integer, LC.Comp)]))
constructSwitch switch =
  case switch of
    [] ->
      return Nothing
    (_ :< EC.Default, code) : _ -> do
      code' <- lowerComp code
      return $ Just (code', [])
    [(m :< _, code)] -> do
      constructSwitch [(m :< EC.Default, code)]
    (m :< EC.Label (EC.EnumLabel _ d _), code) : rest -> do
      constructSwitch $ (m :< EC.Int (D.reify d), code) : rest
    (_ :< EC.Int i, code) : rest -> do
      code' <- lowerComp code
      mSwitch <- constructSwitch rest
      return $ do
        (defaultCase, caseList) <- mSwitch
        return (defaultCase, (i, code') : caseList)

data AggPtrType
  = AggPtrTypeArray Int LT.LowType
  | AggPtrTypeStruct [LT.LowType]

toLowType :: AggPtrType -> LT.LowType
toLowType aggPtrType =
  case aggPtrType of
    AggPtrTypeArray i t ->
      LT.Pointer $ LT.Array i t
    AggPtrTypeStruct ts ->
      LT.Pointer $ LT.Struct ts

createAggData ::
  Context m =>
  AggPtrType -> -- the type of the base pointer
  [(C.Value, LT.LowType)] ->
  Lower m LC.Value
createAggData aggPtrType dts = do
  basePointer <- allocateBasePointer aggPtrType
  castedBasePointer <- cast basePointer $ toLowType aggPtrType
  storeElements castedBasePointer (toLowType aggPtrType) $ zip [0 ..] dts
  return basePointer

getSizeInfoOf :: AggPtrType -> (LT.LowType, Int)
getSizeInfoOf aggPtrType =
  case aggPtrType of
    AggPtrTypeArray len t ->
      (t, len)
    AggPtrTypeStruct ts ->
      (LT.Struct ts, 1)

allocateBasePointer :: Context m => AggPtrType -> Lower m LC.Value
allocateBasePointer aggPtrType = do
  let (elemType, len) = getSizeInfoOf aggPtrType
  sizePointer <- getElemPtr LC.Null (LT.Pointer elemType) [toInteger len]
  c <- uncast sizePointer (LT.Pointer elemType)
  reflect $ LC.Alloc c $ toLowType aggPtrType

storeElements ::
  Context m =>
  LC.Value -> -- base pointer
  LT.LowType -> -- the type of base pointer (like [n x u8]*, {i8*, i8*}*, etc.)
  [(Integer, (C.Value, LT.LowType))] -> -- [(the index of an element, the element to be stored)]
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

toFunPtrType :: [a] -> LT.LowType
toFunPtrType xs =
  LT.Pointer (LT.Function (map (const LT.voidPtr) xs) LT.voidPtr)

toFunPtrType' :: A.Arity -> LT.LowType
toFunPtrType' arity =
  LT.Pointer (LT.Function (LT.toVoidPtrSeq arity) LT.voidPtr)

newValueLocal :: Context m => T.Text -> m (Ident, LC.Value)
newValueLocal name = do
  x <- Gensym.newIdentFromText name
  return (x, LC.VarLocal x)

commConv :: Monad m => Ident -> LC.Comp -> LC.Comp -> m LC.Comp
commConv x lowComp cont2 =
  case lowComp of
    LC.Return d ->
      return $ LC.Let x (LC.Bitcast d LT.voidPtr LT.voidPtr) cont2 -- nop
    LC.Let y op cont1 -> do
      cont <- commConv x cont1 cont2
      return $ LC.Let y op cont
    LC.Cont op cont1 -> do
      cont <- commConv x cont1 cont2
      return $ LC.Cont op cont
    LC.Switch (d, t) defaultCase caseList -> do
      let (ds, es) = unzip caseList
      es' <- mapM (\e -> commConv x e cont2) es
      let caseList' = zip ds es'
      defaultCase' <- commConv x defaultCase cont2
      return $ LC.Switch (d, t) defaultCase' caseList'
    LC.TailCall d ds ->
      return $ LC.Let x (LC.Call d ds) cont2
    LC.Unreachable ->
      return LC.Unreachable
