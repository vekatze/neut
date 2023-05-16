module Scene.Lower
  ( lower,
  )
where

import Codec.Binary.UTF8.String
import Context.App
import Context.Env qualified as Env
import Context.Gensym qualified as Gensym
import Context.Locator qualified as Locator
import Context.Lower
import Context.StaticText
import Context.StaticText qualified as StaticText
import Control.Monad
import Control.Monad.Writer.Lazy
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.Arity qualified as A
import Entity.BaseName qualified as BN
import Entity.Comp qualified as C
import Entity.DeclarationName qualified as DN
import Entity.DefiniteDescription qualified as DD
import Entity.EnumCase qualified as EC
import Entity.Ident
import Entity.LowComp qualified as LC
import Entity.LowType qualified as LT
import Entity.Magic qualified as M
import Entity.PrimNumSize
import Entity.PrimNumSize.ToInt
import Entity.PrimOp
import Entity.PrimType qualified as PT
import Scene.Cancel

-- fixme: remove WriterT
type Lower = WriterT (Cont App) App

newtype Cont m = Cont (LC.Comp -> App LC.Comp)

instance Semigroup (Cont m) where
  Cont newCont <> Cont oldCont =
    Cont $ newCont <=< oldCont

instance Monoid (Cont m) where
  mempty =
    Cont return

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

lower ::
  ([C.CompDef], Maybe C.Comp) ->
  App (DN.DeclEnv, [LC.Def], Maybe LC.Comp, [StaticTextInfo])
lower (defList, mMainTerm) = do
  initialize $ map fst defList
  case mMainTerm of
    Just mainTerm -> do
      defList' <- forM defList $ \(name, (_, args, e)) -> do
        e' <- lowerComp e >>= liftIO . cancel
        return (name, (args, e'))
      mainTerm'' <- lowerComp mainTerm
      -- the result of "main" isn't i8*
      baseSize <- Env.getBaseSize'
      (result, resultVar) <- Gensym.newValueVarLocalWith "result"
      castResult <- runLower $ lowerValueLetCast resultVar (LT.PrimNum $ PT.Int $ IntSize baseSize)
      -- let result: i8* := (main-term) in {cast result to int}
      mainTerm''' <- Just <$> commConv result mainTerm'' castResult
      declEnv <- getDeclEnv
      staticTextList <- StaticText.getAll
      return (declEnv, defList', mainTerm''', staticTextList)
    Nothing -> do
      insDeclEnv (DN.In DD.imm) A.arityS4
      insDeclEnv (DN.In DD.cls) A.arityS4
      defList' <- forM defList $ \(name, (_, args, e)) -> do
        e' <- lowerComp e >>= liftIO . cancel
        return (name, (args, e'))
      declEnv <- getDeclEnv
      staticTextList <- StaticText.getAll
      return (declEnv, defList', Nothing, staticTextList)

lowerComp :: C.Comp -> App LC.Comp
lowerComp term =
  case term of
    C.Primitive theta ->
      runLower $ lowerCompPrimitive theta
    C.PiElimDownElim v ds -> do
      runLowerComp $ do
        v' <- lowerValue v
        ds' <- mapM lowerValue ds
        v'' <- cast v' LT.Pointer
        return $ LC.TailCall v'' ds'
    C.SigmaElim shouldDeallocate xs v e -> do
      let baseType = LT.Array (length xs) LT.voidPtr
      runLowerComp $ do
        basePointer <- lowerValue v
        ds <- loadElements basePointer baseType $ take (length xs) $ map (,LT.voidPtr) [0 ..]
        when shouldDeallocate $ do
          free basePointer (length xs)
        forM_ (zip xs ds) $ \(x, d) -> do
          extend $ return . LC.Let x (LC.Bitcast d LT.voidPtr LT.voidPtr)
        lift $ lowerComp e
    C.UpIntro d ->
      runLower $ lowerValue d
    C.UpElim _ x e1 e2 -> do
      e1' <- lowerComp e1
      e2' <- lowerComp e2
      commConv x e1' e2'
    C.EnumElim v defaultBranch branchList -> do
      (defaultCase, caseList) <- constructSwitch defaultBranch branchList
      runLowerComp $ do
        baseSize <- lift Env.getBaseSize'
        let t = LT.PrimNum $ PT.Int $ IntSize baseSize
        castedValue <- lowerValueLetCast v t
        (phi, phiVar) <- lift $ newValueLocal "phi"
        return $ LC.Switch (castedValue, t) defaultCase caseList (phi, LC.Return phiVar)
    C.Free x size cont -> do
      cont' <- lowerComp cont
      runLowerComp $ do
        x' <- lowerValue x
        return $ LC.Cont (LC.Free x' size) cont'
    C.Unreachable ->
      return LC.Unreachable

load :: LT.LowType -> LC.Value -> Lower LC.Value
load elemType pointer = do
  tmp <- reflect $ LC.Bitcast pointer LT.voidPtr LT.Pointer
  loaded <- reflect $ LC.Load tmp elemType
  uncast loaded elemType

store :: LT.LowType -> LC.Value -> LC.Value -> Lower ()
store lowType value pointer =
  reflectCont $ LC.Store lowType value pointer

loadElements ::
  LC.Value -> -- base pointer
  LT.LowType -> -- the type of base pointer
  [(Int, LT.LowType)] -> -- [(the index of an element, the variable to keep the loaded content)]
  Lower [LC.Value]
loadElements basePointer baseType values =
  case values of
    [] -> do
      return []
    (valueIndex, valueType) : xis -> do
      valuePointer <- getElemPtr basePointer baseType [0, toInteger valueIndex]
      uncastedValuePointer <- uncast valuePointer valueType
      x <- load valueType uncastedValuePointer
      xs <- loadElements basePointer baseType xis
      return $ x : xs

free :: LC.Value -> Int -> Lower ()
free pointer len = do
  reflectCont $ LC.Free pointer len

lowerCompPrimitive :: C.Primitive -> Lower LC.Value
lowerCompPrimitive codeOp =
  case codeOp of
    C.PrimOp op vs ->
      lowerCompPrimOp op vs
    C.Magic der -> do
      case der of
        M.Cast _ _ value -> do
          lowerValue value
        M.Store valueLowType pointer value -> do
          ptrVar <- lowerValueLetCast pointer LT.Pointer
          valVar <- lowerValueLetCast value valueLowType
          extend $ return . LC.Cont (LC.Store valueLowType valVar ptrVar)
          return LC.Null
        M.Load valueLowType pointer -> do
          castedPointer <- lowerValueLetCast pointer LT.Pointer
          result <- reflect $ LC.Load castedPointer valueLowType
          uncast result valueLowType
        M.Syscall i args -> do
          args' <- mapM lowerValue args
          reflect $ LC.Syscall i args'
        M.External name args -> do
          args' <- mapM lowerValue args
          lift $ insDeclEnv (DN.Ext name) $ A.fromInt $ length args'
          reflect $ LC.Call (LC.VarExternal name) args'
        M.Global lt name -> do
          uncast (LC.VarExternal name) lt

lowerCompPrimOp :: PrimOp -> [C.Value] -> Lower LC.Value
lowerCompPrimOp op vs = do
  let (domList, cod) = getTypeInfo op
  argVarList <- lowerValueLetCastPrimArgs $ zip vs domList
  result <- reflect $ LC.PrimOp op argVarList
  uncast result $ LT.PrimNum cod

lowerValueLetCastPrimArgs :: [(C.Value, PT.PrimType)] -> Lower [LC.Value]
lowerValueLetCastPrimArgs dts =
  case dts of
    [] ->
      return []
    ((d, t) : rest) -> do
      argVar <- lowerValueLetCast d $ LT.PrimNum t
      argVarList <- lowerValueLetCastPrimArgs rest
      return $ argVar : argVarList

cast :: LC.Value -> LT.LowType -> Lower LC.Value
cast v lowType = do
  (result, resultVar) <- lift $ newValueLocal "result"
  case lowType of
    LT.PrimNum (PT.Int _) -> do
      extend $ return . LC.Let result (LC.PointerToInt v LT.voidPtr lowType)
    LT.PrimNum (PT.Float size) -> do
      let floatType = LT.PrimNum $ PT.Float size
      let intType = LT.PrimNum $ PT.Int $ IntSize $ floatSizeToInt size
      (tmp, tmpVar) <- lift $ newValueLocal "tmp"
      extend $
        return
          . LC.Let tmp (LC.PointerToInt v LT.voidPtr intType)
          . LC.Let result (LC.Bitcast tmpVar intType floatType)
    _ -> do
      extend $ return . LC.Let result (LC.Bitcast v LT.voidPtr lowType)
  return resultVar

uncast :: LC.Value -> LT.LowType -> Lower LC.Value
uncast castedValue lowType = do
  (result, resultVar) <- lift $ newValueLocal "uncast"
  case lowType of
    LT.PrimNum (PT.Int _) ->
      extend $ return . LC.Let result (LC.IntToPointer castedValue lowType LT.voidPtr)
    LT.PrimNum (PT.Float i) -> do
      let floatType = LT.PrimNum $ PT.Float i
      let intType = LT.PrimNum $ PT.Int $ IntSize $ floatSizeToInt i
      (tmp, tmpVar) <- lift $ newValueLocal "tmp"
      extend $
        return
          . LC.Let tmp (LC.Bitcast castedValue floatType intType)
          . LC.Let result (LC.IntToPointer tmpVar intType LT.voidPtr)
    _ ->
      extend $ return . LC.Let result (LC.Bitcast castedValue lowType LT.voidPtr)
  return resultVar

lowerValueLetCast :: C.Value -> LT.LowType -> Lower LC.Value
lowerValueLetCast v lowType = do
  v' <- lowerValue v
  cast v' lowType

lowerValue :: C.Value -> Lower LC.Value
lowerValue v =
  case v of
    C.VarGlobal globalName arity -> do
      lowNameSet <- lift getDefinedNameSet
      unless (S.member globalName lowNameSet) $ do
        lift $ insDeclEnv (DN.In globalName) arity
      uncast (LC.VarGlobal globalName) LT.voidPtr
    C.VarLocal y ->
      return $ LC.VarLocal y
    C.VarStaticText text -> do
      let i8s = encode $ T.unpack text
      let len = length i8s
      i <- lift Gensym.newCount
      name <- lift $ Locator.attachCurrentLocator $ BN.textName i
      lift $ StaticText.insert name text len
      uncast (LC.VarGlobal name) LT.Pointer
    C.SigmaIntro ds -> do
      let arrayType = AggTypeArray (length ds) LT.voidPtr
      createAggData arrayType $ map (,LT.voidPtr) ds
    C.Int size l -> do
      uncast (LC.Int l) $ LT.PrimNum $ PT.Int size
    C.Float size f ->
      uncast (LC.Float size f) $ LT.PrimNum $ PT.Float size

getElemPtr :: LC.Value -> LT.LowType -> [Integer] -> Lower LC.Value
getElemPtr value valueType indexList = do
  let indexList' = map (\i -> (LC.Int i, LT.PrimNum $ PT.Int $ IntSize 32)) indexList
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
constructSwitch :: C.Comp -> [(EC.EnumCase, C.Comp)] -> App (LC.Comp, [(Integer, LC.Comp)])
constructSwitch defaultBranch switch =
  case switch of
    [] -> do
      defaultBranch' <- lowerComp defaultBranch
      return (defaultBranch', [])
    (EC.Int i, code) : rest -> do
      code' <- lowerComp code
      (defaultBranch', caseList) <- constructSwitch defaultBranch rest
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
  AggType -> -- the type of the base pointer
  [(C.Value, LT.LowType)] ->
  Lower LC.Value
createAggData aggType dts = do
  basePointer <- allocateBasePointer aggType
  storeElements basePointer (toLowType aggType) $ zip [0 ..] dts
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
      reflect $ LC.Alloc sizePointer len allocID

storeElements ::
  LC.Value -> -- base pointer
  LT.LowType -> -- the type of base pointer (like [n x u8]*, {i8*, i8*}*, etc.)
  [(Integer, (C.Value, LT.LowType))] -> -- [(the index of an element, the element to be stored)]
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

newValueLocal :: T.Text -> App (Ident, LC.Value)
newValueLocal name = do
  x <- Gensym.newIdentFromText name
  return (x, LC.VarLocal x)

commConv :: Ident -> LC.Comp -> LC.Comp -> App LC.Comp
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
    LC.Switch (d, t) defaultCase caseList (phiVar, cont) -> do
      cont' <- commConv x cont cont2
      return $ LC.Switch (d, t) defaultCase caseList (phiVar, cont')
    LC.TailCall d ds ->
      return $ LC.Let x (LC.Call d ds) cont2
    LC.Unreachable ->
      return LC.Unreachable
