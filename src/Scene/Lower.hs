{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list comprehension" #-}

module Scene.Lower
  ( lower,
    lowerEntryPoint,
  )
where

import Codec.Binary.UTF8.String
import Context.App
import Context.Env qualified as Env
import Context.Gensym qualified as Gensym
import Context.Locator qualified as Locator
import Control.Monad
import Control.Monad.Writer.Lazy
import Data.ByteString.Builder
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.IntMap qualified as IntMap
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.ArgNum qualified as AN
import Entity.BaseLowType qualified as BLT
import Entity.BaseName qualified as BN
import Entity.Comp qualified as C
import Entity.Const
import Entity.DeclarationName qualified as DN
import Entity.DefiniteDescription qualified as DD
import Entity.EnumCase qualified as EC
import Entity.ExternalName qualified as EN
import Entity.Foreign qualified as F
import Entity.ForeignCodType qualified as F
import Entity.ForeignCodType qualified as FCT
import Entity.Ident
import Entity.LowComp qualified as LC
import Entity.LowType qualified as LT
import Entity.LowType.FromBaseLowType qualified as LT
import Entity.Magic qualified as M
import Entity.PrimNumSize
import Entity.PrimNumSize.ToInt
import Entity.PrimOp
import Entity.PrimType qualified as PT
import Entity.Target
import Scene.Cancel
import Scene.Comp.Reduce qualified as C
import Scene.Comp.Subst qualified as C

type Lower = WriterT Cont App

newtype Cont = Cont (LC.Comp -> App LC.Comp)

instance Semigroup Cont where
  Cont newCont <> Cont oldCont =
    Cont $ newCont <=< oldCont

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
  arch <- Env.getArch Nothing
  forM_ (F.defaultForeignList arch) $ \(F.Foreign _ name domList cod) -> do
    insDeclEnv' h (DN.Ext name) domList cod
  registerInternalNames h stmtList
  return h

lower ::
  [C.CompStmt] ->
  App LC.LowCode
lower stmtList = do
  h <- new stmtList
  insDeclEnv h (DN.In DD.imm) AN.argNumS4
  insDeclEnv h (DN.In DD.cls) AN.argNumS4
  stmtList' <- catMaybes <$> mapM (lowerStmt h) stmtList
  LC.LowCodeNormal <$> summarize h stmtList'

lowerEntryPoint :: MainTarget -> [C.CompStmt] -> App LC.LowCode
lowerEntryPoint target stmtList = do
  h <- new stmtList
  mainDD <- Locator.getMainDefiniteDescriptionByTarget target
  insDeclEnv h (DN.In mainDD) AN.zero
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
          insDeclEnv' h (DN.Ext name) domList cod

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
      runLowerComp $ do
        v' <- lowerValue h v
        ds' <- mapM (lowerValue h) ds
        v'' <- cast v' LT.Pointer
        return $ LC.TailCall LT.Pointer v'' (map (LT.Pointer,) ds')
    C.SigmaElim shouldDeallocate xs v e -> do
      let numOfElems = length xs
      let baseType = LT.Array numOfElems LT.Pointer
      runLowerComp $ do
        basePointer <- lowerValue h v
        valuePointerList <- getElemPtrList basePointer baseType numOfElems
        ds <- loadElements basePointer $ map (,LT.Pointer) valuePointerList
        when shouldDeallocate $ do
          free basePointer (length xs)
        forM_ (zip xs ds) $ \(x, d) -> do
          extend $ return . LC.Let x (LC.Bitcast d LT.Pointer LT.Pointer)
        lift $ lowerComp h e
    C.UpIntro d ->
      runLower $ lowerValue h d
    C.UpElim _ x e1 e2 -> do
      e1' <- lowerComp h e1
      e2' <- lowerComp h e2
      return $ commConv x e1' e2'
    C.EnumElim fvInfo v defaultBranch branchList -> do
      let sub = IntMap.fromList fvInfo
      defaultBranch' <- C.subst sub defaultBranch >>= C.reduce
      let (keys, clauses) = unzip branchList
      clauses' <- mapM (C.subst sub >=> C.reduce) clauses
      let branchList' = zip keys clauses'
      case (defaultBranch', clauses') of
        (C.Unreachable, [clause]) ->
          lowerComp h clause
        (_, []) ->
          lowerComp h defaultBranch'
        _ -> do
          (defaultCase, caseList) <- constructSwitch h defaultBranch' branchList'
          runLowerComp $ do
            baseSize <- lift Env.getBaseSize'
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

load :: LT.LowType -> LC.Value -> Lower LC.Value
load elemType pointer = do
  tmp <- reflect $ LC.Bitcast pointer LT.Pointer LT.Pointer
  loaded <- reflect $ LC.Load tmp elemType
  uncast loaded elemType

store :: LT.LowType -> LC.Value -> LC.Value -> Lower ()
store lowType value pointer =
  reflectCont $ LC.Store lowType value pointer

loadElements ::
  LC.Value -> -- base pointer
  [(LC.Value, LT.LowType)] -> -- [(the index of an element, the variable to keep the loaded content)]
  Lower [LC.Value]
loadElements basePointer values =
  case values of
    [] -> do
      return []
    (valuePointer, valueType) : xis -> do
      uncastedValuePointer <- uncast valuePointer valueType
      x <- load valueType uncastedValuePointer
      xs <- loadElements basePointer xis
      return $ x : xs

free :: LC.Value -> Int -> Lower ()
free pointer len = do
  freeID <- lift Gensym.newCount
  reflectCont $ LC.Free pointer len freeID

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
          baseSize <- lift Env.getBaseSize'
          let indexType = LT.PrimNum $ PT.Int $ IntSize baseSize
          castedSize <- lowerValueLetCast h size indexType
          result <- reflect $ LC.StackAlloc t' indexType castedSize
          uncast result LT.Pointer
        M.External domList cod name args varArgAndTypeList -> do
          alreadyRegistered <- lift $ member h (DN.Ext name)
          unless alreadyRegistered $ do
            lift $ insDeclEnv' h (DN.Ext name) domList cod
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
      lowNameSet <- lift $ getDefinedNameSet h
      unless (S.member globalName lowNameSet) $ do
        -- lift $ Decl.insDeclEnv (DN.In globalName) argNum
        lift $ insDeclEnv h (DN.In globalName) argNum
      uncast (LC.VarGlobal globalName) LT.Pointer
    C.VarLocal y ->
      return $ LC.VarLocal y
    C.VarStaticText text -> do
      let i8s = encode $ T.unpack text
      let len = length i8s
      i <- lift Gensym.newCount
      name <- lift $ Locator.attachCurrentLocator $ BN.textName i
      let encodedText = foldMap (\w -> "\\" <> word8HexFixed w) i8s
      -- lift $ StaticText.insert name encodedText len
      lift $ insertStaticText h name encodedText len
      uncast (LC.VarGlobal name) LT.Pointer
    C.SigmaIntro ds -> do
      let arrayType = AggTypeArray (length ds) LT.Pointer
      createAggData h arrayType $ map (,LT.Pointer) ds
    C.Int size l -> do
      uncast (LC.Int l) $ LT.PrimNum $ PT.Int size
    C.Float size f ->
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
      baseSize <- lift Env.getBaseSize'
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
      LC.Let x (LC.Bitcast d LT.Pointer LT.Pointer) cont2 -- nop
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

insDeclEnv :: Handle -> DN.DeclarationName -> AN.ArgNum -> App ()
insDeclEnv h k argNum = do
  liftIO $ modifyIORef' (declEnv h) $ Map.insert k (BLT.toVoidPtrSeq argNum, FCT.Void)

insDeclEnv' :: Handle -> DN.DeclarationName -> [BLT.BaseLowType] -> FCT.ForeignCodType BLT.BaseLowType -> App ()
insDeclEnv' h k domList cod = do
  liftIO $ modifyIORef' (declEnv h) $ Map.insert k (domList, cod)

member :: Handle -> DN.DeclarationName -> App Bool
member h k = do
  denv <- liftIO $ readIORef (declEnv h)
  return $ Map.member k denv

insertStaticText :: Handle -> DD.DefiniteDescription -> Builder -> Int -> App ()
insertStaticText h name text len =
  liftIO $ modifyIORef' (staticTextList h) $ (:) (name, (text, len))

getDefinedNameSet :: Handle -> App (S.Set DD.DefiniteDescription)
getDefinedNameSet h = do
  liftIO $ readIORef (definedNameSet h)
