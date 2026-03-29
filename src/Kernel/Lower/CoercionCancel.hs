module Kernel.Lower.CoercionCancel (coercionCancel) where

import Data.IntMap.Strict qualified as IntMap
import Data.List (transpose)
import Data.Maybe (catMaybes, isNothing, mapMaybe)
import Language.Common.Ident
import Language.Common.Ident.Reify
import Language.Common.LowType qualified as LT
import Language.LowComp.LowComp qualified as LC

type Env =
  IntMap.IntMap Binding

data Binding = Binding
  { replacement :: Maybe LC.Value,
    coercionOrigin :: Maybe CoercionOrigin
  }

data CoercionOrigin
  = FromIntToPointer LC.Value LT.LowType
  | FromPointerToInt LC.Value LT.LowType

data TerminalInfo
  = TerminalOther
  | TerminalUnreachable
  | TerminalPhi Env [LC.Value]

data PhiInfo = PhiInfo
  { phiReplacement :: Maybe LC.Value,
    phiOrigin :: Maybe CoercionOrigin
  }

emptyBinding :: Binding
emptyBinding =
  Binding
    { replacement = Nothing,
      coercionOrigin = Nothing
    }

coercionCancel :: LC.Comp -> LC.Comp
coercionCancel lowComp =
  fst $ rewriteComp IntMap.empty lowComp

rewriteComp :: Env -> LC.Comp -> (LC.Comp, TerminalInfo)
rewriteComp env lowComp =
  case lowComp of
    LC.Return value ->
      (LC.Return (rewriteValue env value), TerminalOther)
    LC.ReturnVoid ->
      (LC.ReturnVoid, TerminalOther)
    LC.Let x op cont -> do
      let (mOp, env') = rewriteLet env x op
      let (cont', terminalInfo) = rewriteComp env' cont
      case mOp of
        Just op' ->
          (LC.Let x op' cont', terminalInfo)
        Nothing ->
          (cont', terminalInfo)
    LC.Cont op cont -> do
      let op' = rewriteOp env op
      let (cont', terminalInfo) = rewriteComp env cont
      (LC.Cont op' cont', terminalInfo)
    LC.Switch value lowType defaultBranch branchList phiTargets cont -> do
      let value' = rewriteValue env value
      let (defaultBranch', defaultTerminalInfo) = rewriteComp env defaultBranch
      let (caseTags, caseBranches) = unzip branchList
      let rewrittenBranchList = map (rewriteComp env) caseBranches
      let caseBranches' = map fst rewrittenBranchList
      let terminalInfoList = defaultTerminalInfo : map snd rewrittenBranchList
      let mergedPhiEnv = mergePhiEnv phiTargets terminalInfoList
      let env' = IntMap.union mergedPhiEnv env
      let (cont', terminalInfo) = rewriteComp env' cont
      (LC.Switch value' lowType defaultBranch' (zip caseTags caseBranches') phiTargets cont', terminalInfo)
    LC.TailCall codType value valueList ->
      (LC.TailCall codType (rewriteValue env value) (map (rewriteTypedValue env) valueList), TerminalOther)
    LC.Unreachable ->
      (LC.Unreachable, TerminalUnreachable)
    LC.Phi valueList -> do
      let valueList' = map (rewriteValue env) valueList
      (LC.Phi valueList', TerminalPhi env valueList')

rewriteLet :: Env -> Ident -> LC.Op -> (Maybe LC.Op, Env)
rewriteLet env x op =
  case rewriteOp env op of
    LC.Bitcast value from to
      | from == to ->
          (Nothing, insertAlias env x value)
    LC.IntToPointer value lowType ->
      case cancelIntToPointer env value lowType of
        Just value' ->
          (Nothing, insertAlias env x value')
        Nothing ->
          case value of
            LC.Int i ->
              (Nothing, insertAlias env x (LC.Address i))
            _ ->
              (Just (LC.IntToPointer value lowType), insertOrigin env x (FromIntToPointer value lowType))
    LC.PointerToInt value lowType ->
      case cancelPointerToInt env value lowType of
        Just value' ->
          (Nothing, insertAlias env x value')
        Nothing ->
          case value of
            LC.Address a ->
              (Nothing, insertAlias env x (LC.Int a))
            _ ->
              (Just (LC.PointerToInt value lowType), insertOrigin env x (FromPointerToInt value lowType))
    op' ->
      (Just op', insertOpaque env x)

cancelIntToPointer :: Env -> LC.Value -> LT.LowType -> Maybe LC.Value
cancelIntToPointer env value lowType =
  case getOrigin env value of
    Just (FromPointerToInt value' lowType')
      | lowType == lowType' ->
          Just $ rewriteValue env value'
    _ ->
      Nothing

cancelPointerToInt :: Env -> LC.Value -> LT.LowType -> Maybe LC.Value
cancelPointerToInt env value lowType =
  case getOrigin env value of
    Just (FromIntToPointer value' lowType')
      | lowType == lowType' ->
          Just $ rewriteValue env value'
    _ ->
      Nothing

rewriteTypedValue :: Env -> (LT.LowType, LC.Value) -> (LT.LowType, LC.Value)
rewriteTypedValue env (lowType, value) =
  (lowType, rewriteValue env value)

rewriteIndexedValue :: Env -> (LC.Value, LT.LowType) -> (LC.Value, LT.LowType)
rewriteIndexedValue env (value, lowType) =
  (rewriteValue env value, lowType)

rewriteOp :: Env -> LC.Op -> LC.Op
rewriteOp env op =
  case op of
    LC.Call codType value valueList ->
      LC.Call codType (rewriteValue env value) (map (rewriteTypedValue env) valueList)
    LC.MagicCall codType value valueList ->
      LC.MagicCall codType (rewriteValue env value) (map (rewriteTypedValue env) valueList)
    LC.GetElementPtr (value, lowType) valueList ->
      LC.GetElementPtr (rewriteValue env value, lowType) (map (rewriteIndexedValue env) valueList)
    LC.Bitcast value from to ->
      LC.Bitcast (rewriteValue env value) from to
    LC.IntToPointer value lowType ->
      LC.IntToPointer (rewriteValue env value) lowType
    LC.PointerToInt value lowType ->
      LC.PointerToInt (rewriteValue env value) lowType
    LC.Load value lowType ->
      LC.Load (rewriteValue env value) lowType
    LC.Store lowType value1 value2 ->
      LC.Store lowType (rewriteValue env value1) (rewriteValue env value2)
    LC.StackAlloc stackAllocInfo ->
      LC.StackAlloc $
        stackAllocInfo
          { LC.stackSize =
              case LC.stackSize stackAllocInfo of
                Left knownSize ->
                  Left knownSize
                Right runtimeSize ->
                  Right (rewriteValue env runtimeSize)
          }
    LC.StackLifetimeStart stackSlotID ->
      LC.StackLifetimeStart stackSlotID
    LC.StackLifetimeEnd stackSlotID ->
      LC.StackLifetimeEnd stackSlotID
    LC.Calloc value1 value2 ->
      LC.Calloc (rewriteValue env value1) (rewriteValue env value2)
    LC.Alloc size allocID ->
      LC.Alloc
        ( case size of
            Left knownSize ->
              Left knownSize
            Right runtimeSize ->
              Right (rewriteValue env runtimeSize)
        )
        allocID
    LC.Realloc value1 value2 ->
      LC.Realloc (rewriteValue env value1) (rewriteValue env value2)
    LC.Free value size freeID ->
      LC.Free (rewriteValue env value) size freeID
    LC.PrimOp primOp valueList ->
      LC.PrimOp primOp (map (rewriteValue env) valueList)

rewriteValue :: Env -> LC.Value -> LC.Value
rewriteValue env value =
  case value of
    LC.VarLocal x ->
      case IntMap.lookup (toInt x) env >>= replacement of
        Just value' ->
          rewriteValue env value'
        Nothing ->
          LC.VarLocal x
    _ ->
      value

insertAlias :: Env -> Ident -> LC.Value -> Env
insertAlias env x value =
  IntMap.insert
    (toInt x)
    ( Binding
        { replacement = Just value',
          coercionOrigin = getOrigin env value'
        }
    )
    env
  where
    value' = rewriteValue env value

insertOrigin :: Env -> Ident -> CoercionOrigin -> Env
insertOrigin env x origin =
  IntMap.insert
    (toInt x)
    ( Binding
        { replacement = Nothing,
          coercionOrigin = Just origin
        }
    )
    env

insertOpaque :: Env -> Ident -> Env
insertOpaque env x =
  IntMap.insert (toInt x) emptyBinding env

getOrigin :: Env -> LC.Value -> Maybe CoercionOrigin
getOrigin env value =
  case rewriteValue env value of
    LC.VarLocal x ->
      IntMap.lookup (toInt x) env >>= coercionOrigin
    _ ->
      Nothing

mergePhiEnv :: [Ident] -> [TerminalInfo] -> Env
mergePhiEnv phiTargets terminalInfoList =
  case reachablePhiInfoList of
    [] ->
      IntMap.empty
    _ ->
      if all ((== length phiTargets) . length) reachablePhiInfoList
        then
          IntMap.fromList $
            catMaybes $
              zipWith toPhiBinding phiTargets (map mergePhiInfo (transpose reachablePhiInfoList))
        else
          IntMap.empty
  where
    reachablePhiInfoList =
      flip mapMaybe terminalInfoList $ \terminalInfo ->
        case terminalInfo of
          TerminalPhi env valueList ->
            Just $ map (toPhiInfo env) valueList
          TerminalUnreachable ->
            Nothing
          TerminalOther ->
            Nothing

toPhiInfo :: Env -> LC.Value -> PhiInfo
toPhiInfo env value =
  PhiInfo
    { phiReplacement = Just value',
      phiOrigin = getOrigin env value'
    }
  where
    value' = rewriteValue env value

mergePhiInfo :: [PhiInfo] -> PhiInfo
mergePhiInfo phiInfoList =
  PhiInfo
    { phiReplacement = mergeBy sameValue (map phiReplacement phiInfoList),
      phiOrigin = mergeBy sameOrigin (map phiOrigin phiInfoList)
    }

toPhiBinding :: Ident -> PhiInfo -> Maybe (Int, Binding)
toPhiBinding x phiInfo
  | isNothing (phiReplacement phiInfo) && isNothing (phiOrigin phiInfo) =
      Nothing
  | otherwise =
      Just
        ( toInt x,
          Binding
            { replacement = phiReplacement phiInfo,
              coercionOrigin = phiOrigin phiInfo
            }
        )

mergeBy :: (a -> a -> Bool) -> [Maybe a] -> Maybe a
mergeBy eq xs =
  case sequence xs of
    Just (y : ys)
      | all (eq y) ys ->
          Just y
    _ ->
      Nothing

sameOrigin :: CoercionOrigin -> CoercionOrigin -> Bool
sameOrigin origin1 origin2 =
  case (origin1, origin2) of
    (FromIntToPointer value1 lowType1, FromIntToPointer value2 lowType2) ->
      sameValue value1 value2 && lowType1 == lowType2
    (FromPointerToInt value1 lowType1, FromPointerToInt value2 lowType2) ->
      sameValue value1 value2 && lowType1 == lowType2
    _ ->
      False

sameValue :: LC.Value -> LC.Value -> Bool
sameValue value1 value2 =
  case (value1, value2) of
    (LC.VarLocal x, LC.VarLocal y) ->
      x == y
    (LC.VarGlobal x, LC.VarGlobal y) ->
      x == y
    (LC.VarExternal x, LC.VarExternal y) ->
      x == y
    (LC.VarTextName x, LC.VarTextName y) ->
      x == y
    (LC.Int x, LC.Int y) ->
      x == y
    (LC.Float size1 x, LC.Float size2 y) ->
      size1 == size2 && x == y
    (LC.Address x, LC.Address y) ->
      x == y
    (LC.Null, LC.Null) ->
      True
    _ ->
      False
