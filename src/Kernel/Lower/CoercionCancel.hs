module Kernel.Lower.CoercionCancel (coercionCancel) where

import Data.IntMap.Strict qualified as IntMap
import Data.List (unsnoc)
import Language.Common.Ident
import Language.Common.Ident.Reify
import Language.Common.LowType qualified as LT
import Language.LowComp.LowComp qualified as LC

type Env =
  IntMap.IntMap CoercionHistory

data CoercionHistory = CoercionHistory
  { historyBaseValue :: LC.Value,
    historySteps :: [HistoryStep]
  }

data HistoryStep = HistoryStep
  { stepOp :: CoercionStep,
    stepValue :: LC.Value
  }

data CoercionStep
  = StepBitcast LT.LowType LT.LowType
  | StepIntToPointer LT.LowType
  | StepPointerToInt LT.LowType

coercionCancel :: LC.Comp -> LC.Comp
coercionCancel =
  rewriteComp IntMap.empty

rewriteComp :: Env -> LC.Comp -> LC.Comp
rewriteComp env lowComp =
  case lowComp of
    LC.Return value ->
      LC.Return (rewriteValue env value)
    LC.ReturnVoid ->
      LC.ReturnVoid
    LC.Let x op cont -> do
      let op' = rewriteOp env op
      let cont' = rewriteComp (insertCoercionStep env x op') cont
      LC.Let x op' cont'
    LC.Cont op cont -> do
      let op' = rewriteOp env op
      let cont' = rewriteComp env cont
      LC.Cont op' cont'
    LC.Switch value lowType defaultBranch branchList phiTargets cont -> do
      let value' = rewriteValue env value
      let defaultBranch' = rewriteComp env defaultBranch
      let (caseTags, caseBranches) = unzip branchList
      let caseBranches' = map (rewriteComp env) caseBranches
      let cont' = rewriteComp env cont
      LC.Switch value' lowType defaultBranch' (zip caseTags caseBranches') phiTargets cont'
    LC.TailCall codType value valueList ->
      LC.TailCall codType (rewriteValue env value) (map (rewriteTypedValue env) valueList)
    LC.Unreachable ->
      LC.Unreachable
    LC.Phi valueList -> do
      let valueList' = map (rewriteValue env) valueList
      LC.Phi valueList'

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
      LC.StackAlloc $ stackAllocInfo {LC.stackSize = fmap (rewriteValue env) (LC.stackSize stackAllocInfo)}
    LC.StackLifetimeStart stackSlotID ->
      LC.StackLifetimeStart stackSlotID
    LC.StackLifetimeEnd stackSlotID ->
      LC.StackLifetimeEnd stackSlotID
    LC.Calloc value1 value2 ->
      LC.Calloc (rewriteValue env value1) (rewriteValue env value2)
    LC.Alloc size allocID ->
      LC.Alloc (fmap (rewriteValue env) size) allocID
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
      case IntMap.lookup (toInt x) env of
        Just history ->
          historyRepresentative $ normalizeHistory history
        Nothing ->
          LC.VarLocal x
    _ ->
      value

rewriteTypedValue :: Env -> (LT.LowType, LC.Value) -> (LT.LowType, LC.Value)
rewriteTypedValue env (lowType, value) =
  (lowType, rewriteValue env value)

rewriteIndexedValue :: Env -> (LC.Value, LT.LowType) -> (LC.Value, LT.LowType)
rewriteIndexedValue env (value, lowType) =
  (rewriteValue env value, lowType)

insertCoercionStep :: Env -> Ident -> LC.Op -> Env
insertCoercionStep env x op = do
  case op of
    LC.Bitcast value from to -> do
      let base = getHistory env value
      insertHistory env x $ appendCoercionStep base (HistoryStep (StepBitcast from to) (LC.VarLocal x))
    LC.IntToPointer value lowType -> do
      let base = getHistory env value
      insertHistory env x $ appendCoercionStep base (HistoryStep (StepIntToPointer lowType) (LC.VarLocal x))
    LC.PointerToInt value lowType -> do
      let base = getHistory env value
      insertHistory env x $ appendCoercionStep base (HistoryStep (StepPointerToInt lowType) (LC.VarLocal x))
    _ ->
      env

appendCoercionStep :: CoercionHistory -> HistoryStep -> CoercionHistory
appendCoercionStep base step = do
  base {historySteps = historySteps base ++ [step]}

normalizeHistory :: CoercionHistory -> CoercionHistory
normalizeHistory CoercionHistory {historyBaseValue, historySteps} = do
  let normalizedSteps = normalizeCoercionSteps historySteps
  case normalizedSteps of
    HistoryStep {stepOp = StepBitcast from to} : rest
      | from == to ->
          normalizeHistory $ CoercionHistory historyBaseValue rest
    HistoryStep {stepOp = StepIntToPointer _} : rest
      | LC.Int i <- historyBaseValue ->
          normalizeHistory $ CoercionHistory (LC.Address i) rest
    HistoryStep {stepOp = StepPointerToInt _} : rest
      | LC.Address a <- historyBaseValue ->
          normalizeHistory $ CoercionHistory (LC.Int a) rest
    _ ->
      CoercionHistory historyBaseValue normalizedSteps

normalizeCoercionSteps :: [HistoryStep] -> [HistoryStep]
normalizeCoercionSteps steps =
  case steps of
    [] ->
      []
    s : rest ->
      case normalizeStep s of
        Nothing ->
          normalizeCoercionSteps rest
        Just step ->
          case normalizeCoercionSteps rest of
            [] ->
              [step]
            next : rest' ->
              case mergeCoercionSteps (stepOp step) (stepOp next) of
                MergeTo merged ->
                  normalizeCoercionSteps (HistoryStep merged (stepValue next) : rest')
                CancelPair ->
                  rest'
                NoMerge ->
                  step : next : rest'

normalizeStep :: HistoryStep -> Maybe HistoryStep
normalizeStep step =
  case stepOp step of
    StepBitcast from to
      | from == to ->
          Nothing
    _ ->
      Just step

data StepMerge
  = MergeTo CoercionStep
  | CancelPair
  | NoMerge

mergeCoercionSteps :: CoercionStep -> CoercionStep -> StepMerge
mergeCoercionSteps prev next =
  case (prev, next) of
    (StepBitcast from mid1, StepBitcast mid2 to)
      | mid1 == mid2 ->
          MergeTo $ StepBitcast from to
    (StepIntToPointer lowType1, StepPointerToInt lowType2)
      | lowType1 == lowType2 ->
          CancelPair
    (StepPointerToInt lowType1, StepIntToPointer lowType2)
      | lowType1 == lowType2 ->
          CancelPair
    _ ->
      NoMerge

insertHistory :: Env -> Ident -> CoercionHistory -> Env
insertHistory env x history =
  IntMap.insert (toInt x) history env

getHistory :: Env -> LC.Value -> CoercionHistory
getHistory env value =
  case value of
    LC.VarLocal x ->
      case IntMap.lookup (toInt x) env of
        Just history ->
          history
        Nothing ->
          bareHistory value
    _ ->
      bareHistory value

bareHistory :: LC.Value -> CoercionHistory
bareHistory value =
  CoercionHistory
    { historyBaseValue = value,
      historySteps = []
    }

historyRepresentative :: CoercionHistory -> LC.Value
historyRepresentative history =
  case unsnoc (historySteps history) of
    Just (_, HistoryStep {stepValue}) ->
      stepValue
    Nothing ->
      historyBaseValue history
