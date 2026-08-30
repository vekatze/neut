module Kernel.Lower.CoercionCancel (coercionCancel) where

import Data.Bits (shiftL, (.&.))
import Data.IntMap.Strict qualified as IntMap
import Data.List (unsnoc)
import Language.Common.DataSize qualified as DS
import Language.Common.Ident
import Language.Common.Ident.Reify
import Language.Common.LowType qualified as LT
import Language.Common.PrimNumSize.ToInt (intSizeToInt)
import Language.Common.PrimOp
import Language.Common.PrimOp.ConvOp qualified as ConvOp
import Language.Common.PrimType qualified as PT
import Language.Common.SlotSize
import Language.LowComp.LowComp qualified as LC

data Env = Env
  { pointerFillsSlot :: Bool,
    historyMap :: IntMap.IntMap CoercionHistory
  }

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
  | StepIntConv ConvOp.ConvOp PT.PrimType PT.PrimType

coercionCancel :: DS.DataSize -> LC.Comp -> LC.Comp
coercionCancel baseSize =
  rewriteComp
    Env
      { pointerFillsSlot = DS.reify baseSize == slotBitSize,
        historyMap = IntMap.empty
      }

rewriteComp :: Env -> LC.Comp -> LC.Comp
rewriteComp env lowComp =
  case lowComp of
    LC.Return value ->
      LC.Return (rewriteValue env value)
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
    LC.TailCall mustTail codType value valueList ->
      LC.TailCall mustTail codType (rewriteValue env value) (map (rewriteTypedValue env) valueList)
    LC.Unreachable ->
      LC.Unreachable
    LC.Phi valueList -> do
      let valueList' = map (rewriteValue env) valueList
      LC.Phi valueList'

rewriteOp :: Env -> LC.Op -> LC.Op
rewriteOp env op =
  case op of
    LC.Call isPure codType value valueList ->
      LC.Call isPure codType (rewriteValue env value) (map (rewriteTypedValue env) valueList)
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
      case IntMap.lookup (toInt x) (historyMap env) of
        Just history ->
          historyRepresentative $ normalizeHistory (pointerFillsSlot env) history
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
    LC.PrimOp (PrimConvOp convOp domType codType) [value]
      | convOp `elem` [ConvOp.Zext, ConvOp.Trunc] -> do
          let base = getHistory env value
          insertHistory env x $ appendCoercionStep base (HistoryStep (StepIntConv convOp domType codType) (LC.VarLocal x))
    _ ->
      env

appendCoercionStep :: CoercionHistory -> HistoryStep -> CoercionHistory
appendCoercionStep base step = do
  base {historySteps = historySteps base ++ [step]}

normalizeHistory :: Bool -> CoercionHistory -> CoercionHistory
normalizeHistory pointerFits CoercionHistory {historyBaseValue, historySteps} = do
  let normalizedSteps = normalizeCoercionSteps pointerFits historySteps
  case normalizedSteps of
    HistoryStep {stepOp = StepBitcast from to} : rest
      | from == to ->
          normalizeHistory pointerFits $ CoercionHistory historyBaseValue rest
    HistoryStep {stepOp = StepIntToPointer _} : rest
      | pointerFits,
        LC.Int i <- historyBaseValue ->
          normalizeHistory pointerFits $ CoercionHistory (LC.Address i) rest
    HistoryStep {stepOp = StepPointerToInt _} : rest
      | LC.Address a <- historyBaseValue ->
          normalizeHistory pointerFits $ CoercionHistory (LC.Int a) rest
    HistoryStep {stepOp = StepIntConv _ (PT.Int domSize) (PT.Int codSize)} : rest
      | LC.Int i <- historyBaseValue -> do
          let width = min (intSizeToInt domSize) (intSizeToInt codSize)
          normalizeHistory pointerFits $ CoercionHistory (LC.Int (i .&. ((1 `shiftL` width) - 1))) rest
    _ ->
      CoercionHistory historyBaseValue normalizedSteps

normalizeCoercionSteps :: Bool -> [HistoryStep] -> [HistoryStep]
normalizeCoercionSteps pointerFits steps =
  case steps of
    [] ->
      []
    s : rest ->
      case normalizeStep s of
        Nothing ->
          normalizeCoercionSteps pointerFits rest
        Just step ->
          case normalizeCoercionSteps pointerFits rest of
            [] ->
              [step]
            next : rest' ->
              case mergeCoercionSteps pointerFits (stepOp step) (stepOp next) of
                MergeTo merged ->
                  normalizeCoercionSteps pointerFits (HistoryStep merged (stepValue next) : rest')
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

mergeCoercionSteps :: Bool -> CoercionStep -> CoercionStep -> StepMerge
mergeCoercionSteps pointerFits prev next =
  case (prev, next) of
    (StepBitcast from mid1, StepBitcast mid2 to)
      | mid1 == mid2 ->
          MergeTo $ StepBitcast from to
    (StepIntToPointer lowType1, StepPointerToInt lowType2)
      | pointerFits, lowType1 == lowType2 ->
          CancelPair
    (StepPointerToInt lowType1, StepIntToPointer lowType2)
      | lowType1 == lowType2 ->
          CancelPair
    (StepIntConv ConvOp.Zext from mid1, StepIntConv ConvOp.Trunc mid2 to)
      | mid1 == mid2, from == to ->
          CancelPair
    _ ->
      NoMerge

insertHistory :: Env -> Ident -> CoercionHistory -> Env
insertHistory env x history =
  env {historyMap = IntMap.insert (toInt x) history (historyMap env)}

getHistory :: Env -> LC.Value -> CoercionHistory
getHistory env value =
  case value of
    LC.VarLocal x ->
      case IntMap.lookup (toInt x) (historyMap env) of
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
