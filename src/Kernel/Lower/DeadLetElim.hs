module Kernel.Lower.DeadLetElim (deadLetElim) where

import Data.IntSet qualified as IntSet
import Language.Common.Ident
import Language.Common.Ident.Reify
import Language.LowComp.LowComp qualified as LC

data Demand
  = DemandReturn
  | DemandPhi [Bool]

deadLetElim :: LC.Comp -> LC.Comp
deadLetElim lowComp =
  snd $ rewriteComp DemandReturn lowComp

rewriteComp :: Demand -> LC.Comp -> (IntSet.IntSet, LC.Comp)
rewriteComp demand lowComp =
  case lowComp of
    LC.Return value ->
      (usedValueSet value, LC.Return value)
    LC.ReturnVoid ->
      (IntSet.empty, LC.ReturnVoid)
    LC.Let x op cont -> do
      let (liveCont, cont') = rewriteComp demand cont
      let liveAfterDef = IntSet.delete (toInt x) liveCont
      if IntSet.member (toInt x) liveCont || not (isDiscardable op)
        then do
          let liveSet = IntSet.union liveAfterDef (usedOpSet op)
          (liveSet, LC.Let x op cont')
        else do
          (liveAfterDef, cont')
    LC.Cont op cont -> do
      let (liveCont, cont') = rewriteComp demand cont
      let liveSet = IntSet.union (usedOpSet op) liveCont
      (liveSet, LC.Cont op cont')
    LC.Switch value lowType defaultBranch branchList phiTargets cont -> do
      let (liveCont, cont') = rewriteComp demand cont
      let liveMask = map (\phiTarget -> IntSet.member (toInt phiTarget) liveCont) phiTargets
      let phiTargets' = filterByMask liveMask phiTargets
      let branchDemand = DemandPhi liveMask
      let (liveDefault, defaultBranch') = rewriteComp branchDemand defaultBranch
      let (caseTags, caseBranches) = unzip branchList
      let rewrittenBranchList = map (rewriteComp branchDemand) caseBranches
      let liveCaseList = map fst rewrittenBranchList
      let caseBranches' = map snd rewrittenBranchList
      let liveBeforeSwitch = deleteMany liveCont phiTargets
      let liveSet =
            IntSet.unions $
              usedValueSet value : liveBeforeSwitch : liveDefault : liveCaseList
      (liveSet, LC.Switch value lowType defaultBranch' (zip caseTags caseBranches') phiTargets' cont')
    LC.TailCall _ value typedValueList ->
      (IntSet.unions $ usedValueSet value : map (usedValueSet . snd) typedValueList, lowComp)
    LC.Unreachable ->
      (IntSet.empty, LC.Unreachable)
    LC.Phi valueList ->
      case demand of
        DemandReturn ->
          (usedValueListSet valueList, LC.Phi valueList)
        DemandPhi liveMask -> do
          let valueList' = filterByMask liveMask valueList
          (usedValueListSet valueList', LC.Phi valueList')

isDiscardable :: LC.Op -> Bool
isDiscardable op =
  case op of
    LC.GetElementPtr {} ->
      True
    LC.Bitcast {} ->
      True
    LC.IntToPointer {} ->
      True
    LC.PointerToInt {} ->
      True
    LC.Load {} ->
      True
    LC.StackAlloc {} ->
      True
    LC.Calloc {} ->
      True
    LC.Alloc {} ->
      True
    LC.PrimOp {} ->
      True
    _ ->
      False

usedOpSet :: LC.Op -> IntSet.IntSet
usedOpSet op =
  case op of
    LC.Call _ value typedValueList ->
      IntSet.unions $ usedValueSet value : map (usedValueSet . snd) typedValueList
    LC.MagicCall _ value typedValueList ->
      IntSet.unions $ usedValueSet value : map (usedValueSet . snd) typedValueList
    LC.GetElementPtr (value, _) indexedValueList ->
      IntSet.unions $ usedValueSet value : map (usedValueSet . fst) indexedValueList
    LC.Bitcast value _ _ ->
      usedValueSet value
    LC.IntToPointer value _ ->
      usedValueSet value
    LC.PointerToInt value _ ->
      usedValueSet value
    LC.Load value _ ->
      usedValueSet value
    LC.Store _ value1 value2 ->
      IntSet.union (usedValueSet value1) (usedValueSet value2)
    LC.StackAlloc stackAllocInfo ->
      case LC.stackSize stackAllocInfo of
        Left {} ->
          IntSet.empty
        Right runtimeSize ->
          usedValueSet runtimeSize
    LC.StackLifetimeStart {} ->
      IntSet.empty
    LC.StackLifetimeEnd {} ->
      IntSet.empty
    LC.Calloc value1 value2 ->
      IntSet.union (usedValueSet value1) (usedValueSet value2)
    LC.Alloc size _ ->
      case size of
        Left {} ->
          IntSet.empty
        Right runtimeSize ->
          usedValueSet runtimeSize
    LC.Realloc value1 value2 ->
      IntSet.union (usedValueSet value1) (usedValueSet value2)
    LC.Free value _ _ ->
      usedValueSet value
    LC.PrimOp _ valueList ->
      usedValueListSet valueList

usedValueListSet :: [LC.Value] -> IntSet.IntSet
usedValueListSet =
  IntSet.unions . map usedValueSet

usedValueSet :: LC.Value -> IntSet.IntSet
usedValueSet value =
  case value of
    LC.VarLocal x ->
      IntSet.singleton (toInt x)
    _ ->
      IntSet.empty

deleteMany :: IntSet.IntSet -> [Ident] -> IntSet.IntSet
deleteMany =
  foldl (\acc x -> IntSet.delete (toInt x) acc)

filterByMask :: [Bool] -> [a] -> [a]
filterByMask mask xs =
  [x | (keep, x) <- zip mask xs, keep]
