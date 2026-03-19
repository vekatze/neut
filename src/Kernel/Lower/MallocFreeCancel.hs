module Kernel.Lower.MallocFreeCancel (mallocFreeCancel) where

import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set qualified as S
import Language.Common.Ident
import Language.Common.LowType qualified as LT
import Language.Common.PrimNumSize (IntSize (IntSize64, IntSize8))
import Language.Common.PrimType qualified as PT
import Language.LowComp.LowComp qualified as LC

data Axis = Axis
  { allocCanceller :: IntSet.IntSet,
    freeCanceller :: IntSet.IntSet
  }

emptyAxis :: Axis
emptyAxis =
  Axis {allocCanceller = IntSet.empty, freeCanceller = IntSet.empty}

instance Semigroup Axis where
  Axis alloc1 free1 <> Axis alloc2 free2 =
    Axis
      { allocCanceller = IntSet.union alloc1 alloc2,
        freeCanceller = IntSet.union free1 free2
      }

instance Monoid Axis where
  mempty = emptyAxis

mallocFreeCancel :: LC.Comp -> LC.Comp
mallocFreeCancel lowComp =
  cancelMallocFree (analyze lowComp) lowComp

analyze :: LC.Comp -> Axis
analyze lowComp =
  case lowComp of
    LC.Return {} ->
      mempty
    LC.ReturnVoid ->
      mempty
    LC.Let x op cont -> do
      let axis = analyze cont
      case op of
        LC.Alloc _ _ allocID ->
          case collectFreeIDs (S.singleton x) cont of
            Just freeIDs ->
              axis <> Axis {allocCanceller = IntSet.singleton allocID, freeCanceller = freeIDs}
            Nothing ->
              axis
        _ ->
          axis
    LC.Cont _ cont ->
      analyze cont
    LC.Switch _ _ defaultBranch ces phiTarget cont ->
      let branches = defaultBranch : map snd ces
       in analyzeSwitchJoin branches phiTarget cont <> mconcat (map analyze (cont : branches))
    LC.TailCall {} ->
      mempty
    LC.Unreachable ->
      mempty
    LC.Phi {} ->
      mempty

collectFreeIDs :: S.Set Ident -> LC.Comp -> Maybe IntSet.IntSet
collectFreeIDs aliases lowComp =
  case lowComp of
    LC.Return {} ->
      Nothing
    LC.ReturnVoid ->
      Nothing
    LC.Let x op cont ->
      case getAliasSource op of
        Just y
          | S.member y aliases ->
              collectFreeIDs (S.insert x aliases) cont
        _ ->
          collectFreeIDs aliases cont
    LC.Cont op cont ->
      case op of
        LC.Free (LC.VarLocal ptr) _ freeID
          | S.member ptr aliases ->
              Just $ IntSet.singleton freeID
        _ ->
          collectFreeIDs aliases cont
    LC.Switch _ _ defaultBranch ces _ cont ->
      case collectFreeIDs aliases cont of
        Just freeIDs ->
          Just freeIDs
        Nothing ->
          IntSet.unions <$> traverse (collectFreeIDs aliases) (defaultBranch : map snd ces)
    LC.TailCall {} ->
      Nothing
    LC.Unreachable ->
      Just IntSet.empty
    LC.Phi {} ->
      Nothing

getAliasSource :: LC.Op -> Maybe Ident
getAliasSource op =
  case op of
    LC.Bitcast (LC.VarLocal y) LT.Pointer LT.Pointer ->
      Just y
    LC.PointerToInt (LC.VarLocal y) _ ->
      Just y
    LC.IntToPointer (LC.VarLocal y) _ ->
      Just y
    _ ->
      Nothing

analyzeSwitchJoin :: [LC.Comp] -> Ident -> LC.Comp -> Axis
analyzeSwitchJoin branches phiTarget cont =
  case collectFreeIDs (S.singleton phiTarget) cont of
    Just freeIDs ->
      case traverse collectBranchAllocIDs branches of
        Just allocIDList ->
          Axis
            { allocCanceller = IntSet.unions allocIDList,
              freeCanceller = freeIDs
            }
        Nothing ->
          mempty
    Nothing ->
      mempty

collectBranchAllocIDs :: LC.Comp -> Maybe IntSet.IntSet
collectBranchAllocIDs branch = do
  phiOrigin <- collectBranchPhiOrigins Map.empty branch
  case phiOrigin of
    DeadBranch ->
      Just IntSet.empty
    ReachableBranch origin ->
      origin

data BranchPhiOrigins
  = DeadBranch
  | ReachableBranch (Maybe IntSet.IntSet)

type OriginEnv =
  Map.Map Ident (Maybe IntSet.IntSet)

collectBranchPhiOrigins :: OriginEnv -> LC.Comp -> Maybe BranchPhiOrigins
collectBranchPhiOrigins env lowComp =
  case lowComp of
    LC.Return {} ->
      Nothing
    LC.ReturnVoid ->
      Nothing
    LC.Let x op cont -> do
      let env' = Map.insert x (getOrigin env op) env
      collectBranchPhiOrigins env' cont
    LC.Cont _ cont ->
      collectBranchPhiOrigins env cont
    LC.Switch _ _ defaultBranch ces phiTarget cont -> do
      phiOrigin <- collectMergedPhiOrigins env (defaultBranch : map snd ces)
      let env' = Map.insert phiTarget phiOrigin env
      collectBranchPhiOrigins env' cont
    LC.TailCall {} ->
      Nothing
    LC.Unreachable ->
      Just DeadBranch
    LC.Phi value ->
      Just $ ReachableBranch $ getValueOrigin env value

getOrigin :: OriginEnv -> LC.Op -> Maybe IntSet.IntSet
getOrigin env op =
  case op of
    LC.Alloc _ _ allocID ->
      Just $ IntSet.singleton allocID
    _ ->
      case getAliasSource op of
        Just y ->
          Map.findWithDefault Nothing y env
        Nothing ->
          Nothing

getValueOrigin :: OriginEnv -> LC.Value -> Maybe IntSet.IntSet
getValueOrigin env value =
  case value of
    LC.VarLocal x ->
      Map.findWithDefault Nothing x env
    _ ->
      Nothing

collectMergedPhiOrigins :: OriginEnv -> [LC.Comp] -> Maybe (Maybe IntSet.IntSet)
collectMergedPhiOrigins env branches = do
  phiOrigins <- traverse (collectBranchPhiOrigins env) branches
  let reachableOrigins =
        [origin | ReachableBranch origin <- phiOrigins]
  case reachableOrigins of
    [] ->
      return Nothing
    _ ->
      return $ mergeOrigins reachableOrigins

mergeOrigins :: [Maybe IntSet.IntSet] -> Maybe IntSet.IntSet
mergeOrigins origins =
  IntSet.unions <$> sequence origins

cancelMallocFree :: Axis -> LC.Comp -> LC.Comp
cancelMallocFree axis lowComp =
  case lowComp of
    LC.Return {} ->
      lowComp
    LC.ReturnVoid ->
      lowComp
    LC.Let x op cont -> do
      let cont' = cancelMallocFree axis cont
      case op of
        LC.Alloc size _ allocID
          | IntSet.member allocID (allocCanceller axis) -> do
              let byteType = LT.PrimNum $ PT.Int IntSize8
              let indexType = LT.PrimNum $ PT.Int IntSize64
              LC.Let x (LC.StackAlloc byteType indexType size) cont'
        _ ->
          LC.Let x op cont'
    LC.Cont op cont -> do
      let cont' = cancelMallocFree axis cont
      case op of
        LC.Free _ _ freeID
          | IntSet.member freeID (freeCanceller axis) ->
              cont'
        _ ->
          LC.Cont op cont'
    LC.Switch d t defaultBranch ces phi cont -> do
      let defaultBranch' = cancelMallocFree axis defaultBranch
      let (cs, es) = unzip ces
      let es' = map (cancelMallocFree axis) es
      let cont' = cancelMallocFree axis cont
      LC.Switch d t defaultBranch' (zip cs es') phi cont'
    LC.TailCall {} ->
      lowComp
    LC.Unreachable ->
      lowComp
    LC.Phi {} ->
      lowComp
