module Kernel.Lower.MallocFreeCancel (mallocFreeCancel) where

import Control.Monad (join)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (foldl', transpose)
import Data.Map.Strict qualified as Map
import Data.Set qualified as S
import Language.Common.Ident
import Language.Common.LowType qualified as LT
import Language.Common.PrimNumSize (IntSize (IntSize64, IntSize8))
import Language.Common.PrimType qualified as PT
import Language.LowComp.LowComp qualified as LC

data Axis = Axis
  { allocCanceller :: IntSet.IntSet,
    freeCanceller :: IntMap.IntMap IntSet.IntSet
  }

emptyAxis :: Axis
emptyAxis =
  Axis {allocCanceller = IntSet.empty, freeCanceller = IntMap.empty}

instance Semigroup Axis where
  Axis alloc1 free1 <> Axis alloc2 free2 =
    Axis
      { allocCanceller = IntSet.union alloc1 alloc2,
        freeCanceller = IntMap.unionWith IntSet.union free1 free2
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
        LC.Alloc _ allocID ->
          case collectFreeIDs (S.singleton x) cont of
            Just freeIDs ->
              let allocIDs = IntSet.singleton allocID
               in axis <> Axis {allocCanceller = allocIDs, freeCanceller = newFreeCanceller allocIDs freeIDs}
            Nothing ->
              axis
        _ ->
          axis
    LC.Cont _ cont ->
      analyze cont
    LC.Switch _ _ defaultBranch ces phiTargets cont ->
      let branches = defaultBranch : map snd ces
       in analyzeSwitchJoin branches phiTargets cont <> mconcat (map analyze (cont : branches))
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

analyzeSwitchJoin :: [LC.Comp] -> [Ident] -> LC.Comp -> Axis
analyzeSwitchJoin branches phiTargets cont =
  mconcat $ flip map (zip [0 ..] phiTargets) $ \(index, phiTarget) ->
    case collectFreeIDs (S.singleton phiTarget) cont of
      Just freeIDs ->
        case traverse (collectBranchResultAllocIDs index) branches of
          Just allocIDList ->
            let allocIDs = IntSet.unions allocIDList
             in Axis
                  { allocCanceller = allocIDs,
                    freeCanceller = newFreeCanceller allocIDs freeIDs
                  }
          Nothing ->
            mempty
      Nothing ->
        mempty

collectBranchResultAllocIDs :: Int -> LC.Comp -> Maybe IntSet.IntSet
collectBranchResultAllocIDs index branch = do
  resultOrigin <- collectBranchResultOrigin Map.empty branch
  case resultOrigin of
    DeadBranch ->
      Just IntSet.empty
    ReachableBranch originList ->
      join $ getAt index originList

data BranchResultOrigin
  = DeadBranch
  | ReachableBranch [Maybe IntSet.IntSet]

type OriginEnv =
  Map.Map Ident (Maybe IntSet.IntSet)

collectBranchResultOrigin :: OriginEnv -> LC.Comp -> Maybe BranchResultOrigin
collectBranchResultOrigin env lowComp =
  case lowComp of
    LC.Return {} ->
      Nothing
    LC.ReturnVoid ->
      Nothing
    LC.Let x op cont -> do
      let env' = Map.insert x (getOrigin env op) env
      collectBranchResultOrigin env' cont
    LC.Cont _ cont ->
      collectBranchResultOrigin env cont
    LC.Switch _ _ defaultBranch ces phiTargets cont -> do
      resultOrigins <- collectMergedBranchResultOrigins env (length phiTargets) (defaultBranch : map snd ces)
      let env' =
            foldl'
              (\acc (phiTarget, resultOrigin) -> Map.insert phiTarget resultOrigin acc)
              env
              (zip phiTargets resultOrigins)
      collectBranchResultOrigin env' cont
    LC.TailCall {} ->
      Nothing
    LC.Unreachable ->
      Just DeadBranch
    LC.Phi values ->
      Just $ ReachableBranch $ map (getValueOrigin env) values

getOrigin :: OriginEnv -> LC.Op -> Maybe IntSet.IntSet
getOrigin env op =
  case op of
    LC.Alloc _ allocID ->
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

collectMergedBranchResultOrigins :: OriginEnv -> Int -> [LC.Comp] -> Maybe [Maybe IntSet.IntSet]
collectMergedBranchResultOrigins env phiCount branches = do
  resultOrigins <- traverse (collectBranchResultOrigin env) branches
  let reachableOrigins =
        [originList | ReachableBranch originList <- resultOrigins]
  case reachableOrigins of
    [] ->
      return $ replicate phiCount Nothing
    _ -> do
      if all ((== phiCount) . length) reachableOrigins
        then return $ map mergeOrigins $ transpose reachableOrigins
        else Nothing

mergeOrigins :: [Maybe IntSet.IntSet] -> Maybe IntSet.IntSet
mergeOrigins origins =
  IntSet.unions <$> sequence origins

getAt :: Int -> [a] -> Maybe a
getAt index xs =
  case drop index xs of
    y : _ ->
      Just y
    [] ->
      Nothing

newFreeCanceller :: IntSet.IntSet -> IntSet.IntSet -> IntMap.IntMap IntSet.IntSet
newFreeCanceller allocIDs freeIDs =
  IntMap.fromList $ map (,allocIDs) (IntSet.toList freeIDs)

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
        LC.Alloc size allocID
          | IntSet.member allocID (allocCanceller axis) -> do
              let stackAllocInfo =
                    LC.StackAllocInfo
                      { stackSlotID = allocID,
                        stackElemType = LT.PrimNum $ PT.Int IntSize8,
                        stackIndexType = LT.PrimNum $ PT.Int IntSize64,
                        stackSize = size
                      }
              LC.Let x (LC.StackAlloc stackAllocInfo) $
                LC.Cont (LC.StackLifetimeStart allocID) cont'
        _ ->
          LC.Let x op cont'
    LC.Cont op cont -> do
      let cont' = cancelMallocFree axis cont
      case op of
        LC.Free _ _ freeID
          | Just stackSlotIDs <- IntMap.lookup freeID (freeCanceller axis) ->
              foldr (LC.Cont . LC.StackLifetimeEnd) cont' (IntSet.toAscList stackSlotIDs)
        _ ->
          LC.Cont op cont'
    LC.Switch d t defaultBranch ces phiTargets cont -> do
      let defaultBranch' = cancelMallocFree axis defaultBranch
      let (cs, es) = unzip ces
      let es' = map (cancelMallocFree axis) es
      let cont' = cancelMallocFree axis cont
      LC.Switch d t defaultBranch' (zip cs es') phiTargets cont'
    LC.TailCall {} ->
      lowComp
    LC.Unreachable ->
      lowComp
    LC.Phi {} ->
      lowComp
