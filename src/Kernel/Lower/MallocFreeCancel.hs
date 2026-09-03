module Kernel.Lower.MallocFreeCancel (mallocFreeCancel) where

import Control.Monad (join)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (foldl', transpose)
import Data.Map.Strict qualified as Map
import Language.Common.Ident
import Language.Common.Ident.Reify
import Language.Common.LowType qualified as LT
import Language.Common.DataSize qualified as DS
import Language.Common.PrimNumSize (IntSize (IntSize8), dataSizeToIntSize)
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

mallocFreeCancel :: DS.DataSize -> LC.Comp -> LC.Comp
mallocFreeCancel baseSize lowComp = do
  let roots = collectRoots IntMap.empty lowComp
  cancelMallocFree baseSize (fst (analyze roots lowComp)) lowComp

type RootMap =
  IntMap.IntMap Int

rootOf :: RootMap -> Ident -> Int
rootOf roots x =
  IntMap.findWithDefault (toInt x) (toInt x) roots

collectRoots :: RootMap -> LC.Comp -> RootMap
collectRoots roots lowComp =
  case lowComp of
    LC.Return {} ->
      roots
    LC.Let x op cont ->
      case getAliasSource op of
        Just y ->
          collectRoots (IntMap.insert (toInt x) (rootOf roots y) roots) cont
        Nothing ->
          collectRoots roots cont
    LC.Cont _ cont ->
      collectRoots roots cont
    LC.Switch _ _ defaultBranch ces _ cont ->
      collectRoots (foldl' collectRoots roots (defaultBranch : map snd ces)) cont
    LC.TailCall {} ->
      roots
    LC.Unreachable ->
      roots
    LC.Phi {} ->
      roots

data FreeState = FreeState
  { freeDefault :: Maybe IntSet.IntSet,
    freeByRoot :: IntMap.IntMap IntSet.IntSet
  }

escapingState :: FreeState
escapingState =
  FreeState {freeDefault = Nothing, freeByRoot = IntMap.empty}

deadState :: FreeState
deadState =
  FreeState {freeDefault = Just IntSet.empty, freeByRoot = IntMap.empty}

lookupFree :: FreeState -> Int -> Maybe IntSet.IntSet
lookupFree state root =
  case IntMap.lookup root (freeByRoot state) of
    Just freeIDs ->
      Just freeIDs
    Nothing ->
      freeDefault state

analyze :: RootMap -> LC.Comp -> (Axis, FreeState)
analyze roots lowComp =
  case lowComp of
    LC.Return {} ->
      (mempty, escapingState)
    LC.Let x op cont -> do
      let (axis, state) = analyze roots cont
      case op of
        LC.Alloc _ allocID ->
          case lookupFree state (rootOf roots x) of
            Just freeIDs ->
              let allocIDs = IntSet.singleton allocID
               in (axis <> Axis {allocCanceller = allocIDs, freeCanceller = newFreeCanceller allocIDs freeIDs}, state)
            Nothing ->
              (axis, state)
        _ ->
          (axis, state)
    LC.Cont op cont -> do
      let (axis, state) = analyze roots cont
      case op of
        LC.Free (LC.VarLocal ptr) _ freeID ->
          (axis, state {freeByRoot = IntMap.insert (rootOf roots ptr) (IntSet.singleton freeID) (freeByRoot state)})
        _ ->
          (axis, state)
    LC.Switch _ _ defaultBranch ces phiTargets cont -> do
      let (contAxis, contState) = analyze roots cont
      let (branchAxes, branchStates) = unzip $ map (analyze roots) (defaultBranch : map snd ces)
      let joinAxis = analyzeSwitchJoin (defaultBranch : map snd ces) (map fst phiTargets) contState
      (joinAxis <> mconcat (contAxis : branchAxes), joinStates contState branchStates)
    LC.TailCall {} ->
      (mempty, escapingState)
    LC.Unreachable ->
      (mempty, deadState)
    LC.Phi {} ->
      (mempty, escapingState)

joinStates :: FreeState -> [FreeState] -> FreeState
joinStates contState branchStates =
  case freeDefault contState of
    Just _ ->
      contState
    Nothing -> do
      let mergedDefault = mergeOrigins (map freeDefault branchStates)
      let branchKeys = IntSet.unions (map (IntMap.keysSet . freeByRoot) branchStates)
      let addBranchKey acc root =
            if IntMap.member root (freeByRoot contState)
              then acc
              else case mergeOrigins (map (`lookupFree` root) branchStates) of
                Just freeIDs ->
                  IntMap.insert root freeIDs acc
                Nothing ->
                  acc
      FreeState
        { freeDefault = mergedDefault,
          freeByRoot = IntSet.foldl' addBranchKey (freeByRoot contState) branchKeys
        }

getAliasSource :: LC.Op -> Maybe Ident
getAliasSource op =
  case op of
    LC.Bitcast (LC.VarLocal y) from to
      | from == to ->
          Just y
    LC.PointerToInt (LC.VarLocal y) _ ->
      Just y
    LC.IntToPointer (LC.VarLocal y) _ ->
      Just y
    _ ->
      Nothing

analyzeSwitchJoin :: [LC.Comp] -> [Ident] -> FreeState -> Axis
analyzeSwitchJoin branches phiTargets contState =
  mconcat $ flip map (zip [0 ..] phiTargets) $ \(index, phiTarget) ->
    case lookupFree contState (toInt phiTarget) of
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
              (zip (map fst phiTargets) resultOrigins)
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

cancelMallocFree :: DS.DataSize -> Axis -> LC.Comp -> LC.Comp
cancelMallocFree baseSize axis lowComp =
  case lowComp of
    LC.Return {} ->
      lowComp
    LC.Let x op cont -> do
      let cont' = cancelMallocFree baseSize axis cont
      case op of
        LC.Alloc size allocID
          | IntSet.member allocID (allocCanceller axis) -> do
              let stackAllocInfo =
                    LC.StackAllocInfo
                      { stackSlotID = allocID,
                        stackElemType = LT.PrimNum $ PT.Int IntSize8,
                        stackIndexType = LT.PrimNum $ PT.Int (dataSizeToIntSize baseSize),
                        stackSize = size
                      }
              LC.Let x (LC.StackAlloc stackAllocInfo) $
                LC.Cont (LC.StackLifetimeStart allocID) cont'
        _ ->
          LC.Let x op cont'
    LC.Cont op cont -> do
      let cont' = cancelMallocFree baseSize axis cont
      case op of
        LC.Free _ _ freeID
          | Just stackSlotIDs <- IntMap.lookup freeID (freeCanceller axis) ->
              foldr (LC.Cont . LC.StackLifetimeEnd) cont' (IntSet.toAscList stackSlotIDs)
        _ ->
          LC.Cont op cont'
    LC.Switch d t defaultBranch ces phiTargets cont -> do
      let defaultBranch' = cancelMallocFree baseSize axis defaultBranch
      let (cs, es) = unzip ces
      let es' = map (cancelMallocFree baseSize axis) es
      let cont' = cancelMallocFree baseSize axis cont
      LC.Switch d t defaultBranch' (zip cs es') phiTargets cont'
    LC.TailCall {} ->
      lowComp
    LC.Unreachable ->
      lowComp
    LC.Phi {} ->
      lowComp
