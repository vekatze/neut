module Kernel.Lower.HoistStackAlloc (hoistStackAlloc) where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (foldl', minimumBy, sortOn)
import Data.Ord (Down (..), comparing)
import Data.Text qualified as T
import Language.Common.DataSize qualified as DS
import Language.Common.Ident
import Language.Common.LowType qualified as LT
import Language.Common.LowType.ToByteSize (lowTypeToByteSize)
import Language.Common.PrimNumSize (IntSize (IntSize8))
import Language.Common.PrimType qualified as PT
import Language.LowComp.LowComp qualified as LC

type ActiveSlotSet =
  IntSet.IntSet

type ConflictGraph =
  IntMap.IntMap IntSet.IntSet

data HoistableSlot = HoistableSlot
  { hoistableByteSize :: Integer,
    hoistableIndexType :: LT.LowType
  }

data PhysicalSlot = PhysicalSlot
  { physicalSlotID :: LC.StackSlotID,
    physicalByteSize :: Integer,
    physicalIndexType :: LT.LowType,
    physicalMembers :: IntSet.IntSet
  }

hoistStackAlloc :: DS.DataSize -> LC.Comp -> LC.Comp
hoistStackAlloc dataSize lowComp =
  case collectHoistableSlotMap dataSize lowComp of
    slotMap
      | IntMap.null slotMap ->
          lowComp
      | otherwise -> do
          let conflictGraph = snd $ analyzeConflicts slotMap IntSet.empty lowComp
          let (slotAssignment, physicalSlotMap) = allocatePhysicalSlots slotMap conflictGraph
          let lowComp' = rewriteComp dataSize slotAssignment physicalSlotMap lowComp
          prependPhysicalSlotMap physicalSlotMap lowComp'

collectHoistableSlotMap :: DS.DataSize -> LC.Comp -> IntMap.IntMap HoistableSlot
collectHoistableSlotMap dataSize lowComp =
  case lowComp of
    LC.Return {} ->
      IntMap.empty
    LC.ReturnVoid ->
      IntMap.empty
    LC.Let _ op cont ->
      case op of
        LC.StackAlloc stackAllocInfo ->
          case getKnownByteSize dataSize stackAllocInfo of
            Just byteSize ->
              IntMap.insert
                (LC.stackSlotID stackAllocInfo)
                ( HoistableSlot
                    { hoistableByteSize = byteSize,
                      hoistableIndexType = LC.stackIndexType stackAllocInfo
                    }
                )
                (collectHoistableSlotMap dataSize cont)
            Nothing ->
              collectHoistableSlotMap dataSize cont
        _ ->
          collectHoistableSlotMap dataSize cont
    LC.Cont _ cont ->
      collectHoistableSlotMap dataSize cont
    LC.Switch _ _ defaultBranch ces _ cont -> do
      let branchMap = IntMap.unions $ map (collectHoistableSlotMap dataSize) (defaultBranch : map snd ces)
      branchMap <> collectHoistableSlotMap dataSize cont
    LC.TailCall {} ->
      IntMap.empty
    LC.Unreachable ->
      IntMap.empty

getKnownByteSize :: DS.DataSize -> LC.StackAllocInfo -> Maybe Integer
getKnownByteSize dataSize stackAllocInfo =
  case LC.stackSize stackAllocInfo of
    Left knownSize ->
      Just $ lowTypeToByteSize dataSize (LC.stackElemType stackAllocInfo) * knownSize
    Right {} ->
      Nothing

analyzeConflicts :: IntMap.IntMap HoistableSlot -> ActiveSlotSet -> LC.Comp -> (Maybe ActiveSlotSet, ConflictGraph)
analyzeConflicts slotMap active lowComp =
  case lowComp of
    LC.Return {} ->
      (Just active, IntMap.empty)
    LC.ReturnVoid ->
      (Just active, IntMap.empty)
    LC.Let _ _ cont ->
      analyzeConflicts slotMap active cont
    LC.Cont op cont -> do
      let (active', graph1) = stepConflict slotMap active op
      let (outActive, graph2) = analyzeConflicts slotMap active' cont
      (outActive, mergeConflictGraph graph1 graph2)
    LC.Switch _ _ defaultBranch ces _ cont -> do
      let branches = defaultBranch : map snd ces
      let branchResults = map (analyzeConflicts slotMap active) branches
      let branchGraph = foldMap snd branchResults
      let reachableActives = [activeSet | (Just activeSet, _) <- branchResults]
      case reachableActives of
        [] ->
          (Nothing, branchGraph)
        _ -> do
          let active' = IntSet.unions reachableActives
          let (outActive, contGraph) = analyzeConflicts slotMap active' cont
          (outActive, mergeConflictGraph branchGraph contGraph)
    LC.TailCall {} ->
      (Nothing, IntMap.empty)
    LC.Unreachable ->
      (Nothing, IntMap.empty)

stepConflict :: IntMap.IntMap HoistableSlot -> ActiveSlotSet -> LC.Op -> (ActiveSlotSet, ConflictGraph)
stepConflict slotMap active op =
  case op of
    LC.StackLifetimeStart stackSlotID
      | IntMap.member stackSlotID slotMap -> do
          let graph = foldl' (\acc activeSlotID -> addConflict stackSlotID activeSlotID acc) IntMap.empty (IntSet.toList active)
          (IntSet.insert stackSlotID active, graph)
    LC.StackLifetimeEnd stackSlotID
      | IntMap.member stackSlotID slotMap ->
          (IntSet.delete stackSlotID active, IntMap.empty)
    _ ->
      (active, IntMap.empty)

mergeConflictGraph :: ConflictGraph -> ConflictGraph -> ConflictGraph
mergeConflictGraph =
  IntMap.unionWith IntSet.union

addConflict :: LC.StackSlotID -> LC.StackSlotID -> ConflictGraph -> ConflictGraph
addConflict slotID1 slotID2 graph
  | slotID1 == slotID2 =
      graph
  | otherwise =
      IntMap.insertWith IntSet.union slotID1 (IntSet.singleton slotID2) $
        IntMap.insertWith IntSet.union slotID2 (IntSet.singleton slotID1) graph

allocatePhysicalSlots ::
  IntMap.IntMap HoistableSlot ->
  ConflictGraph ->
  (IntMap.IntMap LC.StackSlotID, IntMap.IntMap PhysicalSlot)
allocatePhysicalSlots slotMap conflictGraph = do
  let orderedSlotList =
        sortOn
          (Down . hoistableByteSize . snd)
          (IntMap.toList slotMap)
  let (physicalSlots, slotAssignment) =
        foldl' (assignPhysicalSlot conflictGraph) ([], IntMap.empty) orderedSlotList
  (slotAssignment, IntMap.fromList $ map (\slot -> (physicalSlotID slot, slot)) physicalSlots)

assignPhysicalSlot ::
  ConflictGraph ->
  ([PhysicalSlot], IntMap.IntMap LC.StackSlotID) ->
  (LC.StackSlotID, HoistableSlot) ->
  ([PhysicalSlot], IntMap.IntMap LC.StackSlotID)
assignPhysicalSlot conflictGraph (physicalSlots, slotAssignment) (stackSlotID, hoistableSlot) =
  case findReusableSlot conflictGraph stackSlotID physicalSlots of
    Just reusableSlot -> do
      let physicalSlotID' = physicalSlotID reusableSlot
      let updatedReusableSlot =
            reusableSlot
              { physicalByteSize = max (physicalByteSize reusableSlot) (hoistableByteSize hoistableSlot),
                physicalMembers = IntSet.insert stackSlotID (physicalMembers reusableSlot)
              }
      let physicalSlots' = updatedReusableSlot : filter ((/= physicalSlotID') . physicalSlotID) physicalSlots
      (physicalSlots', IntMap.insert stackSlotID physicalSlotID' slotAssignment)
    Nothing -> do
      let physicalSlot =
            PhysicalSlot
              { physicalSlotID = stackSlotID,
                physicalByteSize = hoistableByteSize hoistableSlot,
                physicalIndexType = hoistableIndexType hoistableSlot,
                physicalMembers = IntSet.singleton stackSlotID
              }
      (physicalSlot : physicalSlots, IntMap.insert stackSlotID stackSlotID slotAssignment)

findReusableSlot :: ConflictGraph -> LC.StackSlotID -> [PhysicalSlot] -> Maybe PhysicalSlot
findReusableSlot conflictGraph stackSlotID physicalSlots =
  case compatibleSlots of
    [] ->
      Nothing
    _ ->
      Just $ minimumBy (comparing physicalByteSize) compatibleSlots
  where
    compatibleSlots =
      filter (isCompatiblePhysicalSlot conflictGraph stackSlotID) physicalSlots

isCompatiblePhysicalSlot :: ConflictGraph -> LC.StackSlotID -> PhysicalSlot -> Bool
isCompatiblePhysicalSlot conflictGraph stackSlotID physicalSlot = do
  let conflictSet = IntMap.findWithDefault IntSet.empty stackSlotID conflictGraph
  IntSet.null $ IntSet.intersection conflictSet (physicalMembers physicalSlot)

rewriteComp ::
  DS.DataSize ->
  IntMap.IntMap LC.StackSlotID ->
  IntMap.IntMap PhysicalSlot ->
  LC.Comp ->
  LC.Comp
rewriteComp dataSize slotAssignment physicalSlotMap lowComp =
  case lowComp of
    LC.Return {} ->
      lowComp
    LC.ReturnVoid ->
      lowComp
    LC.Let x op cont -> do
      let cont' = rewriteComp dataSize slotAssignment physicalSlotMap cont
      case op of
        LC.StackAlloc stackAllocInfo
          | Just _ <- getKnownByteSize dataSize stackAllocInfo,
            Just physicalSlotID' <- IntMap.lookup (LC.stackSlotID stackAllocInfo) slotAssignment,
            Just _ <- IntMap.lookup physicalSlotID' physicalSlotMap ->
              LC.Let x (LC.Bitcast (LC.VarLocal $ physicalSlotIdent physicalSlotID') LT.Pointer LT.Pointer) cont'
        _ ->
          LC.Let x op cont'
    LC.Cont op cont -> do
      let cont' = rewriteComp dataSize slotAssignment physicalSlotMap cont
      case op of
        LC.StackLifetimeStart stackSlotID
          | Just physicalSlotID' <- IntMap.lookup stackSlotID slotAssignment ->
              LC.Cont (LC.StackLifetimeStart physicalSlotID') cont'
        LC.StackLifetimeEnd stackSlotID
          | Just physicalSlotID' <- IntMap.lookup stackSlotID slotAssignment ->
              LC.Cont (LC.StackLifetimeEnd physicalSlotID') cont'
        _ ->
          LC.Cont op cont'
    LC.Switch d t defaultBranch ces phiTarget cont -> do
      let defaultBranch' = rewriteComp dataSize slotAssignment physicalSlotMap defaultBranch
      let (cs, es) = unzip ces
      let es' = map (rewriteComp dataSize slotAssignment physicalSlotMap) es
      let cont' = rewriteComp dataSize slotAssignment physicalSlotMap cont
      LC.Switch d t defaultBranch' (zip cs es') phiTarget cont'
    LC.TailCall {} ->
      lowComp
    LC.Unreachable ->
      lowComp

prependPhysicalSlotMap :: IntMap.IntMap PhysicalSlot -> LC.Comp -> LC.Comp
prependPhysicalSlotMap physicalSlotMap lowComp =
  foldr prependPhysicalSlot lowComp $ sortOn physicalSlotID (IntMap.elems physicalSlotMap)

prependPhysicalSlot :: PhysicalSlot -> LC.Comp -> LC.Comp
prependPhysicalSlot physicalSlot =
  LC.Let (physicalSlotIdent $ physicalSlotID physicalSlot) $
    LC.StackAlloc $
      LC.StackAllocInfo
        { stackSlotID = physicalSlotID physicalSlot,
          stackElemType = LT.PrimNum $ PT.Int IntSize8,
          stackIndexType = physicalIndexType physicalSlot,
          stackSize = Left $ physicalByteSize physicalSlot
        }

physicalSlotIdent :: LC.StackSlotID -> Ident
physicalSlotIdent stackSlotID =
  I (T.pack "stack-slot", negate stackSlotID - 1)
