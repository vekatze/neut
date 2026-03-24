module Kernel.Lower.HoistStackAlloc (hoistStackAlloc) where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (foldl', minimumBy, sortOn)
import Data.Ord (Down (..), comparing)
import Gensym.Handle qualified as GensymHandle
import Language.Common.CreateSymbol qualified as CreateSymbol
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

hoistStackAlloc :: GensymHandle.Handle -> DS.DataSize -> LC.Comp -> IO LC.Comp
hoistStackAlloc gensymHandle dataSize lowComp =
  case collectHoistableSlotMap dataSize lowComp of
    slotMap
      | IntMap.null slotMap ->
          pure lowComp
      | otherwise -> do
          let conflictGraph = snd $ analyzeConflicts slotMap IntSet.empty lowComp
          let (slotAssignment, physicalSlotMap) = allocatePhysicalSlots slotMap conflictGraph
          physicalSlotIdentMap <- createPhysicalSlotIdentMap gensymHandle physicalSlotMap
          let lowComp' = rewriteComp slotAssignment physicalSlotIdentMap lowComp
          pure $ prependPhysicalSlotMap physicalSlotIdentMap physicalSlotMap lowComp'

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
    LC.Phi {} ->
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
    LC.Phi {} ->
      (Just active, IntMap.empty)

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

createPhysicalSlotIdentMap :: GensymHandle.Handle -> IntMap.IntMap PhysicalSlot -> IO (IntMap.IntMap Ident)
createPhysicalSlotIdentMap gensymHandle =
  traverse (const $ CreateSymbol.newIdentFromText gensymHandle "stack-slot")

rewriteComp ::
  IntMap.IntMap LC.StackSlotID ->
  IntMap.IntMap Ident ->
  LC.Comp ->
  LC.Comp
rewriteComp slotAssignment physicalSlotIdentMap lowComp =
  case lowComp of
    LC.Return {} ->
      lowComp
    LC.ReturnVoid ->
      lowComp
    LC.Let x op cont -> do
      let cont' = rewriteComp slotAssignment physicalSlotIdentMap cont
      case op of
        LC.StackAlloc stackAllocInfo
          | Just physicalSlotID' <- IntMap.lookup (LC.stackSlotID stackAllocInfo) slotAssignment,
            Just physicalSlotIdent <- IntMap.lookup physicalSlotID' physicalSlotIdentMap ->
              LC.Let x (LC.Bitcast (LC.VarLocal physicalSlotIdent) LT.Pointer LT.Pointer) cont'
        _ ->
          LC.Let x op cont'
    LC.Cont op cont -> do
      let cont' = rewriteComp slotAssignment physicalSlotIdentMap cont
      case op of
        LC.StackLifetimeStart stackSlotID
          | Just physicalSlotID' <- IntMap.lookup stackSlotID slotAssignment ->
              LC.Cont (LC.StackLifetimeStart physicalSlotID') cont'
        LC.StackLifetimeEnd stackSlotID
          | Just physicalSlotID' <- IntMap.lookup stackSlotID slotAssignment ->
              LC.Cont (LC.StackLifetimeEnd physicalSlotID') cont'
        _ ->
          LC.Cont op cont'
    LC.Switch d t defaultBranch ces phiTargets cont -> do
      let defaultBranch' = rewriteComp slotAssignment physicalSlotIdentMap defaultBranch
      let (cs, es) = unzip ces
      let es' = map (rewriteComp slotAssignment physicalSlotIdentMap) es
      let cont' = rewriteComp slotAssignment physicalSlotIdentMap cont
      LC.Switch d t defaultBranch' (zip cs es') phiTargets cont'
    LC.TailCall {} ->
      lowComp
    LC.Unreachable ->
      lowComp
    LC.Phi {} ->
      lowComp

prependPhysicalSlotMap :: IntMap.IntMap Ident -> IntMap.IntMap PhysicalSlot -> LC.Comp -> LC.Comp
prependPhysicalSlotMap physicalSlotIdentMap physicalSlotMap lowComp =
  foldr (prependPhysicalSlot physicalSlotIdentMap) lowComp $ sortOn physicalSlotID (IntMap.elems physicalSlotMap)

prependPhysicalSlot :: IntMap.IntMap Ident -> PhysicalSlot -> LC.Comp -> LC.Comp
prependPhysicalSlot physicalSlotIdentMap physicalSlot =
  LC.Let physicalSlotIdent $
    LC.StackAlloc $
      LC.StackAllocInfo
        { stackSlotID = physicalSlotID physicalSlot,
          stackElemType = LT.PrimNum $ PT.Int IntSize8,
          stackIndexType = physicalIndexType physicalSlot,
          stackSize = Left $ physicalByteSize physicalSlot
        }
  where
    physicalSlotIdent =
      IntMap.findWithDefault
        (error "missing physical slot ident")
        (physicalSlotID physicalSlot)
        physicalSlotIdentMap
