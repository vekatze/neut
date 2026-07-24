module Language.Term.Trace
  ( Handle,
    FrameID (..),
    FrameKind (..),
    TraceEntry (..),
    Frame (..),
    TracePath (..),
    Snapshot,
    Remapping,
    new,
    registerSourceSite,
    registerExpansion,
    resolve,
    snapshot,
    restore,
    remapOrKeep,
    remapOrDrop,
  )
where

import Data.Binary (Binary)
import Data.IORef
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (foldl')
import Data.Map.Strict qualified as Map
import GHC.Generics (Generic)
import Language.Common.DefiniteDescription qualified as DD
import Language.Term.TraceID
import Logger.Hint

newtype FrameID = FrameID Int
  deriving (Eq, Ord, Show)

data FrameKind
  = NoInlineFrame
  | InlineFrame
  | MacroFrame
  | MacroInlineFrame
  | ConstantMetaFrame
  | DataIntroFrame
  | DefaultArgFrame
  deriving (Eq, Show, Generic)

instance Binary FrameKind

data TraceEntry
  = SourceSite SavedHint
  | UnderFrame FrameID TraceID
  deriving (Show)

data Frame = Frame
  { callee :: DD.DefiniteDescription,
    callSite :: TraceID,
    frameKind :: FrameKind
  }
  deriving (Eq, Show)

data TracePath = TracePath
  { sourceSite :: Maybe SavedHint,
    frames :: [Frame]
  }

data CompactEntry
  = CompactSource Int Line Column Bool
  | CompactUnder Int Int
  deriving (Generic)

instance Binary CompactEntry

data CompactFrame
  = CompactFrame Int Int FrameKind
  deriving (Generic)

instance Binary CompactFrame

data Snapshot = Snapshot
  { snapshotPaths :: [FilePath],
    snapshotCallees :: [DD.DefiniteDescription],
    snapshotEntryKeys :: [Int],
    snapshotEntries :: [CompactEntry],
    snapshotFrames :: [CompactFrame],
    snapshotSiteGroups :: [[Int]]
  }
  deriving (Generic)

instance Binary Snapshot

newtype Remapping = Remapping (IntMap.IntMap TraceID)

data TraceState = TraceState
  { nextTraceID :: {-# UNPACK #-} !Int,
    nextFrameID :: {-# UNPACK #-} !Int,
    traceEntries :: !(IntMap.IntMap TraceEntry),
    traceFrames :: !(IntMap.IntMap Frame)
  }

newtype Handle = Handle (IORef TraceState)

new :: IO Handle
new = do
  ref <- newIORef $ TraceState 1 1 IntMap.empty IntMap.empty
  return $ Handle ref

registerSourceSite :: Handle -> Hint -> IO TraceID
registerSourceSite h m =
  registerEntry h $ SourceSite $ SavedHint m

registerExpansion ::
  Handle ->
  DD.DefiniteDescription ->
  Hint ->
  TraceID ->
  FrameKind ->
  IntSet.IntSet ->
  IO Remapping
registerExpansion (Handle ref) callee callHint currentCallSite frameKind innerIDs = do
  atomicModifyIORef' ref $ \state -> do
    let (callSite, stateWithCallSite) = registerHintInState callHint currentCallSite state
    let rawFrameID = nextFrameID stateWithCallSite
    let frameID = FrameID rawFrameID
    let frame = Frame {callee, callSite, frameKind}
    let innerKeys = IntSet.toAscList innerIDs
    let firstRawTraceID = nextTraceID stateWithCallSite
    let rawTraceIDs = take (length innerKeys) [firstRawTraceID ..]
    let newTraceIDs = map TraceID rawTraceIDs
    let remapping = IntMap.fromDistinctAscList $ zip innerKeys newTraceIDs
    let newEntries = IntMap.fromDistinctAscList $ zip rawTraceIDs (map (UnderFrame frameID . TraceID) innerKeys)
    let entries' = IntMap.union newEntries (traceEntries stateWithCallSite)
    let frames' = IntMap.insert rawFrameID frame (traceFrames stateWithCallSite)
    let state' =
          stateWithCallSite
            { nextTraceID = firstRawTraceID + length innerKeys,
              nextFrameID = rawFrameID + 1,
              traceEntries = entries',
              traceFrames = frames'
            }
    (state', Remapping remapping)

registerHintInState :: Hint -> TraceID -> TraceState -> (TraceID, TraceState)
registerHintInState m currentTraceID state = do
  if currentTraceID /= noTrace || null (metaFileName m)
    then (currentTraceID, state)
    else do
      let rawID = nextTraceID state
      let traceID = TraceID rawID
      let entry = SourceSite $ SavedHint m
      let entries' = IntMap.insert rawID entry (traceEntries state)
      (traceID, state {nextTraceID = rawID + 1, traceEntries = entries'})

registerEntry :: Handle -> TraceEntry -> IO TraceID
registerEntry (Handle ref) entry = do
  atomicModifyIORef' ref $ \state -> do
    let rawID = nextTraceID state
    let traceID = TraceID rawID
    let entries' = IntMap.insert rawID entry (traceEntries state)
    (state {nextTraceID = rawID + 1, traceEntries = entries'}, traceID)

resolve :: Handle -> TraceID -> IO TracePath
resolve (Handle ref) traceID = do
  state <- readIORef ref
  return $ resolveFrom state traceID

snapshot :: Handle -> IntSet.IntSet -> [IntSet.IntSet] -> IO Snapshot
snapshot (Handle ref) roots siteGroups = do
  state <- readIORef ref
  let (entryKeySet, frameKeySet) = collectReachable state IntSet.empty IntSet.empty (IntSet.toList roots)
  let entryKeys = filter (\key -> IntMap.member key (traceEntries state)) (IntSet.toAscList entryKeySet)
  let frameKeys = filter (\key -> IntMap.member key (traceFrames state)) (IntSet.toAscList frameKeySet)
  let entryDense = IntMap.fromDistinctAscList $ zip entryKeys [1 ..]
  let frameDense = IntMap.fromDistinctAscList $ zip frameKeys [1 ..]
  let rawEntries = map (traceEntries state IntMap.!) entryKeys
  let rawFrames = map (traceFrames state IntMap.!) frameKeys
  let (paths, pathIndex) = internTable [metaFileName m | SourceSite (SavedHint m) <- rawEntries]
  let (callees, calleeIndex) = internTable (map callee rawFrames)
  let denseOfTrace traceID = IntMap.findWithDefault 0 (traceIDKey traceID) entryDense
  let compactEntry entry =
        case entry of
          SourceSite (SavedHint m) -> do
            let (line, column) = metaLocation m
            CompactSource (pathIndex Map.! metaFileName m) line column (metaShouldSaveLocation m)
          UnderFrame frameID inner ->
            CompactUnder (IntMap.findWithDefault 0 (frameIDKey frameID) frameDense) (denseOfTrace inner)
  let compactFrame frame =
        CompactFrame (calleeIndex Map.! callee frame) (denseOfTrace (callSite frame)) (frameKind frame)
  let denseGroups = map (filter (/= 0) . map (denseOfTrace . TraceID) . IntSet.toAscList) siteGroups
  return $
    Snapshot
      { snapshotPaths = paths,
        snapshotCallees = callees,
        snapshotEntryKeys = entryKeys,
        snapshotEntries = map compactEntry rawEntries,
        snapshotFrames = map compactFrame rawFrames,
        snapshotSiteGroups = denseGroups
      }

internTable :: (Ord a) => [a] -> ([a], Map.Map a Int)
internTable items = do
  let step (revTable, index) item =
        if Map.member item index
          then (revTable, index)
          else (item : revTable, Map.insert item (Map.size index) index)
  let (revTable, index) = foldl' step ([], Map.empty) items
  (reverse revTable, index)

restore :: Handle -> Snapshot -> IO (Remapping, [IntSet.IntSet])
restore (Handle ref) (Snapshot paths callees entryKeys entries frameList siteGroups) = do
  atomicModifyIORef' ref $ \state -> do
    let entryCount = length entries
    let frameCount = length frameList
    let entryBase = nextTraceID state
    let frameBase = nextFrameID state
    let pathTable = IntMap.fromDistinctAscList $ zip [0 ..] paths
    let calleeTable = IntMap.fromDistinctAscList $ zip [0 ..] callees
    let globalTrace denseID =
          if denseID == 0
            then noTrace
            else TraceID (entryBase + denseID - 1)
    let expandEntry entry =
          case entry of
            CompactSource pathIndex line column shouldSaveLocation -> do
              let m =
                    Hint
                      { metaFileName = pathTable IntMap.! pathIndex,
                        metaLocation = (line, column),
                        metaShouldSaveLocation = shouldSaveLocation
                      }
              SourceSite (SavedHint m)
            CompactUnder frameIndex inner ->
              UnderFrame (FrameID (frameBase + frameIndex - 1)) (globalTrace inner)
    let expandFrame (CompactFrame calleeIndex callSiteID kind) =
          Frame
            { callee = calleeTable IntMap.! calleeIndex,
              callSite = globalTrace callSiteID,
              frameKind = kind
            }
    let newEntries = IntMap.fromDistinctAscList $ zip [entryBase ..] (map expandEntry entries)
    let newFrames = IntMap.fromDistinctAscList $ zip [frameBase ..] (map expandFrame frameList)
    let remapping = Remapping $ IntMap.fromDistinctAscList $ zip entryKeys (map TraceID [entryBase ..])
    let globalGroups = map (IntSet.fromDistinctAscList . map (\denseID -> entryBase + denseID - 1)) siteGroups
    let state' =
          state
            { nextTraceID = entryBase + entryCount,
              nextFrameID = frameBase + frameCount,
              traceEntries = IntMap.union newEntries (traceEntries state),
              traceFrames = IntMap.union newFrames (traceFrames state)
            }
    (state', (remapping, globalGroups))

remapOrKeep :: Remapping -> TraceID -> TraceID
remapOrKeep (Remapping traceMap) traceID = do
  if traceID == noTrace
    then noTrace
    else IntMap.findWithDefault traceID (traceIDKey traceID) traceMap
{-# INLINE remapOrKeep #-}

remapOrDrop :: Remapping -> TraceID -> TraceID
remapOrDrop (Remapping traceMap) traceID = do
  if traceID == noTrace
    then noTrace
    else IntMap.findWithDefault noTrace (traceIDKey traceID) traceMap
{-# INLINE remapOrDrop #-}

collectReachable :: TraceState -> IntSet.IntSet -> IntSet.IntSet -> [Int] -> (IntSet.IntSet, IntSet.IntSet)
collectReachable state seenEntries seenFrames pending = do
  case pending of
    [] -> (seenEntries, seenFrames)
    traceKey' : rest
      | traceKey' == 0 || IntSet.member traceKey' seenEntries ->
          collectReachable state seenEntries seenFrames rest
      | otherwise -> do
          let seenEntries' = IntSet.insert traceKey' seenEntries
          let traceID = TraceID traceKey'
          case lookupTrace traceID (traceEntries state) of
            Nothing ->
              collectReachable state seenEntries' seenFrames rest
            Just (SourceSite _) ->
              collectReachable state seenEntries' seenFrames rest
            Just (UnderFrame frameID inner) -> do
              let frameKey' = frameIDKey frameID
              let seenFrames' = IntSet.insert frameKey' seenFrames
              let callSites = maybe [] (pure . traceIDKey . callSite) $ lookupFrame frameID (traceFrames state)
              collectReachable state seenEntries' seenFrames' $ traceIDKey inner : callSites ++ rest

traceIDKey :: TraceID -> Int
traceIDKey (TraceID rawID) =
  rawID

frameIDKey :: FrameID -> Int
frameIDKey (FrameID rawID) =
  rawID

resolveFrom :: TraceState -> TraceID -> TracePath
resolveFrom state traceID = do
  case lookupTrace traceID (traceEntries state) of
    Nothing ->
      TracePath Nothing []
    Just (SourceSite source) ->
      TracePath (Just source) []
    Just (UnderFrame frameID inner) -> do
      let TracePath source innerFrames = resolveFrom state inner
      case lookupFrame frameID (traceFrames state) of
        Nothing ->
          TracePath source innerFrames
        Just frame ->
          TracePath source (innerFrames ++ [frame])

lookupTrace :: TraceID -> IntMap.IntMap a -> Maybe a
lookupTrace (TraceID rawID) =
  IntMap.lookup rawID

lookupFrame :: FrameID -> IntMap.IntMap a -> Maybe a
lookupFrame (FrameID rawID) =
  IntMap.lookup rawID
