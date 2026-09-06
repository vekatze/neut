module Kernel.Lower.FreeMallocCancel (MatchMode (..), freeMallocCancel) where

import Control.Monad
import Data.IORef
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Gensym.Handle qualified as GensymHandle
import Language.Common.CreateSymbol qualified as CreateSymbol
import Language.Common.Ident
import Language.Common.LowType qualified as LT
import Language.LowComp.LowComp qualified as LC

data MatchMode
  = Exact
  | Compatible

data AllocState
  = Live
  | Reused LC.Value
  | Lifted

data Item
  = PlainLet Ident LC.Op
  | PlainCont LC.Op
  | AllocItem Ident LC.Op Int (IORef AllocState)
  | FreeItem Ident Int LC.Op (IORef Bool)
  | SwitchItem Switch

data Switch = Switch
  { switchValue :: LC.Value,
    switchType :: LT.LowType,
    switchDefault :: Segment,
    switchCases :: [(Integer, Segment)],
    switchPhiTargets :: IORef [(Ident, LT.LowType)]
  }

data Terminal
  = TerminalReturn LC.Value
  | TerminalTailCall Bool LT.LowType LC.Value [(LT.LowType, LC.Value)]
  | TerminalUnreachable
  | TerminalPhi (IORef [LC.Value])

data Segment = Segment
  { segmentItems :: IntMap.IntMap Item,
    segmentSize :: Int,
    segmentTerminal :: Terminal,
    liveAllocs :: IORef (Map.Map Int IntSet.IntSet),
    liveFrees :: IORef (Map.Map Int IntSet.IntSet),
    switchPositions :: IntSet.IntSet,
    failedConsumptions :: IORef (IntMap.IntMap (Set.Set Int))
  }

freeMallocCancel :: MatchMode -> GensymHandle.Handle -> LC.Comp -> IO LC.Comp
freeMallocCancel matchMode gensymHandle lowComp = do
  segment <- toSegment lowComp
  process matchMode gensymHandle segment
  fromSegment segment

toSegment :: LC.Comp -> IO Segment
toSegment lowComp = do
  (items, terminal) <- collectItems 0 [] lowComp
  let itemMap = IntMap.fromList items
  let allocs = Map.fromListWith IntSet.union [(size, IntSet.singleton pos) | (pos, AllocItem _ _ size _) <- items]
  let frees = Map.fromListWith IntSet.union [(size, IntSet.singleton pos) | (pos, FreeItem _ size _ _) <- items]
  liveAllocs <- newIORef allocs
  liveFrees <- newIORef frees
  failedConsumptions <- newIORef IntMap.empty
  return $
    Segment
      { segmentItems = itemMap,
        segmentSize = length items,
        segmentTerminal = terminal,
        liveAllocs = liveAllocs,
        liveFrees = liveFrees,
        switchPositions = IntSet.fromList [pos | (pos, SwitchItem _) <- items],
        failedConsumptions = failedConsumptions
      }

collectItems :: Int -> [(Int, Item)] -> LC.Comp -> IO ([(Int, Item)], Terminal)
collectItems pos acc lowComp =
  case lowComp of
    LC.Return v ->
      return (reverse acc, TerminalReturn v)
    LC.Let x op cont ->
      case op of
        LC.Alloc (Left knownSize) _ -> do
          stateRef <- newIORef Live
          collectItems (pos + 1) ((pos, AllocItem x op (fromInteger knownSize) stateRef) : acc) cont
        _ ->
          collectItems (pos + 1) ((pos, PlainLet x op) : acc) cont
    LC.Cont op cont ->
      case op of
        LC.Free (LC.VarLocal ptr) (Just size) _ -> do
          droppedRef <- newIORef False
          collectItems (pos + 1) ((pos, FreeItem ptr size op droppedRef) : acc) cont
        _ ->
          collectItems (pos + 1) ((pos, PlainCont op) : acc) cont
    LC.Switch d t defaultBranch ces phiTargets cont -> do
      defaultSegment <- toSegment defaultBranch
      caseSegments <- forM ces $ \(c, e) -> do
        e' <- toSegment e
        return (c, e')
      phiTargetsRef <- newIORef phiTargets
      let switch =
            Switch
              { switchValue = d,
                switchType = t,
                switchDefault = defaultSegment,
                switchCases = caseSegments,
                switchPhiTargets = phiTargetsRef
              }
      collectItems (pos + 1) ((pos, SwitchItem switch) : acc) cont
    LC.TailCall mustTail t f args ->
      return (reverse acc, TerminalTailCall mustTail t f args)
    LC.Unreachable ->
      return (reverse acc, TerminalUnreachable)
    LC.Phi values -> do
      valuesRef <- newIORef values
      return (reverse acc, TerminalPhi valuesRef)

fromSegment :: Segment -> IO LC.Comp
fromSegment segment = do
  terminal <- case segmentTerminal segment of
    TerminalReturn v ->
      return $ LC.Return v
    TerminalTailCall mustTail t f args ->
      return $ LC.TailCall mustTail t f args
    TerminalUnreachable ->
      return LC.Unreachable
    TerminalPhi valuesRef ->
      LC.Phi <$> readIORef valuesRef
  foldM prependItem terminal (reverse (IntMap.elems (segmentItems segment)))

prependItem :: LC.Comp -> Item -> IO LC.Comp
prependItem cont item =
  case item of
    PlainLet x op ->
      return $ LC.Let x op cont
    PlainCont op ->
      return $ LC.Cont op cont
    AllocItem x op _ stateRef -> do
      state <- readIORef stateRef
      case state of
        Live ->
          return $ LC.Let x op cont
        Reused v ->
          return $ LC.Let x (LC.Bitcast v LT.Pointer LT.Pointer) cont
        Lifted ->
          return cont
    FreeItem _ _ op droppedRef -> do
      dropped <- readIORef droppedRef
      if dropped
        then return cont
        else return $ LC.Cont op cont
    SwitchItem switch -> do
      defaultBranch <- fromSegment (switchDefault switch)
      ces <- forM (switchCases switch) $ \(c, e) -> do
        e' <- fromSegment e
        return (c, e')
      phiTargets <- readIORef (switchPhiTargets switch)
      return $ LC.Switch (switchValue switch) (switchType switch) defaultBranch ces phiTargets cont

process :: MatchMode -> GensymHandle.Handle -> Segment -> IO ()
process matchMode gensymHandle segment =
  forM_ (IntMap.toAscList (segmentItems segment)) $ \(pos, item) ->
    case item of
      FreeItem ptr size _ droppedRef -> do
        dropped <- readIORef droppedRef
        unless dropped $ do
          consumption <- findConsumption matchMode segment (pos + 1) ptr size
          case consumption of
            Just commit -> do
              commit
              dropFree segment pos size droppedRef
            Nothing ->
              return ()
      SwitchItem switch -> do
        liftAllocsIntoPhi matchMode gensymHandle segment (pos + 1) switch
        forM_ (branchesOf switch) $ process matchMode gensymHandle
      _ ->
        return ()

branchesOf :: Switch -> [Segment]
branchesOf switch =
  switchDefault switch : map snd (switchCases switch)

sizeMatches :: MatchMode -> Int -> Int -> Bool
sizeMatches matchMode freeSize allocSize =
  case matchMode of
    Exact ->
      freeSize == allocSize
    Compatible ->
      freeSize >= allocSize

firstFittingAlloc :: MatchMode -> Map.Map Int IntSet.IntSet -> Int -> Int -> Maybe Int
firstFittingAlloc matchMode allocs start freeSize =
  minimumMaybe [pos | (allocSize, positions) <- Map.toList allocs, sizeMatches matchMode freeSize allocSize, Just pos <- [IntSet.lookupGE start positions]]

firstFittingFree :: MatchMode -> Map.Map Int IntSet.IntSet -> Int -> Maybe Int
firstFittingFree matchMode frees allocSize =
  minimumMaybe [pos | (freeSize, positions) <- Map.toList frees, sizeMatches matchMode freeSize allocSize, Just pos <- [IntSet.lookupGE 0 positions]]

minimumMaybe :: [Int] -> Maybe Int
minimumMaybe xs =
  case xs of
    [] ->
      Nothing
    _ ->
      Just (minimum xs)

switchesBetween :: Segment -> Int -> Int -> [Int]
switchesBetween segment start limit =
  IntSet.toAscList $ fst $ IntSet.split limit $ snd $ IntSet.split (start - 1) (switchPositions segment)

switchAt :: Segment -> Int -> Switch
switchAt segment pos =
  case IntMap.lookup pos (segmentItems segment) of
    Just (SwitchItem switch) ->
      switch
    _ ->
      error "Kernel.Lower.FreeMallocCancel.switchAt"

findConsumption :: MatchMode -> Segment -> Int -> Ident -> Int -> IO (Maybe (IO ()))
findConsumption matchMode segment start ptr freeSize = do
  allocs <- readIORef (liveAllocs segment)
  let candidate = firstFittingAlloc matchMode allocs start freeSize
  let limit = maybe (segmentSize segment) id candidate
  viaSwitch <- findConsumptionViaSwitches matchMode segment (switchesBetween segment start limit) ptr freeSize
  case viaSwitch of
    Just commit ->
      return $ Just commit
    Nothing ->
      case candidate of
        Just pos -> do
          return $ Just $ reuseAlloc segment pos (LC.VarLocal ptr)
        Nothing ->
          return Nothing

findConsumptionViaSwitches :: MatchMode -> Segment -> [Int] -> Ident -> Int -> IO (Maybe (IO ()))
findConsumptionViaSwitches matchMode segment positions ptr freeSize =
  case positions of
    [] ->
      return Nothing
    pos : rest -> do
      failed <- readIORef (failedConsumptions segment)
      let knownToFail = case IntMap.lookup pos failed of
            Just failedSizes ->
              case matchMode of
                Exact ->
                  Set.member freeSize failedSizes
                Compatible ->
                  maybe False (const True) (Set.lookupGE freeSize failedSizes)
            Nothing ->
              False
      if knownToFail
        then findConsumptionViaSwitches matchMode segment rest ptr freeSize
        else do
          result <- findConsumptionInBranches matchMode (switchAt segment pos) ptr freeSize
          case result of
            Just commit ->
              return $ Just commit
            Nothing -> do
              modifyIORef' (failedConsumptions segment) $ IntMap.insertWith Set.union pos (Set.singleton freeSize)
              findConsumptionViaSwitches matchMode segment rest ptr freeSize

findConsumptionInBranches :: MatchMode -> Switch -> Ident -> Int -> IO (Maybe (IO ()))
findConsumptionInBranches matchMode switch ptr freeSize = do
  results <- forM (branchesOf switch) $ \branch ->
    if isDeadBranch branch
      then return $ Just Nothing
      else fmap Just <$> findConsumption matchMode branch 0 ptr freeSize
  case sequence results of
    Just commits
      | any isJustCommit commits ->
          return $ Just $ sequence_ [commit | Just commit <- commits]
    _ ->
      return Nothing

isJustCommit :: Maybe a -> Bool
isJustCommit m =
  case m of
    Just _ ->
      True
    Nothing ->
      False

isDeadBranch :: Segment -> Bool
isDeadBranch segment =
  case (segmentSize segment, segmentTerminal segment) of
    (0, TerminalUnreachable) ->
      True
    _ ->
      False

reuseAlloc :: Segment -> Int -> LC.Value -> IO ()
reuseAlloc segment pos value =
  case IntMap.lookup pos (segmentItems segment) of
    Just (AllocItem _ _ size stateRef) -> do
      writeIORef stateRef (Reused value)
      removePosition (liveAllocs segment) size pos
    _ ->
      error "Kernel.Lower.FreeMallocCancel.reuseAlloc"

liftAlloc :: Segment -> Int -> IO ()
liftAlloc segment pos =
  case IntMap.lookup pos (segmentItems segment) of
    Just (AllocItem _ _ size stateRef) -> do
      writeIORef stateRef Lifted
      removePosition (liveAllocs segment) size pos
    _ ->
      error "Kernel.Lower.FreeMallocCancel.liftAlloc"

dropFree :: Segment -> Int -> Int -> IORef Bool -> IO ()
dropFree segment pos size droppedRef = do
  writeIORef droppedRef True
  removePosition (liveFrees segment) size pos

removePosition :: IORef (Map.Map Int IntSet.IntSet) -> Int -> Int -> IO ()
removePosition ref size pos =
  modifyIORef' ref $ Map.update (nonEmpty . IntSet.delete pos) size

nonEmpty :: IntSet.IntSet -> Maybe IntSet.IntSet
nonEmpty positions =
  if IntSet.null positions
    then Nothing
    else Just positions

liftAllocsIntoPhi :: MatchMode -> GensymHandle.Handle -> Segment -> Int -> Switch -> IO ()
liftAllocsIntoPhi matchMode gensymHandle segment pos switch =
  case IntMap.lookup pos (segmentItems segment) of
    Just (AllocItem x _ size stateRef) -> do
      state <- readIORef stateRef
      case state of
        Live -> do
          captured <- captureAcrossBranches matchMode gensymHandle size (branchesOf switch)
          case captured of
            Just commit -> do
              commit
              liftAlloc segment pos
              modifyIORef' (switchPhiTargets switch) (++ [(x, LT.Pointer)])
            Nothing ->
              return ()
        _ ->
          return ()
      liftAllocsIntoPhi matchMode gensymHandle segment (pos + 1) switch
    Just (SwitchItem _) ->
      return ()
    Just _ ->
      liftAllocsIntoPhi matchMode gensymHandle segment (pos + 1) switch
    Nothing ->
      return ()

data Capture
  = CaptureDead
  | CapturePending
  | CaptureFound (IO ()) LC.Value

captureAcrossBranches :: MatchMode -> GensymHandle.Handle -> Int -> [Segment] -> IO (Maybe (IO ()))
captureAcrossBranches matchMode gensymHandle allocSize branches = do
  captures <- forM branches $ captureFirstFree matchMode gensymHandle allocSize
  case sequence captures of
    Just captureList
      | any isReachableCapture captureList ->
          return $ appendCapturedValues (zip branches captureList)
    _ ->
      return Nothing

isReachableCapture :: Capture -> Bool
isReachableCapture capture =
  case capture of
    CaptureDead ->
      False
    _ ->
      True

appendCapturedValues :: [(Segment, Capture)] -> Maybe (IO ())
appendCapturedValues pairs =
  sequence_ <$> mapM appendCapturedValue pairs

appendCapturedValue :: (Segment, Capture) -> Maybe (IO ())
appendCapturedValue (branch, capture) =
  case capture of
    CaptureDead ->
      Just $ return ()
    CapturePending ->
      Nothing
    CaptureFound commit value ->
      case segmentTerminal branch of
        TerminalPhi valuesRef ->
          Just $ do
            commit
            modifyIORef' valuesRef (++ [value])
        _ ->
          Nothing

captureFirstFree :: MatchMode -> GensymHandle.Handle -> Int -> Segment -> IO (Maybe Capture)
captureFirstFree matchMode gensymHandle allocSize segment = do
  frees <- readIORef (liveFrees segment)
  let candidate = firstFittingFree matchMode frees allocSize
  let limit = maybe (segmentSize segment) id candidate
  captureViaSwitches matchMode gensymHandle allocSize segment (switchesBetween segment 0 limit) candidate

captureViaSwitches :: MatchMode -> GensymHandle.Handle -> Int -> Segment -> [Int] -> Maybe Int -> IO (Maybe Capture)
captureViaSwitches matchMode gensymHandle allocSize segment positions candidate =
  case positions of
    [] ->
      case candidate of
        Just pos ->
          case IntMap.lookup pos (segmentItems segment) of
            Just (FreeItem ptr size _ droppedRef) ->
              return $ Just $ CaptureFound (dropFree segment pos size droppedRef) (LC.VarLocal ptr)
            _ ->
              error "Kernel.Lower.FreeMallocCancel.captureViaSwitches"
        Nothing ->
          case segmentTerminal segment of
            TerminalUnreachable ->
              return $ Just CaptureDead
            _ ->
              return $ Just CapturePending
    pos : rest -> do
      nested <- captureViaSwitch matchMode gensymHandle allocSize (switchAt segment pos)
      case nested of
        NestedFound commit value ->
          return $ Just $ CaptureFound commit value
        NestedContinue ->
          captureViaSwitches matchMode gensymHandle allocSize segment rest candidate
        NestedFailure ->
          return Nothing

data NestedCapture
  = NestedFound (IO ()) LC.Value
  | NestedContinue
  | NestedFailure

captureViaSwitch :: MatchMode -> GensymHandle.Handle -> Int -> Switch -> IO NestedCapture
captureViaSwitch matchMode gensymHandle allocSize switch = do
  let branches = branchesOf switch
  captures <- forM branches $ captureFirstFree matchMode gensymHandle allocSize
  case sequence captures of
    Just captureList -> do
      let reachableCaptures = filter isReachableCapture captureList
      case () of
        _
          | null reachableCaptures ->
              return NestedFailure
          | all isFoundCapture reachableCaptures -> do
              phiTarget <- CreateSymbol.newIdentFromText gensymHandle "free-malloc-phi"
              case appendCapturedValues (zip branches captureList) of
                Just commit ->
                  return $
                    NestedFound
                      (commit >> modifyIORef' (switchPhiTargets switch) (++ [(phiTarget, LT.Pointer)]))
                      (LC.VarLocal phiTarget)
                Nothing ->
                  return NestedFailure
          | all (not . isFoundCapture) reachableCaptures ->
              return NestedContinue
          | otherwise ->
              return NestedFailure
    Nothing ->
      return NestedFailure

isFoundCapture :: Capture -> Bool
isFoundCapture capture =
  case capture of
    CaptureFound {} ->
      True
    _ ->
      False
