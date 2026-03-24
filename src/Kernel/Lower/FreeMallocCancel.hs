module Kernel.Lower.FreeMallocCancel (MatchMode (..), freeMallocCancel) where

import Data.Maybe (isJust, isNothing)
import Gensym.Handle qualified as GensymHandle
import Language.Common.CreateSymbol qualified as CreateSymbol
import Language.Common.Ident
import Language.Common.LowType qualified as LT
import Language.LowComp.LowComp qualified as LC

data MatchMode
  = Exact
  | Compatible

freeMallocCancel :: MatchMode -> GensymHandle.Handle -> LC.Comp -> IO LC.Comp
freeMallocCancel matchMode gensymHandle lowComp =
  case lowComp of
    LC.Return {} ->
      pure lowComp
    LC.ReturnVoid ->
      pure lowComp
    LC.Let x op cont -> do
      LC.Let x op <$> freeMallocCancel matchMode gensymHandle cont
    LC.Cont op cont -> do
      case op of
        LC.Free (LC.VarLocal ptr) size _ ->
          case consumeAlloc matchMode ptr size cont of
            Just cont' ->
              freeMallocCancel matchMode gensymHandle cont'
            Nothing ->
              LC.Cont op <$> freeMallocCancel matchMode gensymHandle cont
        _ ->
          LC.Cont op <$> freeMallocCancel matchMode gensymHandle cont
    LC.Switch d t defaultBranch ces phiTargets cont -> do
      let (cs, es) = unzip ces
      result <- liftAllocIntoPhi matchMode gensymHandle phiTargets defaultBranch es cont
      defaultBranch' <- freeMallocCancel matchMode gensymHandle (liftDefaultBranch result)
      es' <- traverse (freeMallocCancel matchMode gensymHandle) (liftBranches result)
      cont' <- freeMallocCancel matchMode gensymHandle (liftCont result)
      pure $ LC.Switch d t defaultBranch' (zip cs es') (liftPhiTargets result) cont'
    LC.TailCall {} ->
      pure lowComp
    LC.Unreachable ->
      pure lowComp
    LC.Phi {} ->
      pure lowComp

consumeAlloc :: MatchMode -> Ident -> Int -> LC.Comp -> Maybe LC.Comp
consumeAlloc matchMode ptr size lowComp =
  case lowComp of
    LC.Return {} ->
      Nothing
    LC.ReturnVoid ->
      Nothing
    LC.Let x op cont ->
      case op of
        LC.Alloc (Left knownSize) _
          | sizeMatches matchMode size (fromInteger knownSize) ->
              Just $ LC.Let x (LC.Bitcast (LC.VarLocal ptr) LT.Pointer LT.Pointer) cont
        _ -> do
          cont' <- consumeAlloc matchMode ptr size cont
          Just $ LC.Let x op cont'
    LC.Cont op cont -> do
      cont' <- consumeAlloc matchMode ptr size cont
      Just $ LC.Cont op cont'
    LC.Switch d t defaultBranch ces phiTargets cont ->
      case consumeAllocAcrossBranches matchMode ptr size (defaultBranch : map snd ces) of
        Just (defaultBranch' : es') ->
          Just $ LC.Switch d t defaultBranch' (zip (map fst ces) es') phiTargets cont
        _ -> do
          cont' <- consumeAlloc matchMode ptr size cont
          Just $ LC.Switch d t defaultBranch ces phiTargets cont'
    LC.TailCall {} ->
      Nothing
    LC.Unreachable ->
      Nothing
    LC.Phi {} ->
      Nothing

consumeAllocAcrossBranches :: MatchMode -> Ident -> Int -> [LC.Comp] -> Maybe [LC.Comp]
consumeAllocAcrossBranches matchMode ptr size branches = do
  branchResults <- traverse (consumeAllocInBranch matchMode ptr size) branches
  let reachableBranches = [branchComp | ReachableBranch branchComp <- branchResults]
  if null reachableBranches
    then Nothing
    else Just $ map branchAllocComp branchResults

data BranchAlloc
  = DeadBranch LC.Comp
  | ReachableBranch LC.Comp

consumeAllocInBranch :: MatchMode -> Ident -> Int -> LC.Comp -> Maybe BranchAlloc
consumeAllocInBranch matchMode ptr size lowComp =
  case lowComp of
    LC.Unreachable ->
      Just $ DeadBranch LC.Unreachable
    _ -> do
      lowComp' <- consumeAlloc matchMode ptr size lowComp
      Just $ ReachableBranch lowComp'

branchAllocComp :: BranchAlloc -> LC.Comp
branchAllocComp branchAlloc =
  case branchAlloc of
    DeadBranch lowComp ->
      lowComp
    ReachableBranch lowComp ->
      lowComp

data LiftResult = LiftResult
  { liftPhiTargets :: [Ident],
    liftDefaultBranch :: LC.Comp,
    liftBranches :: [LC.Comp],
    liftCont :: LC.Comp
  }

liftAllocIntoPhi :: MatchMode -> GensymHandle.Handle -> [Ident] -> LC.Comp -> [LC.Comp] -> LC.Comp -> IO LiftResult
liftAllocIntoPhi matchMode gensymHandle phiTargets defaultBranch branches cont =
  case cont of
    LC.Let x allocOp@(LC.Alloc (Left knownSize) _) cont1 -> do
      capturedBranches <- captureAndAppendAcrossBranches matchMode gensymHandle (fromInteger knownSize) (defaultBranch : branches)
      case capturedBranches of
        Just (defaultBranch' : branches') -> do
          liftAllocIntoPhi matchMode gensymHandle (phiTargets ++ [x]) defaultBranch' branches' cont1
        _ -> do
          result <- liftAllocIntoPhi matchMode gensymHandle phiTargets defaultBranch branches cont1
          pure $ result {liftCont = LC.Let x allocOp (liftCont result)}
    LC.Let x op cont1 -> do
      result <- liftAllocIntoPhi matchMode gensymHandle phiTargets defaultBranch branches cont1
      pure $ result {liftCont = LC.Let x op (liftCont result)}
    LC.Cont op cont1 -> do
      result <- liftAllocIntoPhi matchMode gensymHandle phiTargets defaultBranch branches cont1
      pure $ result {liftCont = LC.Cont op (liftCont result)}
    _ ->
      pure $
        LiftResult
          { liftPhiTargets = phiTargets,
            liftDefaultBranch = defaultBranch,
            liftBranches = branches,
            liftCont = cont
          }

captureAndAppendAcrossBranches :: MatchMode -> GensymHandle.Handle -> Int -> [LC.Comp] -> IO (Maybe [LC.Comp])
captureAndAppendAcrossBranches matchMode gensymHandle size branches = do
  captures <- traverse (captureFirstFree matchMode gensymHandle size) branches
  case sequence captures of
    Just captureList
      | hasReachableCapture captureList ->
          pure $ appendCapturedValues captureList
    _ ->
      pure Nothing

data Capture = Capture
  { captureComp :: LC.Comp,
    captureValue :: Maybe LC.Value,
    captureReachable :: Bool
  }

captureFirstFree :: MatchMode -> GensymHandle.Handle -> Int -> LC.Comp -> IO (Maybe Capture)
captureFirstFree matchMode gensymHandle size lowComp =
  case lowComp of
    LC.Unreachable ->
      pure $ Just $ deadCapture LC.Unreachable
    LC.Return {} ->
      pure $ Just $ pendingCapture lowComp
    LC.ReturnVoid ->
      pure $ Just $ pendingCapture lowComp
    LC.Phi {} ->
      pure $ Just $ pendingCapture lowComp
    LC.TailCall {} ->
      pure $ Just $ pendingCapture lowComp
    LC.Let x op cont -> do
      fmap (mapCaptureComp (LC.Let x op)) <$> captureFirstFree matchMode gensymHandle size cont
    LC.Cont op cont ->
      case op of
        LC.Free (LC.VarLocal ptr) size' _
          | sizeMatches matchMode size' size ->
              pure $ Just $ foundCapture cont (LC.VarLocal ptr)
        _ ->
          fmap (mapCaptureComp (LC.Cont op)) <$> captureFirstFree matchMode gensymHandle size cont
    LC.Switch d t defaultBranch ces phiTargets cont -> do
      let (cs, es) = unzip ces
      defaultCapture <- captureFirstFree matchMode gensymHandle size defaultBranch
      branchCaptures <- traverse (captureFirstFree matchMode gensymHandle size) es
      case sequence (defaultCapture : branchCaptures) of
        Just captureList -> do
          let reachableCaptures = filter captureReachable captureList
          case () of
            _
              | null reachableCaptures ->
                  pure Nothing
              | all (isJust . captureValue) reachableCaptures -> do
                  phiTarget <- freshLiftedPhiTarget gensymHandle
                  case appendCapturedValues captureList of
                    Just (defaultBranch' : es') -> do
                      let switch' = LC.Switch d t defaultBranch' (zip cs es') (phiTargets ++ [phiTarget]) cont
                      pure $ Just $ foundCapture switch' (LC.VarLocal phiTarget)
                    _ ->
                      pure Nothing
              | all (isNothing . captureValue) reachableCaptures -> do
                  contCapture <- captureFirstFree matchMode gensymHandle size cont
                  case (contCapture, map captureComp captureList) of
                    (Just capture, defaultBranch' : es') -> do
                      let wrap = LC.Switch d t defaultBranch' (zip cs es') phiTargets
                      pure $ Just $ mapCaptureComp wrap capture
                    _ ->
                      pure Nothing
              | otherwise ->
                  pure Nothing
        Nothing ->
          pure Nothing

appendPhiValue :: LC.Value -> LC.Comp -> Maybe LC.Comp
appendPhiValue value lowComp =
  case lowComp of
    LC.Let x op cont ->
      LC.Let x op <$> appendPhiValue value cont
    LC.Cont op cont ->
      LC.Cont op <$> appendPhiValue value cont
    LC.Switch d t defaultBranch ces phiTargets cont ->
      LC.Switch d t defaultBranch ces phiTargets <$> appendPhiValue value cont
    LC.Phi values ->
      Just $ LC.Phi (values ++ [value])
    _ ->
      Nothing

appendCapturedValue :: Capture -> Maybe LC.Comp
appendCapturedValue capture =
  case capture of
    Capture {captureReachable = False} ->
      Just $ captureComp capture
    Capture {captureValue = Just value} ->
      appendPhiValue value (captureComp capture)
    Capture {} ->
      Nothing

appendCapturedValues :: [Capture] -> Maybe [LC.Comp]
appendCapturedValues =
  traverse appendCapturedValue

pendingCapture :: LC.Comp -> Capture
pendingCapture lowComp =
  Capture
    { captureComp = lowComp,
      captureValue = Nothing,
      captureReachable = True
    }

deadCapture :: LC.Comp -> Capture
deadCapture lowComp =
  Capture
    { captureComp = lowComp,
      captureValue = Nothing,
      captureReachable = False
    }

foundCapture :: LC.Comp -> LC.Value -> Capture
foundCapture lowComp value =
  Capture
    { captureComp = lowComp,
      captureValue = Just value,
      captureReachable = True
    }

mapCaptureComp :: (LC.Comp -> LC.Comp) -> Capture -> Capture
mapCaptureComp f capture =
  capture {captureComp = f (captureComp capture)}

hasReachableCapture :: [Capture] -> Bool
hasReachableCapture =
  any captureReachable

sizeMatches :: MatchMode -> Int -> Int -> Bool
sizeMatches matchMode freeSize allocSize =
  case matchMode of
    Exact ->
      freeSize == allocSize
    Compatible ->
      freeSize >= allocSize

freshLiftedPhiTarget :: GensymHandle.Handle -> IO Ident
freshLiftedPhiTarget gensymHandle =
  CreateSymbol.newIdentFromText gensymHandle "free-malloc-phi"
