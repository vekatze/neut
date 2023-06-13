module Scene.Cancel (cancel) where

import Data.IORef
import Data.IntMap qualified as IntMap
import Data.Set qualified as S
import Entity.Ident
import Entity.Ident.Reify
import Entity.LowComp qualified as LC
import Entity.LowType qualified as LT

newtype Ctx = Ctx
  {constraints :: IORef [Constraint]}

type FreeInfo =
  (Int, Ident)

type AllocInfo = (Int, LC.AllocID)

data AllocScenario
  = Simple [AllocInfo]
  | Complex [AllocInfo] [AllocScenario] AllocScenario
  deriving (Show)

type Constraint =
  (FreeInfo, AllocScenario)

type FreeCanceller =
  S.Set Int

type AllocCanceller =
  IntMap.IntMap Ident

type Canceller =
  (FreeCanceller, AllocCanceller)

data CancelCtx = CancelCtx
  { freeCanceller :: FreeCanceller,
    allocCanceller :: AllocCanceller
  }

cancel :: LC.Comp -> IO LC.Comp
cancel lowComp = do
  emptyConstraints <- newIORef []
  let ctx = Ctx {constraints = emptyConstraints}
  _ <- analyze ctx lowComp
  cs <- readIORef (constraints ctx)
  let (freeCanceller, allocCanceller) = synthesize cs
  let cancelCtx =
        CancelCtx
          { freeCanceller = freeCanceller,
            allocCanceller = allocCanceller
          }
  return $ cancel' cancelCtx lowComp

analyze :: Ctx -> LC.Comp -> IO AllocScenario
analyze ctx lowComp = do
  case lowComp of
    LC.Return _ ->
      return emptyInfo
    LC.Let _ op cont -> do
      info <- analyze ctx cont
      case op of
        LC.Alloc _ size allocID -> do
          return $ insert (size, allocID) info
        _ ->
          return info
    LC.Cont op cont -> do
      willAllocInfo <- analyze ctx cont
      case op of
        LC.Free (LC.VarLocal ptr) size -> do
          modifyIORef' (constraints ctx) $ (:) ((size, ptr), willAllocInfo)
        _ ->
          return ()
      return willAllocInfo
    LC.Switch _ defaultBranch les (_, cont) -> do
      let (_, es) = unzip les
      contAllocInfo <- analyze ctx cont
      case defaultBranch of
        LC.Unreachable -> do
          branchMapList <- mapM (analyze ctx) es
          return $ Complex [] branchMapList contAllocInfo
        _ -> do
          defaultBranchMap <- analyze ctx defaultBranch
          branchMapList <- mapM (analyze ctx) es
          return $ Complex [] (defaultBranchMap : branchMapList) contAllocInfo
    LC.TailCall {} ->
      return emptyInfo
    LC.Unreachable ->
      return emptyInfo

insert :: AllocInfo -> AllocScenario -> AllocScenario
insert varInfo allocInfo =
  case allocInfo of
    Simple varInfoList ->
      Simple $ varInfo : varInfoList
    Complex varInfoList foo bar ->
      Complex (varInfo : varInfoList) foo bar

emptyInfo :: AllocScenario
emptyInfo =
  Simple []

synthesize :: [Constraint] -> Canceller
synthesize cs =
  case cs of
    [] ->
      (S.empty, IntMap.empty)
    c : rest -> do
      case resolve c of
        Just (freeCanceller1, allocCanceller1) -> do
          let allocIDList = IntMap.keys allocCanceller1
          let rest' = map (eraseUsedAllocIDs allocIDList) rest
          let (freeCanceller2, allocCanceller2) = synthesize rest'
          (S.union freeCanceller1 freeCanceller2, IntMap.union allocCanceller1 allocCanceller2)
        Nothing ->
          synthesize rest

eraseUsedAllocIDs :: [LC.AllocID] -> Constraint -> Constraint
eraseUsedAllocIDs idList (freeInfo, c) =
  (freeInfo, eraseUsedAllocIDs' idList c)

eraseUsedAllocIDs' :: [LC.AllocID] -> AllocScenario -> AllocScenario
eraseUsedAllocIDs' idList c =
  case idList of
    [] ->
      c
    i : rest ->
      eraseUsedAllocIDs' rest $ eraseID i c

eraseID :: LC.AllocID -> AllocScenario -> AllocScenario
eraseID i c =
  case c of
    Simple xs ->
      Simple $ filter (\(_, j) -> i /= j) xs
    Complex pre mid post -> do
      let pre' = filter (\(_, j) -> i /= j) pre
      let mid' = map (eraseID i) mid
      let post' = eraseID i post
      Complex pre' mid' post'

resolve :: Constraint -> Maybe Canceller
resolve (freeInfo@(size, ptr), allocInfo) =
  case allocInfo of
    Simple sizeAllocList -> do
      allocID <- lookup size sizeAllocList
      return (S.singleton (toInt ptr), IntMap.singleton allocID ptr)
    Complex varInfoList branchVarInfoList confluenceVarInfoList ->
      case resolveAll freeInfo branchVarInfoList of
        Just result ->
          return result
        Nothing ->
          case resolve (freeInfo, Simple varInfoList) of
            Just result ->
              return result
            Nothing -> do
              resolve (freeInfo, confluenceVarInfoList)

resolveAll :: FreeInfo -> [AllocScenario] -> Maybe Canceller
resolveAll freeInfo cs =
  case cs of
    [] ->
      return (S.empty, IntMap.empty)
    c : rest -> do
      (freeCanceller1, allocCanceller1) <- resolve (freeInfo, c)
      (freeCanceller2, allocCanceller2) <- resolveAll freeInfo rest
      return (S.union freeCanceller1 freeCanceller2, IntMap.union allocCanceller1 allocCanceller2)

cancel' :: CancelCtx -> LC.Comp -> LC.Comp
cancel' ctx lowComp =
  case lowComp of
    LC.Return {} ->
      lowComp
    LC.Let x op cont -> do
      let cont' = cancel' ctx cont
      case op of
        LC.Alloc _ _ allocID
          | Just ptr <- IntMap.lookup allocID (allocCanceller ctx) -> do
              LC.Let x (LC.Bitcast (LC.VarLocal ptr) LT.Pointer LT.Pointer) cont'
        _ ->
          LC.Let x op cont'
    LC.Cont op cont -> do
      let cont' = cancel' ctx cont
      case op of
        LC.Free (LC.VarLocal ptr) _
          | S.member (toInt ptr) (freeCanceller ctx) -> do
              cont'
        _ ->
          LC.Cont op cont'
    LC.Switch d defaultBranch les (phi, cont) -> do
      let defaultBranch' = cancel' ctx defaultBranch
      let (ls, es) = unzip les
      let es' = map (cancel' ctx) es
      let cont' = cancel' ctx cont
      LC.Switch d defaultBranch' (zip ls es') (phi, cont')
    LC.TailCall {} ->
      lowComp
    LC.Unreachable ->
      lowComp
