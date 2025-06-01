module Kernel.Lower.Rule.Cancel (cancel) where

import Data.IntMap qualified as IntMap
import Data.Set qualified as S
import Language.Common.Rule.Ident
import Language.Common.Rule.LowType qualified as LT
import Language.LowComp.Rule.LowComp qualified as LC

type FreeInfo =
  (Int, Ident, LC.FreeID)

data MemOp
  = Alloc Int LC.AllocID
  | Free Int Ident LC.FreeID
  deriving (Show)

data Scenario
  = Simple [MemOp]
  | Complex [MemOp] [Scenario] Scenario
  deriving (Show)

emptyScenario :: Scenario
emptyScenario =
  Simple []

type FreeCanceller =
  S.Set LC.FreeID

type AllocCanceller =
  IntMap.IntMap Ident

type Canceller =
  (FreeCanceller, AllocCanceller)

emptyCanceller :: Canceller
emptyCanceller =
  (S.empty, IntMap.empty)

data Axis = Axis
  { freeCanceller :: FreeCanceller,
    allocCanceller :: AllocCanceller
  }

cancel :: LC.Comp -> LC.Comp
cancel lowComp = do
  let scenario = analyze lowComp
  let (freeCanceller, allocCanceller) = relate scenario
  cancel' (Axis {freeCanceller = freeCanceller, allocCanceller = allocCanceller}) lowComp

analyze :: LC.Comp -> Scenario
analyze lowComp = do
  case lowComp of
    LC.Return _ ->
      emptyScenario
    LC.Let _ op cont -> do
      let scenario = analyze cont
      case op of
        LC.Alloc _ size allocID -> do
          insert (Alloc size allocID) scenario
        _ ->
          scenario
    LC.Cont op cont -> do
      let scenario = analyze cont
      case op of
        LC.Free (LC.VarLocal ptr) size freeID -> do
          insert (Free size ptr freeID) scenario
        _ ->
          scenario
    LC.Switch _ defaultBranch ces (_, _, cont) -> do
      let (_, es) = unzip ces
      let contAllocInfo = analyze cont
      case defaultBranch of
        LC.Unreachable -> do
          let branchMapList = map analyze es
          Complex [] branchMapList contAllocInfo
        _ -> do
          let defaultBranchMap = analyze defaultBranch
          let branchMapList = map analyze es
          Complex [] (defaultBranchMap : branchMapList) contAllocInfo
    LC.TailCall {} ->
      emptyScenario
    LC.Unreachable ->
      emptyScenario
    LC.Phi _ _ ->
      emptyScenario

insert :: MemOp -> Scenario -> Scenario
insert memOp allocInfo =
  case allocInfo of
    Simple memOpList ->
      Simple $ memOp : memOpList
    Complex memOpList scenarioList cont ->
      Complex (memOp : memOpList) scenarioList cont

relate :: Scenario -> Canceller
relate scenario = do
  case scenario of
    Simple memOpList -> do
      relateMemOpList memOpList
    Complex memOpList scenarioList cont ->
      case extractFirstFree memOpList of
        Just ((size, ptr, freeID), suffix) -> do
          let scenario' = Complex suffix scenarioList cont
          case findAlloc size scenario' of
            Just (allocList, scenario'') ->
              newCanceller allocList (size, ptr, freeID) <> relate scenario''
            Nothing ->
              relate scenario'
        Nothing -> do
          mconcat $ map relate $ cont : scenarioList

extractFirstFree :: [MemOp] -> Maybe (FreeInfo, [MemOp])
extractFirstFree memOpList =
  case memOpList of
    [] ->
      Nothing
    Alloc {} : rest ->
      extractFirstFree rest
    Free size ptr freeID : rest ->
      return ((size, ptr, freeID), rest)

relateMemOpList :: [MemOp] -> Canceller
relateMemOpList memOpList =
  case extractFirstFree memOpList of
    Just ((size, ptr, freeID), suffix) ->
      case findAllocInMemOpList size suffix of
        Just (allocID, suffix') -> do
          newCanceller [allocID] (size, ptr, freeID) <> relateMemOpList suffix'
        Nothing ->
          relateMemOpList suffix
    Nothing ->
      emptyCanceller

newCanceller :: [LC.AllocID] -> FreeInfo -> Canceller
newCanceller allocList (_, ptr, freeID) = do
  let freeCanceller = S.singleton freeID
  let allocCanceller = IntMap.fromList $ map (,ptr) allocList
  (freeCanceller, allocCanceller)

findAlloc :: Int -> Scenario -> Maybe ([LC.AllocID], Scenario)
findAlloc size scenario =
  case scenario of
    Simple memOpList -> do
      (allocID, memOpList') <- findAllocInMemOpList size memOpList
      return ([allocID], Simple memOpList')
    Complex memOpList scenarioList cont -> do
      case findAllocInMemOpList size memOpList of
        Just (allocID, memOpList') -> do
          return ([allocID], Complex memOpList' scenarioList cont)
        Nothing -> do
          case distributeMaybe $ map (findAlloc size) scenarioList of
            Just allocScenarioList -> do
              let (allocListList, scenarioList') = unzip allocScenarioList
              return (concat allocListList, Complex memOpList scenarioList' cont)
            Nothing -> do
              (allocList, cont') <- findAlloc size cont
              return (allocList, Complex memOpList scenarioList cont')

distributeMaybe :: [Maybe a] -> Maybe [a]
distributeMaybe xs =
  case xs of
    [] ->
      return []
    my : rest -> do
      y <- my
      rest' <- distributeMaybe rest
      return $ y : rest'

findAllocInMemOpList :: Int -> [MemOp] -> Maybe (LC.AllocID, [MemOp])
findAllocInMemOpList size =
  findAllocInMemOpList' size []

findAllocInMemOpList' :: Int -> [MemOp] -> [MemOp] -> Maybe (LC.AllocID, [MemOp])
findAllocInMemOpList' size prefix memOpList =
  case memOpList of
    [] ->
      Nothing
    Alloc size' allocID : rest
      | size == size' ->
          Just (allocID, reverse prefix ++ rest)
    memOp : rest ->
      findAllocInMemOpList' size (memOp : prefix) rest

cancel' :: Axis -> LC.Comp -> LC.Comp
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
        LC.Free _ _ freeID
          | S.member freeID (freeCanceller ctx) -> do
              cont'
        _ ->
          LC.Cont op cont'
    LC.Switch d defaultBranch ces (phi, label, cont) -> do
      let defaultBranch' = cancel' ctx defaultBranch
      let (cs, es) = unzip ces
      let es' = map (cancel' ctx) es
      let cont' = cancel' ctx cont
      LC.Switch d defaultBranch' (zip cs es') (phi, label, cont')
    LC.TailCall {} ->
      lowComp
    LC.Unreachable ->
      lowComp
    LC.Phi {} ->
      lowComp
