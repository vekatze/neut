module Kernel.Parse.Internal.Handle.UnusedTopLevelName
  ( Handle,
    ReferenceLog,
    new,
    clear,
    insertMany,
    newReferenceLog,
    recordReference,
    getReferences,
    addConstructorOwners,
    insertReferences,
    flushRemarks,
  )
where

import Control.Monad (when)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.List qualified as List
import Data.Map.Strict qualified as OrdMap
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Kernel.Common.GlobalName qualified as GN
import Kernel.Common.Handle.Global.ModulePath qualified as ModulePath
import Kernel.Common.HashMapRef (insertEntries)
import Kernel.Common.Module qualified as Module
import Kernel.Common.TopLevelReferences
import Language.Common.Availability qualified as AV
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.ModuleID qualified as MID
import Logger.Hint
import Logger.Log qualified as L
import Logger.LogLevel qualified as L

data Handle = Handle
  { mainModule :: Module.MainModule,
    unusedTopLevelNameMapRef :: IORef (Map.HashMap DD.DefiniteDescription Hint),
    constructorOwnerMapRef :: IORef (Map.HashMap DD.DefiniteDescription DD.DefiniteDescription),
    referencesRef :: IORef TopLevelReferences
  }

newtype ReferenceLog = ReferenceLog (IORef TopLevelReferences)

new :: Module.MainModule -> IO Handle
new mainModule = do
  unusedTopLevelNameMapRef <- newIORef Map.empty
  constructorOwnerMapRef <- newIORef Map.empty
  referencesRef <- newIORef mempty
  return $ Handle {..}

clear :: Handle -> IO ()
clear h = do
  writeIORef (unusedTopLevelNameMapRef h) Map.empty
  writeIORef (constructorOwnerMapRef h) Map.empty
  writeIORef (referencesRef h) mempty

insertMany :: Handle -> [(DD.DefiniteDescription, Hint, GN.GlobalName)] -> IO ()
insertMany h entries = do
  insertEntries (unusedTopLevelNameMapRef h) [(dd, m) | (dd, m, _) <- entries]
  insertEntries (constructorOwnerMapRef h) [(consDD, dd) | (dd, _, GN.Data _ consInfoList _) <- entries, (consDD, _) <- consInfoList]

newReferenceLog :: IO ReferenceLog
newReferenceLog =
  ReferenceLog <$> newIORef mempty

recordReference :: ReferenceLog -> Maybe DD.DefiniteDescription -> DD.DefiniteDescription -> IO ()
recordReference (ReferenceLog referencesRef) mSource target =
  when (isInMainModule target) $
    modifyIORef' referencesRef $ \references ->
      case mSource of
        Just source ->
          references {referenceMap = OrdMap.insertWith S.union source (S.singleton target) (referenceMap references)}
        Nothing ->
          references {externalReferenceSet = S.insert target (externalReferenceSet references)}

getReferences :: ReferenceLog -> IO TopLevelReferences
getReferences (ReferenceLog referencesRef) =
  readIORef referencesRef

addConstructorOwners :: Handle -> TopLevelReferences -> IO TopLevelReferences
addConstructorOwners h references = do
  constructorOwnerMap <- readIORef (constructorOwnerMapRef h)
  let withOwners targets = S.union targets $ S.fromList $ mapMaybe (`Map.lookup` constructorOwnerMap) $ S.toList targets
  return $
    TopLevelReferences
      { referenceMap = OrdMap.map withOwners (referenceMap references),
        externalReferenceSet = withOwners (externalReferenceSet references)
      }

insertReferences :: Handle -> TopLevelReferences -> IO ()
insertReferences h references =
  atomicModifyIORef' (referencesRef h) $ \allReferences ->
    (references <> allReferences, ())

shouldReportUnused :: DD.DefiniteDescription -> Bool
shouldReportUnused dd =
  isInMainModule dd
    && AV.isRestricted dd
    && DD.localLocator dd /= "main"
    && DD.localLocator dd /= "zen"
    && not (isInternalTopLevelName dd)

isInMainModule :: DD.DefiniteDescription -> Bool
isInMainModule dd = do
  let (moduleID, _) = DD.unconsDD dd
  moduleID == MID.Main

flushRemarks :: ModulePath.ModulePathMap -> Handle -> IO [L.Log]
flushRemarks modulePathMap h = do
  unusedTopLevelNameMap <- readIORef (unusedTopLevelNameMapRef h)
  TopLevelReferences {referenceMap, externalReferenceSet = recordedExternalReferenceSet} <- readIORef (referencesRef h)
  let externalReferenceList = filter (isExternalReference unusedTopLevelNameMap) $ OrdMap.toList referenceMap
  let derivedExternalReferenceSet = S.unions $ map snd externalReferenceList
  let externalReferenceSet = S.union recordedExternalReferenceSet derivedExternalReferenceSet
  let reachable = reachableNames referenceMap externalReferenceSet
  let unreachableTopLevelNames = filter (not . (`S.member` reachable) . fst) $ Map.toList unusedTopLevelNameMap
  let unusedTopLevelNames = List.sort $ filter (shouldReportUnused . fst) unreachableTopLevelNames
  return $ flip map unusedTopLevelNames $ \(dd, m) ->
    L.newLog m L.Warning $
      "Defined but not used: `" <> ModulePath.renderDD modulePathMap dd <> "`"

isExternalReference ::
  Map.HashMap DD.DefiniteDescription Hint ->
  (DD.DefiniteDescription, S.Set DD.DefiniteDescription) ->
  Bool
isExternalReference unusedTopLevelNameMap (source, _) =
  case Map.lookup source unusedTopLevelNameMap of
    Nothing ->
      not $ isInternalTopLevelName source
    Just _ ->
      not (shouldReportUnused source) && not (isInternalTopLevelName source)

isInternalTopLevelName :: DD.DefiniteDescription -> Bool
isInternalTopLevelName dd =
  ";" `T.isInfixOf` DD.reify dd || "#" `T.isInfixOf` DD.reify dd

reachableNames ::
  OrdMap.Map DD.DefiniteDescription (S.Set DD.DefiniteDescription) ->
  S.Set DD.DefiniteDescription ->
  S.Set DD.DefiniteDescription
reachableNames referenceMap =
  reachableNames' referenceMap S.empty . S.toList

reachableNames' ::
  OrdMap.Map DD.DefiniteDescription (S.Set DD.DefiniteDescription) ->
  S.Set DD.DefiniteDescription ->
  [DD.DefiniteDescription] ->
  S.Set DD.DefiniteDescription
reachableNames' referenceMap visited names =
  case names of
    [] ->
      visited
    name : rest -> do
      if S.member name visited
        then reachableNames' referenceMap visited rest
        else do
          let next = S.toList $ OrdMap.findWithDefault S.empty name referenceMap
          reachableNames' referenceMap (S.insert name visited) (next ++ rest)
