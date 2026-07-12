module Kernel.Parse.Internal.Handle.UnusedTopLevelName
  ( Handle,
    new,
    clear,
    insert,
    recordReference,
    delete,
    deleteMany,
    flushRemarks,
  )
where

import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.List qualified as List
import Data.Set qualified as S
import Data.Text qualified as T
import Kernel.Common.GlobalName qualified as GN
import Kernel.Common.Handle.Global.ModulePath qualified as ModulePath
import Kernel.Common.Module qualified as Module
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
    referenceMapRef :: IORef (Map.HashMap DD.DefiniteDescription (S.Set DD.DefiniteDescription)),
    externalReferenceSetRef :: IORef (S.Set DD.DefiniteDescription)
  }

new :: Module.MainModule -> IO Handle
new mainModule = do
  unusedTopLevelNameMapRef <- newIORef Map.empty
  constructorOwnerMapRef <- newIORef Map.empty
  referenceMapRef <- newIORef Map.empty
  externalReferenceSetRef <- newIORef S.empty
  return $ Handle {..}

clear :: Handle -> IO ()
clear h = do
  writeIORef (unusedTopLevelNameMapRef h) Map.empty
  writeIORef (constructorOwnerMapRef h) Map.empty
  writeIORef (referenceMapRef h) Map.empty
  writeIORef (externalReferenceSetRef h) S.empty

insert :: Handle -> DD.DefiniteDescription -> Hint -> GN.GlobalName -> IO ()
insert h dd m gn = do
  atomicModifyIORef' (unusedTopLevelNameMapRef h) $ \mp ->
    (Map.insert dd m mp, ())
  case gn of
    GN.Data _ consInfoList _ ->
      forM_ consInfoList $ \(consDD, _) ->
        atomicModifyIORef' (constructorOwnerMapRef h) $ \mp ->
          (Map.insert consDD dd mp, ())
    _ ->
      return ()

recordReference :: Handle -> Maybe DD.DefiniteDescription -> DD.DefiniteDescription -> IO ()
recordReference h mSource target = do
  constructorOwnerMap <- readIORef (constructorOwnerMapRef h)
  let targetSet = S.insert target $ maybe S.empty S.singleton $ Map.lookup target constructorOwnerMap
  case mSource of
    Just source ->
      atomicModifyIORef' (referenceMapRef h) $ \mp ->
        (Map.insertWith S.union source targetSet mp, ())
    Nothing ->
      atomicModifyIORef' (externalReferenceSetRef h) $ \s ->
        (S.union targetSet s, ())

shouldReportUnused :: DD.DefiniteDescription -> Bool
shouldReportUnused dd = do
  let (moduleID, _) = DD.unconsDD dd
  moduleID == MID.Main
    && AV.isRestricted dd
    && DD.localLocator dd /= "main"
    && DD.localLocator dd /= "zen"
    && not (isInternalTopLevelName dd)

delete :: Handle -> DD.DefiniteDescription -> IO ()
delete h dd = do
  constructorOwnerMap <- readIORef (constructorOwnerMapRef h)
  atomicModifyIORef' (unusedTopLevelNameMapRef h) $ \mp -> do
    let mp' = Map.delete dd $ maybe id Map.delete (Map.lookup dd constructorOwnerMap) mp
    (mp', ())

deleteMany :: Handle -> [DD.DefiniteDescription] -> IO ()
deleteMany h =
  mapM_ (delete h)

flushRemarks :: ModulePath.ModulePathMap -> Handle -> IO [L.Log]
flushRemarks modulePathMap h = do
  unusedTopLevelNameMap <- readIORef (unusedTopLevelNameMapRef h)
  referenceMap <- readIORef (referenceMapRef h)
  recordedExternalReferenceSet <- readIORef (externalReferenceSetRef h)
  let externalReferenceList = filter (isExternalReference unusedTopLevelNameMap) $ Map.toList referenceMap
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
  Map.HashMap DD.DefiniteDescription (S.Set DD.DefiniteDescription) ->
  S.Set DD.DefiniteDescription ->
  S.Set DD.DefiniteDescription
reachableNames referenceMap =
  reachableNames' referenceMap S.empty . S.toList

reachableNames' ::
  Map.HashMap DD.DefiniteDescription (S.Set DD.DefiniteDescription) ->
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
          let next = S.toList $ Map.findWithDefault S.empty name referenceMap
          reachableNames' referenceMap (S.insert name visited) (next ++ rest)
