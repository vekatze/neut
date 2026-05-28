module Kernel.Parse.Internal.Handle.UnusedTopLevelName
  ( Handle,
    new,
    clear,
    insert,
    delete,
    deleteMany,
    flushRemarks,
  )
where

import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.List qualified as List
import Data.Text qualified as T
import Kernel.Common.GlobalName qualified as GN
import Language.Common.Availability qualified as AV
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.ModuleID qualified as MID
import Logger.Hint
import Logger.Log qualified as L
import Logger.LogLevel qualified as L

data Handle = Handle
  { unusedTopLevelNameMapRef :: IORef (Map.HashMap DD.DefiniteDescription Hint),
    constructorOwnerMapRef :: IORef (Map.HashMap DD.DefiniteDescription DD.DefiniteDescription)
  }

new :: IO Handle
new = do
  unusedTopLevelNameMapRef <- newIORef Map.empty
  constructorOwnerMapRef <- newIORef Map.empty
  return $ Handle {..}

clear :: Handle -> IO ()
clear h = do
  writeIORef (unusedTopLevelNameMapRef h) Map.empty
  writeIORef (constructorOwnerMapRef h) Map.empty

insert :: Handle -> DD.DefiniteDescription -> Hint -> GN.GlobalName -> IO ()
insert h dd m gn = do
  when (shouldTrack dd) $
    atomicModifyIORef' (unusedTopLevelNameMapRef h) $ \mp ->
      (Map.insert dd m mp, ())
  case gn of
    GN.Data _ consInfoList _ ->
      forM_ consInfoList $ \(consDD, _) ->
        atomicModifyIORef' (constructorOwnerMapRef h) $ \mp ->
          (Map.insert consDD dd mp, ())
    _ ->
      return ()

shouldTrack :: DD.DefiniteDescription -> Bool
shouldTrack dd = do
  let (moduleID, _) = DD.unconsDD dd
  moduleID == MID.Main
    && AV.isRestricted dd
    && DD.localLocator dd /= "main"
    && DD.localLocator dd /= "zen"
    && not (";" `T.isInfixOf` DD.reify dd)
    && not ("#" `T.isInfixOf` DD.reify dd)

delete :: Handle -> DD.DefiniteDescription -> IO ()
delete h dd = do
  constructorOwnerMap <- readIORef (constructorOwnerMapRef h)
  atomicModifyIORef' (unusedTopLevelNameMapRef h) $ \mp -> do
    let mp' =
          Map.delete dd $
            maybe id Map.delete (Map.lookup dd constructorOwnerMap) mp
    (mp', ())

deleteMany :: Handle -> [DD.DefiniteDescription] -> IO ()
deleteMany h =
  mapM_ (delete h)

flushRemarks :: Handle -> IO [L.Log]
flushRemarks h = do
  unusedTopLevelNameMap <- atomicModifyIORef' (unusedTopLevelNameMapRef h) $ \m ->
    (Map.empty, m)
  writeIORef (constructorOwnerMapRef h) Map.empty
  let unusedTopLevelNames = List.sortOn fst $ Map.toList unusedTopLevelNameMap
  return $ flip map unusedTopLevelNames $ \(dd, m) ->
    L.newLog m L.Warning $
      "Defined but not used: `" <> DD.reify dd <> "`"
