module Kernel.Common.Handle.Global.ImportedTypeDefCache
  ( Handle,
    new,
    getOrInsert,
  )
where

import App.App (App)
import App.Error qualified as E
import Control.Concurrent.MVar
import Control.Monad.Except (catchError, throwError)
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Language.Common.DefiniteDescription qualified as DD
import Language.Comp.Comp qualified as C

data CacheEntry
  = Pending (MVar (Either E.Error [C.CompStmt]))
  | Done [C.CompStmt]

newtype Handle = Handle
  { cacheRef :: IORef (Map.HashMap DD.DefiniteDescription CacheEntry)
  }

new :: IO Handle
new = do
  cacheRef <- newIORef Map.empty
  return $ Handle {..}

getOrInsert :: Handle -> DD.DefiniteDescription -> App [C.CompStmt] -> App [C.CompStmt]
getOrInsert h name action = do
  lookupResult <- liftIO $ do
    resultVar <- newEmptyMVar
    atomicModifyIORef' (cacheRef h) $ \cache ->
      case Map.lookup name cache of
        Just cacheEntry ->
          (cache, Left cacheEntry)
        Nothing ->
          (Map.insert name (Pending resultVar) cache, Right resultVar)
  case lookupResult of
    Left (Done stmtList) ->
      return stmtList
    Left (Pending resultVar) -> do
      result <- liftIO $ readMVar resultVar
      case result of
        Left err ->
          throwError err
        Right stmtList ->
          return stmtList
    Right resultVar -> do
      stmtList <-
        action `catchError` \err -> do
          liftIO $ putMVar resultVar (Left err)
          liftIO $ atomicModifyIORef' (cacheRef h) $ \cache ->
            (Map.delete name cache, ())
          throwError err
      liftIO $ putMVar resultVar (Right stmtList)
      liftIO $ atomicModifyIORef' (cacheRef h) $ \cache ->
        (Map.insert name (Done stmtList) cache, ())
      return stmtList
