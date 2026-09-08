module Kernel.Parse.Internal.Handle.PreDecl
  ( Handle,
    new,
    insert,
    insertExposed,
    lookup,
    lookupMaybe,
    lookupExposed,
  )
where

import App.App (App)
import App.Run (raiseError)
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Language.Common.ExternalName qualified as EN
import Logger.Hint
import Prelude hiding (lookup, read)

data Handle = Handle
  { preDeclEnvRef :: IORef (Map.HashMap EN.ExternalName Hint),
    exposedNameEnvRef :: IORef (Map.HashMap EN.ExternalName Hint)
  }

new :: IO Handle
new = do
  preDeclEnvRef <- newIORef Map.empty
  exposedNameEnvRef <- newIORef Map.empty
  return $ Handle {..}

insert :: Handle -> EN.ExternalName -> Hint -> IO ()
insert h k m =
  modifyIORef' (preDeclEnvRef h) $ Map.insert k m

insertExposed :: Handle -> EN.ExternalName -> Hint -> IO ()
insertExposed h k m =
  modifyIORef' (exposedNameEnvRef h) $ Map.insert k m

lookupMaybe :: Handle -> EN.ExternalName -> IO (Maybe Hint)
lookupMaybe h name = do
  preDeclEnv <- readIORef (preDeclEnvRef h)
  return $ Map.lookup name preDeclEnv

lookupExposed :: Handle -> EN.ExternalName -> IO (Maybe Hint)
lookupExposed h name = do
  exposedNameEnv <- readIORef (exposedNameEnvRef h)
  return $ Map.lookup name exposedNameEnv

lookup :: Handle -> Hint -> EN.ExternalName -> App Hint
lookup h m name = do
  preDeclEnv <- liftIO $ readIORef (preDeclEnvRef h)
  case Map.lookup name preDeclEnv of
    Just typeInfo ->
      return typeInfo
    Nothing -> do
      raiseError m $ "Undeclared function: " <> EN.reify name
