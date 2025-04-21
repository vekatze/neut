module Move.Context.App
  ( App,
    runApp,
    runAppInEnv,
    readRef,
    readRefMaybe,
    writeRef,
    readRef',
    writeRef',
    modifyRef',
    getEnv,
  )
where

import Move.Context.App.Internal
import Control.Monad.Reader
import Data.IORef
import Data.Text qualified as T

type App = ReaderT Env IO

runApp :: App a -> IO a
runApp app = do
  newEnv >>= runReaderT app

runAppInEnv :: Env -> App a -> IO a
runAppInEnv env app = do
  runReaderT app env

readRef :: T.Text -> (Env -> Ref a) -> App a
readRef name accessor = do
  mValue <- asks accessor >>= liftIO . readIORef
  case mValue of
    Just a ->
      return a
    Nothing ->
      error $ T.unpack $ "[compiler bug] `" <> name <> "` is uninitialized"

readRefMaybe :: (Env -> Ref a) -> App (Maybe a)
readRefMaybe accessor = do
  mValue <- asks accessor >>= liftIO . readIORef
  case mValue of
    Just a ->
      return (Just a)
    Nothing ->
      return Nothing

writeRef :: (Env -> Ref a) -> a -> App ()
writeRef accessor value = do
  ref <- asks accessor
  liftIO $ writeIORef ref (Just value)

readRef' :: (Env -> IORef a) -> App a
readRef' accessor = do
  asks accessor >>= liftIO . readIORef

writeRef' :: (Env -> IORef a) -> a -> App ()
writeRef' accessor value = do
  ref <- asks accessor
  liftIO $ writeIORef ref value

modifyRef' :: (Env -> IORef a) -> (a -> a) -> App ()
modifyRef' accessor modifier = do
  ref <- asks accessor
  value <- liftIO $ readIORef ref
  liftIO $ writeIORef ref $ modifier value

getEnv :: App Env
getEnv =
  ask
