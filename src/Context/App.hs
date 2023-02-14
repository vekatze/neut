module Context.App
  ( App,
    runApp,
    readRef,
    writeRef,
    readRef',
    writeRef',
    modifyRef',
  )
where

import Context.App.Internal
import Control.Monad.Reader
import Data.IORef
import Data.Text qualified as T

type App = ReaderT Env IO

runApp :: App a -> IO a
runApp app = do
  newEnv >>= runReaderT app

readRef :: T.Text -> (Env -> Ref a) -> App a
readRef name accessor = do
  mValue <- asks accessor >>= liftIO . readIORef
  case mValue of
    Just a ->
      return a
    Nothing ->
      error $ T.unpack $ "[compiler bug] `" <> name <> "` is uninitialized"

writeRef :: (Env -> Ref a) -> a -> App ()
writeRef accessor value = do
  ref <- asks accessor
  liftIO $ writeIORef ref (Just value)

-- execute :: IO ()
-- execute = do
--   runApp $ do
--     c <- OptParse.parseCommand
--     Throw.run $ do
--       case c of
--         C.Build cfg -> do
--           Build.build cfg
--         C.Check cfg -> do
--           Check.check cfg
--         C.Clean cfg ->
--           Clean.clean cfg
--         C.Release cfg ->
--           Release.release cfg
--         C.Create cfg ->
--           Create.create cfg
--         C.Get cfg ->
--           Get.get cfg
--         C.Tidy cfg ->
--           Tidy.tidy cfg
--         C.ShowVersion cfg ->
--           Version.showVersion cfg

readRef' :: (Env -> FastRef a) -> App a
readRef' accessor = do
  asks accessor >>= liftIO . readIORef

writeRef' :: (Env -> FastRef a) -> a -> App ()
writeRef' accessor value = do
  ref <- asks accessor
  liftIO $ writeIORef ref value

modifyRef' :: (Env -> FastRef a) -> (a -> a) -> App ()
modifyRef' accessor modifier = do
  ref <- asks accessor
  value <- liftIO $ readIORef ref
  liftIO $ writeIORef ref $ modifier value
