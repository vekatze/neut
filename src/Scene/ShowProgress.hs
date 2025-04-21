module Scene.ShowProgress
  ( Handle,
    new,
    increment,
    close,
  )
where

import Context.App
import Context.Color qualified as Color
import Context.Env (getSilentMode)
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Rule.Log qualified as L
import Rule.ProgressBar (Frame, ProgressBar (..), next, renderFinished, renderInProgress)
import System.Console.ANSI
import System.IO hiding (Handle)
import UnliftIO.Async
import UnliftIO.Concurrent (threadDelay)

type Handle =
  Maybe InnerHandle

data InnerHandle
  = Handle
  { progressBarRef :: IORef ProgressBar,
    renderThread :: Maybe (Async ())
  }

new :: Maybe Int -> T.Text -> T.Text -> [SGR] -> App Handle
new numOfItems workingTitle completedTitle color = do
  silentMode <- getSilentMode
  case (silentMode, numOfItems) of
    (True, _) ->
      return Nothing
    (_, Just i)
      | i <= 0 ->
          return Nothing
    _ -> do
      progress <-
        case numOfItems of
          Nothing ->
            return Nothing
          Just v -> do
            return $ Just (0, v)
      let progressBar = ProgressBar {workingTitle, completedTitle, color, progress}
      progressBarRef <- liftIO $ newIORef progressBar
      renderThread <- Just <$> async (render 0 progressBarRef)
      return $ Just $ Handle {progressBarRef, renderThread}

increment :: Handle -> App ()
increment mh = do
  case mh of
    Nothing ->
      return ()
    Just h -> do
      liftIO $ atomicModifyIORef' (progressBarRef h) $ \progressBar -> do
        (next progressBar, ())

render :: Frame -> IORef ProgressBar -> App ()
render i ref = do
  progressBar <- liftIO $ readIORef ref
  Color.printStdOut $ renderInProgress i progressBar <> L.pack' "\n"
  threadDelay 33333 -- 2F
  liftIO $ clear ref
  render (i + 1) ref

clear :: IORef ProgressBar -> IO ()
clear ref = do
  hSetCursorColumn stdout 0
  hClearFromCursorToLineEnd stdout
  hCursorUpLine stdout 1
  hClearFromCursorToLineEnd stdout
  progressBar <- readIORef ref
  case progress progressBar of
    Nothing ->
      return ()
    Just _ -> do
      hCursorUpLine stdout 1
      hClearFromCursorToLineEnd stdout

close :: Handle -> App ()
close mh = do
  case mh of
    Nothing ->
      return ()
    Just h -> do
      forM_ (renderThread h) cancel
      liftIO $ clear (progressBarRef h)
      progressBar <- liftIO $ readIORef (progressBarRef h)
      Color.printStdOut $ renderFinished progressBar
