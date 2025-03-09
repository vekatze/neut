module Scene.ShowProgress
  ( Handle,
    new,
    increment,
    close,
  )
where

import Context.App
import Context.Color qualified as Color
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Entity.Log qualified as L
import Entity.ProgressBar (Frame, ProgressBar (..), next, renderFinished, renderInProgress)
import System.Console.ANSI
import System.IO hiding (Handle)
import UnliftIO.Async
import UnliftIO.Concurrent (threadDelay)

data Handle
  = Handle
  { progressBarRef :: IORef ProgressBar,
    renderThread :: Maybe (Async ())
  }

new :: Maybe Int -> T.Text -> T.Text -> [SGR] -> App Handle
new numOfItems workingTitle completedTitle color = do
  progress <-
    case numOfItems of
      Nothing ->
        return Nothing
      Just v -> do
        return $ Just (0, v)
  stdoutIsTerminal <- liftIO $ hIsTerminalDevice stdout
  let progressBar = ProgressBar {workingTitle, completedTitle, color, progress, showSymbol = stdoutIsTerminal}
  if stdoutIsTerminal
    then do
      progressBarRef <- liftIO $ newIORef progressBar
      renderThread <- Just <$> async (render 0 progressBarRef)
      return $ Handle {progressBarRef, renderThread}
    else do
      let progressBar' = progressBar {progress = Nothing}
      progressBarRef <- liftIO $ newIORef progressBar'
      Color.printStdOut $ renderInProgress 0 progressBar' <> L.pack' "\n"
      return $ Handle {progressBarRef, renderThread = Nothing}

increment :: Handle -> App ()
increment h = do
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
  stdoutIsTerminal <- liftIO $ hIsTerminalDevice stdout
  when stdoutIsTerminal $ do
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
close h = do
  forM_ (renderThread h) cancel
  liftIO $ clear (progressBarRef h)
  progressBar <- liftIO $ readIORef (progressBarRef h)
  Color.printStdOut $ renderFinished progressBar
