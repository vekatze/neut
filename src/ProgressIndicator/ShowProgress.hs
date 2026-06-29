module ProgressIndicator.ShowProgress
  ( Handle,
    new,
    increment,
    close,
  )
where

import Console.Handle qualified as Console
import Console.Print qualified as Console
import Console.Text qualified as Console
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import ProgressIndicator.Handle
import ProgressIndicator.ProgressIndicator
import System.Console.ANSI
import System.IO hiding (Handle)
import UnliftIO.Async
import UnliftIO.Concurrent (threadDelay)

new :: Console.Handle -> Bool -> Maybe Int -> T.Text -> T.Text -> [SGR] -> IO Handle
new consoleHandle silentMode numOfItems workingTitle completedTitle color = do
  case (shouldShowProgress consoleHandle silentMode, numOfItems) of
    (False, _) ->
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
      progressBarRef <- newIORef progressBar
      renderThread <- async (render consoleHandle 0 progressBarRef)
      return $ Just $ Handle {consoleHandle, progressBarRef, renderThread}

shouldShowProgress :: Console.Handle -> Bool -> Bool
shouldShowProgress consoleHandle silentMode = do
  not silentMode && Console.supportsInteractiveOutput consoleHandle

increment :: Handle -> IO ()
increment mh = do
  case mh of
    Nothing ->
      return ()
    Just h -> do
      atomicModifyIORef' (progressBarRef h) $ \progressBar -> do
        (next progressBar, ())

render :: Console.Handle -> Frame -> IORef ProgressBar -> IO ()
render consoleHandle i ref = do
  progressBar <- readIORef ref
  Console.printStdOut consoleHandle $ renderInProgress i progressBar <> Console.pack' "\n"
  threadDelay 33333 -- 2F
  clear ref
  render consoleHandle (i + 1) ref

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

close :: Handle -> IO ()
close mh = do
  case mh of
    Nothing ->
      return ()
    Just h -> do
      cancel (renderThread h)
      clear (progressBarRef h)
      progressBar <- readIORef (progressBarRef h)
      Console.printStdOut (consoleHandle h) $ renderFinished progressBar
