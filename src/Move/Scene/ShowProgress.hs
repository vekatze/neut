module Move.Scene.ShowProgress
  ( Handle,
    new,
    increment,
    close,
  )
where

import Control.Monad
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Move.Context.Color qualified as Color
import Move.Context.Env qualified as Env
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
  { envHandle :: Env.Handle,
    colorHandle :: Color.Handle,
    progressBarRef :: IORef ProgressBar,
    renderThread :: Maybe (Async ())
  }

new :: Env.Handle -> Color.Handle -> Maybe Int -> T.Text -> T.Text -> [SGR] -> IO Handle
new envHandle colorHandle numOfItems workingTitle completedTitle color = do
  let silentMode = Env.getSilentMode envHandle
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
      progressBarRef <- newIORef progressBar
      renderThread <- Just <$> async (render colorHandle 0 progressBarRef)
      return $ Just $ Handle {envHandle, colorHandle, progressBarRef, renderThread}

increment :: Handle -> IO ()
increment mh = do
  case mh of
    Nothing ->
      return ()
    Just h -> do
      atomicModifyIORef' (progressBarRef h) $ \progressBar -> do
        (next progressBar, ())

render :: Color.Handle -> Frame -> IORef ProgressBar -> IO ()
render colorHandle i ref = do
  progressBar <- readIORef ref
  Color.printStdOut colorHandle $ renderInProgress i progressBar <> L.pack' "\n"
  threadDelay 33333 -- 2F
  clear ref
  render colorHandle (i + 1) ref

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
      forM_ (renderThread h) cancel
      clear (progressBarRef h)
      progressBar <- readIORef (progressBarRef h)
      Color.printStdOut (colorHandle h) $ renderFinished progressBar
