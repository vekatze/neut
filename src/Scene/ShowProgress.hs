module Scene.ShowProgress
  ( Handle,
    new,
    increment,
    close,
  )
where

import Data.ByteString qualified as B
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Entity.ProgressBar (Frame, ProgressBar (..), next, renderFinished, renderInProgress)
import System.Console.ANSI
import System.IO hiding (Handle)
import UnliftIO.Async
import UnliftIO.Concurrent (threadDelay)

data Handle
  = Handle
  { progressBarRef :: IORef ProgressBar,
    renderThread :: Async ()
  }

new :: Maybe Int -> T.Text -> T.Text -> Maybe [SGR] -> IO Handle
new numOfItems workingTitle completedTitle color = do
  progress <-
    case numOfItems of
      Nothing ->
        return Nothing
      Just v -> do
        return $ Just (0, v)
  progressBarRef <- newIORef $ ProgressBar {workingTitle, completedTitle, color, progress}
  renderThread <- async $ render 0 progressBarRef
  return $ Handle {progressBarRef, renderThread}

increment :: Handle -> IO ()
increment h = do
  atomicModifyIORef' (progressBarRef h) $ \progressBar -> do
    (next progressBar, ())

render :: Frame -> IORef ProgressBar -> IO ()
render i ref = do
  progressBar <- readIORef ref
  B.hPutStr stderr $ renderInProgress i progressBar
  threadDelay 33333 -- 2F
  render (i + 1) ref

close :: Handle -> IO ()
close h = do
  cancel $ renderThread h
  hClearFromCursorToLineBeginning stderr
  B.hPutStr stderr $ encodeUtf8 "\r"
  progressBar <- readIORef (progressBarRef h)
  B.hPutStr stderr $ renderFinished progressBar
