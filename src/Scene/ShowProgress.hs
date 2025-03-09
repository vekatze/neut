module Scene.ShowProgress
  ( Handle,
    new,
    increment,
    render,
    close,
  )
where

import Data.ByteString qualified as B
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Entity.ProgressBar (ProgressBar (..), renderFinished, renderInProgress)
import System.Console.ANSI
import System.IO hiding (Handle)

type Handle =
  IORef ProgressBar

new :: Maybe Int -> T.Text -> T.Text -> Maybe [SGR] -> IO Handle
new numOfItems workingTitle completedTitle color = do
  progress <-
    case numOfItems of
      Nothing ->
        return Nothing
      Just v -> do
        return $ Just (0, v)
  newIORef $ ProgressBar {workingTitle, completedTitle, color, progress}

increment :: Handle -> IO ()
increment ref = do
  atomicModifyIORef' ref $ \progressBar -> do
    case progress progressBar of
      Nothing ->
        (progressBar, ())
      Just (i, count) ->
        (progressBar {progress = Just (i + 1, count)}, ())

render :: Handle -> IO ()
render ref = do
  progressBar <- readIORef ref
  case renderInProgress progressBar of
    Nothing ->
      return ()
    Just bar -> do
      B.hPutStr stdout bar

close :: Handle -> IO ()
close ref = do
  B.hPutStr stdout $ encodeUtf8 "\r"
  clearFromCursorToLineEnd
  progressBar <- readIORef ref
  B.hPutStr stdout $ renderFinished progressBar
