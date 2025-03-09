module Scene.ShowProgress
  ( initialize,
    increment,
    render,
    finalize,
  )
where

import Data.ByteString qualified as B
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Entity.ProgressBar (ProgressBar (..))
import System.Console.ANSI
import System.IO

initialize :: Maybe Int -> T.Text -> T.Text -> Maybe [SGR] -> IO ProgressBar
initialize numOfItems workingTitle completedTitle color = do
  progress <-
    case numOfItems of
      Nothing ->
        return Nothing
      Just v -> do
        ref <- newIORef 0
        return $ Just (ref, v)
  return $ ProgressBar {workingTitle, completedTitle, color, progress}

increment :: ProgressBar -> IO ()
increment progressBar = do
  case progress progressBar of
    Nothing ->
      return ()
    Just (ref, _) ->
      atomicModifyIORef' ref (\x -> (x + 1, ()))

render :: ProgressBar -> IO ()
render progressBar = do
  let title = workingTitle progressBar
  case progress progressBar of
    Nothing ->
      return ()
    Just (currentRef, size) -> do
      current <- readIORef currentRef
      let frac :: Float = fromIntegral current / fromIntegral size
      let pivot = floor $ fromIntegral barLength * frac
      let spinner = withSGR (color progressBar) $ chooseSpinner current
      let title' = spinner <> " " <> title
      let prefix = withSGR (color progressBar) $ T.replicate pivot barFinished
      let suffix = T.replicate (barLength - pivot) barInProgress
      let bar = prefix <> suffix
      let content = "\r" <> title' <> ": " <> bar <> " " <> T.pack (show current) <> "/" <> T.pack (show size)
      B.hPutStr stdout $ encodeUtf8 content

finalize :: ProgressBar -> IO ()
finalize progressBar = do
  let check = withSGR (color progressBar) "✓"
  let title' = check <> " " <> completedTitle progressBar
  B.hPutStr stdout $ encodeUtf8 "\r"
  clearFromCursorToLineEnd
  B.hPutStr stdout $ encodeUtf8 $ "\r" <> title' <> "\n"

withSGR :: Maybe [SGR] -> T.Text -> T.Text
withSGR colorOrNothing str = do
  case colorOrNothing of
    Just color ->
      T.pack (setSGRCode color) <> str <> T.pack (setSGRCode [Reset])
    Nothing ->
      str

chooseSpinner :: Int -> T.Text
chooseSpinner i = do
  case i `rem` 10 of
    0 -> "⠋"
    1 -> "⠙"
    2 -> "⠹"
    3 -> "⠸"
    4 -> "⠼"
    5 -> "⠴"
    6 -> "⠦"
    7 -> "⠧"
    8 -> "⠇"
    _ -> "⠏"

barLength :: Int
barLength =
  32

barInProgress :: T.Text
barInProgress =
  -- "░"
  -- "─"
  " "

barFinished :: T.Text
barFinished =
  -- "█"
  "━"
