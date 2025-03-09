module Entity.ProgressBar
  ( ProgressBar (..),
    Frame,
    renderInProgress,
    renderFinished,
    next,
  )
where

import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import System.Console.ANSI

data ProgressBar
  = ProgressBar
  { workingTitle :: T.Text,
    completedTitle :: T.Text,
    color :: Maybe [SGR],
    progress :: Maybe (Int, Int)
  }

type Frame =
  Int

renderInProgress :: Frame -> ProgressBar -> B.ByteString
renderInProgress frame progressBar = do
  let spinner = withSGR (color progressBar) $ chooseSpinner frame
  let title' = spinner <> " " <> workingTitle progressBar
  case progress progressBar of
    Nothing -> do
      encodeUtf8 $ "\r" <> title'
    Just (current, size) -> do
      let frac :: Float = fromIntegral current / fromIntegral size
      let pivot = floor $ fromIntegral barLength * frac
      let prefix = withSGR (color progressBar) $ T.replicate pivot barFinished
      let suffix = T.replicate (barLength - pivot) barInProgress
      let bar = prefix <> suffix
      encodeUtf8 $ "\r" <> title' <> ": " <> bar <> " " <> T.pack (show current) <> "/" <> T.pack (show size)

renderFinished :: ProgressBar -> B.ByteString
renderFinished progressBar = do
  let check = withSGR (color progressBar) "✓"
  encodeUtf8 $ "\r" <> check <> " " <> completedTitle progressBar <> "\n"

next :: ProgressBar -> ProgressBar
next progressBar = do
  case progress progressBar of
    Nothing ->
      progressBar
    Just (i, count) ->
      progressBar {progress = Just (i + 1, count)}

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
  " "

barFinished :: T.Text
barFinished =
  "━"
