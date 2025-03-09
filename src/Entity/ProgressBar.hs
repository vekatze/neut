module Entity.ProgressBar
  ( ProgressBar (..),
    renderInProgress,
    renderFinished,
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

renderInProgress :: ProgressBar -> Maybe B.ByteString
renderInProgress progressBar = do
  (current, size) <- progress progressBar
  let frac :: Float = fromIntegral current / fromIntegral size
  let pivot = floor $ fromIntegral barLength * frac
  let spinner = withSGR (color progressBar) $ chooseSpinner current
  let title' = spinner <> " " <> workingTitle progressBar
  let prefix = withSGR (color progressBar) $ T.replicate pivot barFinished
  let suffix = T.replicate (barLength - pivot) barInProgress
  let bar = prefix <> suffix
  Just $ encodeUtf8 $ "\r" <> title' <> ": " <> bar <> " " <> T.pack (show current) <> "/" <> T.pack (show size)

renderFinished :: ProgressBar -> B.ByteString
renderFinished progressBar = do
  let check = withSGR (color progressBar) "✓"
  encodeUtf8 $ "\r" <> check <> " " <> completedTitle progressBar <> "\n"

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
