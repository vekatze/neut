module Entity.ProgressBar
  ( ProgressBar (..),
    Frame,
    renderInProgress,
    renderFinished,
    next,
  )
where

import Data.Text qualified as T
import Entity.Log qualified as L
import System.Console.ANSI

data ProgressBar
  = ProgressBar
  { workingTitle :: T.Text,
    completedTitle :: T.Text,
    color :: [SGR],
    progress :: Maybe (Int, Int)
  }

type Frame =
  Int

renderInProgress :: Frame -> ProgressBar -> L.Log
renderInProgress frame progressBar = do
  let spinner = L.pack (color progressBar) $ chooseSpinner frame
  let title' = spinner <> " " <> L.pack' (workingTitle progressBar)
  case progress progressBar of
    Nothing -> do
      title'
    Just (current, size) -> do
      let frac :: Float = fromIntegral current / fromIntegral size
      let pivot = floor $ fromIntegral barLength * frac
      let prefix = L.pack (color progressBar) $ T.replicate pivot barFinished
      let suffix = L.pack' $ T.replicate (barLength - pivot) barInProgress
      let bar = prefix <> suffix
      let current' = L.pack' $ T.pack (show current)
      let size' = L.pack' $ T.pack (show size)
      title' <> "\n  " <> bar <> " " <> current' <> "/" <> size'

renderFinished :: ProgressBar -> L.Log
renderFinished progressBar = do
  let check = L.pack (color progressBar) "✓" <> " "
  check <> L.pack' (completedTitle progressBar) <> "\n"

next :: ProgressBar -> ProgressBar
next progressBar = do
  case progress progressBar of
    Nothing ->
      progressBar
    Just (i, count) ->
      progressBar {progress = Just (i + 1, count)}

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
