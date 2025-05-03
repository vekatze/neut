module Move.Context.Debug
  ( Handle,
    new,
    report,
  )
where

import Color.Move.Print qualified as Color
import Color.Rule.Handle qualified as Color
import Color.Rule.Text qualified as Color
import Control.Monad (when)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import System.Console.ANSI
import Text.Printf (printf)

data Handle
  = Handle
  { colorHandle :: Color.Handle,
    enableDebugMode :: Bool,
    baseTime :: UTCTime
  }

new :: Color.Handle -> Bool -> IO Handle
new colorHandle enableDebugMode = do
  baseTime <- getCurrentTime
  return $ Handle {..}

report :: Handle -> T.Text -> IO ()
report h message = do
  when (enableDebugMode h) $ do
    currentTime <- getCurrentTime
    let elapsedTime = diffUTCTime currentTime (baseTime h)
    let elapsedTime' = Color.pack [SetColor Foreground Vivid Black] (T.pack $ formatNominalDiffTime elapsedTime)
    Color.printStdErr (colorHandle h) $ elapsedTime' <> " " <> Color.pack' message <> "\n"

formatNominalDiffTime :: NominalDiffTime -> String
formatNominalDiffTime t =
  printf "%.6f" (realToFrac t :: Double)
