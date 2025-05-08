module Library.Logger.Move.Debug (report) where

import Control.Monad (when)
import Data.Text qualified as T
import Data.Time (diffUTCTime, getCurrentTime)
import Library.Color.Move.Print qualified as Color
import Library.Color.Rule.Text qualified as Color
import Library.Logger.Rule.Handle
import System.Console.ANSI

report :: Handle -> T.Text -> IO ()
report h message = do
  when (_enableDebugMode h) $ do
    currentTime <- getCurrentTime
    let elapsedTime = diffUTCTime currentTime (_baseTime h)
    let elapsedTime' = Color.pack [SetColor Foreground Vivid Black] (T.pack $ _formatNominalDiffTime elapsedTime)
    Color.printStdErr (_colorHandle h) $ elapsedTime' <> " " <> Color.pack' message <> "\n"
