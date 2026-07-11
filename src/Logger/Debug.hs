module Logger.Debug (report, trace) where

import Console.Handle (isActivityMode, isTraceMode)
import Console.Print qualified as Console
import Console.Text qualified as Console
import Control.Monad (when)
import Data.Text qualified as T
import Data.Time (diffUTCTime, getCurrentTime)
import Logger.Handle
import System.Console.ANSI

report :: Handle -> T.Text -> IO ()
report h message = do
  when (isActivityMode (_consoleHandle h)) $ do
    printMessage h message

trace :: Handle -> T.Text -> IO ()
trace h message = do
  when (isTraceMode (_consoleHandle h)) $ do
    printMessage h message

printMessage :: Handle -> T.Text -> IO ()
printMessage h message = do
  currentTime <- getCurrentTime
  let elapsedTime = diffUTCTime currentTime (_baseTime h)
  let elapsedTime' = Console.pack [SetColor Foreground Vivid Black] (T.pack $ _formatNominalDiffTime elapsedTime)
  Console.printStdErr (_consoleHandle h) $ elapsedTime' <> " " <> Console.pack' message <> "\n"
