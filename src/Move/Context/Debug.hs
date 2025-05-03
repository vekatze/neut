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
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Move.Context.EIO (EIO)
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

report :: Handle -> T.Text -> EIO ()
report h message = do
  when (enableDebugMode h) $ do
    currentTime <- liftIO getCurrentTime
    let elapsedTime = diffUTCTime currentTime (baseTime h)
    let elapsedTime' = Color.pack [SetColor Foreground Vivid Black] (T.pack $ formatNominalDiffTime elapsedTime)
    liftIO $ Color.printStdErr (colorHandle h) $ elapsedTime' <> " " <> Color.pack' message <> "\n"

formatNominalDiffTime :: NominalDiffTime -> String
formatNominalDiffTime t =
  printf "%.6f" (realToFrac t :: Double)
