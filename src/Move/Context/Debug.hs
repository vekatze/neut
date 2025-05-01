module Move.Context.Debug
  ( Handle,
    new,
    report,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Move.Console.Report (printStdErr)
import Move.Context.Color (getShouldColorizeStderr)
import Move.Context.Color qualified as Color
import Move.Context.EIO (EIO)
import Rule.Log qualified as L
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
    let elapsedTime' = L.pack [SetColor Foreground Vivid Black] (T.pack $ formatNominalDiffTime elapsedTime)
    let shouldColorize = getShouldColorizeStderr (colorHandle h)
    let colorSpec = if shouldColorize then L.Colorful else L.Colorless
    liftIO $ printStdErr colorSpec $ elapsedTime' <> " " <> L.pack' message <> "\n"

formatNominalDiffTime :: NominalDiffTime -> String
formatNominalDiffTime t =
  printf "%.6f" (realToFrac t :: Double)
