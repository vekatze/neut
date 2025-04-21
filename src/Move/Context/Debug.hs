module Move.Context.Debug
  ( Handle,
    new,
    report,
    enableDebugMode,
    setDebugMode,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Move.Console.Report (printStdErr)
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.EIO (EIO)
import Rule.Log qualified as L
import System.Console.ANSI
import Text.Printf (printf)

data Handle
  = Handle
  { enableDebugMode :: Bool,
    colorSpec :: L.ColorSpec,
    baseTime :: UTCTime
  }

new :: App Handle
new = do
  enableDebugMode <- readRef' App.enableDebugMode
  shouldColorize <- readRef' App.shouldColorizeStderr
  let colorSpec = if shouldColorize then L.Colorful else L.Colorless
  baseTime <- asks App.startTime
  return $ Handle {..}

report :: Handle -> T.Text -> EIO ()
report h message = do
  when (enableDebugMode h) $ do
    currentTime <- liftIO getCurrentTime
    let elapsedTime = diffUTCTime currentTime (baseTime h)
    let elapsedTime' = L.pack [SetColor Foreground Vivid Black] (T.pack $ formatNominalDiffTime elapsedTime)
    liftIO $ printStdErr (colorSpec h) $ elapsedTime' <> " " <> L.pack' message <> "\n"

setDebugMode :: Bool -> App ()
setDebugMode =
  writeRef' App.enableDebugMode

formatNominalDiffTime :: NominalDiffTime -> String
formatNominalDiffTime t = printf "%.6f" (realToFrac t :: Double)
