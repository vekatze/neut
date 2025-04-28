module Move.Context.Debug
  ( Handle,
    new,
    report,
    setDebugMode,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef
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
    enableDebugModeRef :: IORef Bool,
    baseTime :: UTCTime
  }

new :: Color.Handle -> IO Handle
new colorHandle = do
  enableDebugModeRef <- newIORef False
  baseTime <- getCurrentTime
  return $ Handle {..}

report :: Handle -> T.Text -> EIO ()
report h message = do
  enableDebugMode <- liftIO $ readIORef (enableDebugModeRef h)
  when enableDebugMode $ do
    currentTime <- liftIO getCurrentTime
    let elapsedTime = diffUTCTime currentTime (baseTime h)
    let elapsedTime' = L.pack [SetColor Foreground Vivid Black] (T.pack $ formatNominalDiffTime elapsedTime)
    shouldColorize <- liftIO $ getShouldColorizeStderr (colorHandle h)
    let colorSpec = if shouldColorize then L.Colorful else L.Colorless
    liftIO $ printStdErr colorSpec $ elapsedTime' <> " " <> L.pack' message <> "\n"

setDebugMode :: Handle -> Bool -> IO ()
setDebugMode h =
  writeIORef (enableDebugModeRef h)

formatNominalDiffTime :: NominalDiffTime -> String
formatNominalDiffTime t =
  printf "%.6f" (realToFrac t :: Double)
