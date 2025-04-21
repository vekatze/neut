module Move.Context.Debug
  ( report,
    enableDebugMode,
    setDebugMode,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as T
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import Move.Console.Report (printStdErr)
import Move.Context.App
import Move.Context.App.Internal
import Move.Context.EIO (EIO)
import Rule.Log qualified as L
import System.Console.ANSI
import Text.Printf (printf)

report :: T.Text -> EIO ()
report message = do
  -- todo: parameterize
  let b = False
  let c = L.Colorful
  baseTime <- liftIO getCurrentTime -- todo: use startTime
  -- b <- getDebugMode
  when b $ do
    -- baseTime <- asks startTime
    currentTime <- liftIO getCurrentTime
    let elapsedTime = diffUTCTime currentTime baseTime
    let elapsedTime' = L.pack [SetColor Foreground Vivid Black] (T.pack $ formatNominalDiffTime elapsedTime)
    -- Color.printStdErr $ elapsedTime' <> " " <> L.pack' message <> "\n"
    liftIO $ printStdErr c $ elapsedTime' <> " " <> L.pack' message <> "\n"

setDebugMode :: Bool -> App ()
setDebugMode =
  writeRef' enableDebugMode

formatNominalDiffTime :: NominalDiffTime -> String
formatNominalDiffTime t = printf "%.6f" (realToFrac t :: Double)
