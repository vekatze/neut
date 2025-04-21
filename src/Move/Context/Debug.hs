module Move.Context.Debug
  ( report,
    enableDebugMode,
    setDebugMode,
  )
where

import Move.Context.App
import Move.Context.App.Internal
import Move.Context.Color qualified as Color
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import Rule.Log qualified as L
import System.Console.ANSI
import Text.Printf (printf)

report :: T.Text -> App ()
report message = do
  b <- getDebugMode
  when b $ do
    baseTime <- asks startTime
    currentTime <- liftIO getCurrentTime
    let elapsedTime = diffUTCTime currentTime baseTime
    let elapsedTime' = L.pack [SetColor Foreground Vivid Black] (T.pack $ formatNominalDiffTime elapsedTime)
    Color.printStdErr $ elapsedTime' <> " " <> L.pack' message <> "\n"

getDebugMode :: App Bool
getDebugMode =
  readRef' enableDebugMode

setDebugMode :: Bool -> App ()
setDebugMode =
  writeRef' enableDebugMode

formatNominalDiffTime :: NominalDiffTime -> String
formatNominalDiffTime t = printf "%.6f" (realToFrac t :: Double)
