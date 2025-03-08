module Context.Debug
  ( report,
    enableDebugMode,
    setDebugMode,
  )
where

import Context.App
import Context.App.Internal
import Context.Remark (withSGR)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import System.Console.ANSI
import System.IO (stderr)
import Text.Printf (printf)

report :: T.Text -> App ()
report message = do
  b <- getDebugMode
  when b $ do
    baseTime <- asks startTime
    currentTime <- liftIO getCurrentTime
    let elapsedTime = diffUTCTime currentTime baseTime
    elapsedTime' <- withSGR [SetColor Foreground Vivid Black] (T.pack $ formatNominalDiffTime elapsedTime)
    liftIO $ B.hPutStr stderr $ encodeUtf8 $ elapsedTime' <> " " <> message <> "\n"

getDebugMode :: App Bool
getDebugMode =
  readRef' enableDebugMode

setDebugMode :: Bool -> App ()
setDebugMode =
  writeRef' enableDebugMode

formatNominalDiffTime :: NominalDiffTime -> String
formatNominalDiffTime t = printf "%.6f" (realToFrac t :: Double)
