module Logger.Print
  ( printNote',
    printLog,
    printLogList,
    printWarning',
  )
where

import Console.Print qualified as Console
import Console.Text qualified as Console
import Data.IORef (readIORef)
import Data.Text qualified as T
import Logger.Handle (Handle (..))
import Logger.Hint
import Logger.Log qualified as L
import Logger.LogLevel qualified as L
import System.Console.ANSI

printLogList :: Handle -> [L.Log] -> IO ()
printLogList h logList = do
  foldr ((>>) . printLog h) (return ()) $ L.sortByPosition logList

printNote' :: Handle -> T.Text -> IO ()
printNote' h =
  printLogWithoutFilePos h L.Note

printWarning' :: Handle -> T.Text -> IO ()
printWarning' h =
  printLogWithoutFilePos h L.Warning

printLogWithoutFilePos :: Handle -> L.LogLevel -> T.Text -> IO ()
printLogWithoutFilePos h level txt =
  printLog h $ L.Log Nothing level txt

printLog :: Handle -> L.Log -> IO ()
printLog h l = do
  locText <- getLogLocation h $ L.position l
  let levelText = getLogLevel (L.logLevel l)
  let logText = Console.pack' $ getLogText (L.content l) (logLevelToPad (L.logLevel l))
  Console.printStdErr (_consoleHandle h) $ locText <> levelText <> logText

getLogLocation :: Handle -> Maybe SavedHint -> IO Console.Text
getLogLocation h mpos = do
  case mpos of
    Just (SavedHint pos) -> do
      moduleDir <- readIORef (_moduleDirRef h)
      return . Console.pack [SetConsoleIntensity BoldIntensity] $ showFilePosRelative moduleDir pos <> "\n"
    _ ->
      return Console.empty

getLogLevel :: L.LogLevel -> Console.Text
getLogLevel l =
  Console.pack (L._logLevelToSGR l) (L._logLevelToText l <> ": ")

getLogText :: T.Text -> T.Text -> T.Text
getLogText str padComp = do
  let logText = stylizeLogText str padComp
  logText <> "\n"

logLevelToPad :: L.LogLevel -> T.Text
logLevelToPad level = do
  T.replicate (T.length (L._logLevelToText level) + 2) " "

stylizeLogText :: T.Text -> T.Text -> T.Text
stylizeLogText str pad = do
  let ls = T.lines str
  case ls of
    [] ->
      str
    l : rest ->
      T.intercalate "\n" $ l : map (pad <>) rest
