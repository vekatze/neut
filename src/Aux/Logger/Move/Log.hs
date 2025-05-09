module Aux.Logger.Move.Log
  ( printErrorList,
    printNote',
    printLog,
    printLogList,
    printWarning',
  )
where

import Aux.Color.Move.Print qualified as Color
import Aux.Color.Rule.Text qualified as Color
import Aux.Logger.Rule.Handle (Handle (..))
import Aux.Logger.Rule.Hint
import Aux.Logger.Rule.Log qualified as L
import Aux.Logger.Rule.LogLevel qualified as L
import Data.Text qualified as T
import System.Console.ANSI

printLog :: Handle -> L.Log -> IO ()
printLog =
  printLogIO

printLogList :: Handle -> [L.Log] -> IO ()
printLogList h logList = do
  foldr ((>>) . printLog h) (return ()) logList

printErrorList :: Handle -> [L.Log] -> IO ()
printErrorList h logList = do
  foldr ((>>) . printErrorIO h) (return ()) logList

printNote' :: Handle -> T.Text -> IO ()
printNote' h =
  printLogWithoutFilePos h L.Note

printWarning' :: Handle -> T.Text -> IO ()
printWarning' h =
  printLogWithoutFilePos h L.Warning

printLogWithoutFilePos :: Handle -> L.LogLevel -> T.Text -> IO ()
printLogWithoutFilePos h level txt =
  printLog h $ L.Log Nothing True level txt

printLogIO :: Handle -> L.Log -> IO ()
printLogIO h l = do
  let locText = getLogLocation $ L.position l
  let levelText = getLogLevel (L.logLevel l)
  let logText = Color.pack' $ getLogText (L.content l) (logLevelToPad (L.shouldInsertPadding l) (L.logLevel l))
  footerText <- Color.pack' <$> getFooter h
  Color.printStdOut (_colorHandle h) $ locText <> levelText <> logText <> footerText

printErrorIO :: Handle -> L.Log -> IO ()
printErrorIO h l = do
  let locText = getLogLocation $ L.position l
  let levelText = getLogLevel (L.logLevel l)
  let logText = Color.pack' $ getLogText (L.content l) (logLevelToPad (L.shouldInsertPadding l) (L.logLevel l))
  footerText <- Color.pack' <$> getFooter h
  Color.printStdErr (_colorHandle h) $ locText <> levelText <> logText <> footerText

getLogLocation :: Maybe SavedHint -> Color.Text
getLogLocation mpos = do
  case mpos of
    Just (SavedHint pos) -> do
      Color.pack [SetConsoleIntensity BoldIntensity] $ T.pack (showFilePos pos ++ "\n")
    _ ->
      Color.empty

getFooter :: Handle -> IO T.Text
getFooter h = do
  let eoe = getEndOfEntry h
  if eoe == ""
    then return ""
    else return $ eoe <> "\n"

getLogLevel :: L.LogLevel -> Color.Text
getLogLevel l =
  Color.pack (L._logLevelToSGR l) (L._logLevelToText l <> ": ")

getLogText :: T.Text -> T.Text -> T.Text
getLogText str padComp = do
  let logText = stylizeLogText str padComp
  logText <> "\n"

logLevelToPad :: L.ShouldInsertPadding -> L.LogLevel -> T.Text
logLevelToPad shouldInsertPadding level = do
  if shouldInsertPadding
    then T.replicate (T.length (L._logLevelToText level) + 2) " "
    else ""

stylizeLogText :: T.Text -> T.Text -> T.Text
stylizeLogText str pad = do
  let ls = T.lines str
  if null ls
    then str
    else T.intercalate "\n" $ head ls : map (pad <>) (tail ls)

getEndOfEntry :: Handle -> T.Text
getEndOfEntry =
  _endOfEntry
