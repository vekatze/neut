module Context.Log
  ( printString,
    printLog,
    printNote,
    printNote',
    printWarning,
    printWarning',
    printError,
    printError',
    printCritical,
    printCritical',
    printPass,
    printPass',
    printFail,
    printFail',
    setEndOfEntry,
    getEndOfEntry,
    setShouldColorize,
    getShouldColorize,
  )
where

import Context.App
import Context.App.Internal
import Control.Monad.IO.Class
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Entity.FilePos
import Entity.FilePos qualified as FilePos
import Entity.Hint
import Entity.Log
import Entity.Log qualified as L
import System.Console.ANSI

printString :: String -> App ()
printString =
  liftIO . putStrLn

printLog :: L.Log -> App ()
printLog =
  printLogIO

printNote :: Hint -> T.Text -> App ()
printNote =
  printLogWithFilePos L.Note

printNote' :: T.Text -> App ()
printNote' =
  printLogWithoutFilePos L.Note

printWarning :: Hint -> T.Text -> App ()
printWarning =
  printLogWithFilePos L.Warning

printWarning' :: T.Text -> App ()
printWarning' =
  printLogWithoutFilePos L.Warning

printError :: Hint -> T.Text -> App ()
printError =
  printLogWithFilePos L.Error

printError' :: T.Text -> App ()
printError' =
  printLogWithoutFilePos L.Error

printCritical :: Hint -> T.Text -> App ()
printCritical =
  printLogWithFilePos L.Critical

printCritical' :: T.Text -> App ()
printCritical' =
  printLogWithoutFilePos L.Critical

printPass :: Hint -> T.Text -> App ()
printPass =
  printLogWithFilePos L.Pass

printPass' :: T.Text -> App ()
printPass' =
  printLogWithoutFilePos L.Pass

printFail :: Hint -> T.Text -> App ()
printFail =
  printLogWithFilePos L.Fail

printFail' :: T.Text -> App ()
printFail' =
  printLogWithoutFilePos L.Fail

printLogWithFilePos :: L.LogLevel -> Hint -> T.Text -> App ()
printLogWithFilePos level m txt = do
  printLog (Just (FilePos.fromHint m), level, txt)

printLogWithoutFilePos :: L.LogLevel -> T.Text -> App ()
printLogWithoutFilePos level txt =
  printLog (Nothing, level, txt)

printLogIO :: L.Log -> App ()
printLogIO (mpos, l, t) = do
  outputLogLocation mpos
  outputLogLevel l
  outputLogText t (logLevelToPad l)
  outputFooter

outputLogLocation :: Maybe FilePos -> App ()
outputLogLocation mpos = do
  case mpos of
    Just pos ->
      withSGR [SetConsoleIntensity BoldIntensity] $ do
        liftIO $ TIO.putStr $ T.pack (showFilePos pos)
        liftIO $ TIO.putStrLn ":"
    _ ->
      return ()

outputFooter :: App ()
outputFooter = do
  eoe <- getEndOfEntry
  if eoe == ""
    then return ()
    else liftIO $ TIO.putStrLn eoe

outputLogLevel :: LogLevel -> App ()
outputLogLevel l =
  withSGR (logLevelToSGR l) $ do
    liftIO $ TIO.putStr $ logLevelToText l
    liftIO $ TIO.putStr ": "

outputLogText :: T.Text -> App T.Text -> App ()
outputLogText str padComp = do
  pad <- padComp
  liftIO $ TIO.putStrLn $ stylizeLogText str pad

logLevelToPad :: LogLevel -> App T.Text
logLevelToPad level = do
  return $ T.replicate (T.length (logLevelToText level) + 2) " "

stylizeLogText :: T.Text -> T.Text -> T.Text
stylizeLogText str pad = do
  let ls = T.lines str
  if null ls
    then str
    else T.intercalate "\n" $ head ls : map (pad <>) (tail ls)

withSGR :: [SGR] -> App () -> App ()
withSGR arg f = do
  shouldColorize <- getShouldColorize
  if shouldColorize
    then liftIO (setSGR arg) >> f >> liftIO (setSGR [Reset])
    else f

setEndOfEntry :: T.Text -> App ()
setEndOfEntry =
  writeRef' endOfEntry

getEndOfEntry :: App T.Text
getEndOfEntry =
  readRef' endOfEntry

setShouldColorize :: Bool -> App ()
setShouldColorize =
  writeRef' shouldColorize

getShouldColorize :: App Bool
getShouldColorize =
  readRef' shouldColorize
