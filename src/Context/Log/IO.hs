module Context.Log.IO
  ( new,
  )
where

import qualified Context.Log as Log
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Entity.FilePos
import Entity.Log
import System.Console.ANSI

new :: Log.Config -> IO Log.Context
new config =
  return
    Log.Context
      { Log.printLog = printLogIO config
      }

printLogIO :: Log.Config -> Log -> IO ()
printLogIO cfg (mpos, l, t) = do
  outputLogLocation cfg mpos
  outputLogLevel cfg l
  outputLogText t (logLevelToPad l)
  outputFooter cfg

outputLogLocation :: Log.Config -> Maybe FilePos -> IO ()
outputLogLocation cfg mpos = do
  case mpos of
    Just pos ->
      withSGR cfg [SetConsoleIntensity BoldIntensity] $ do
        TIO.putStr $ T.pack (showFilePos pos)
        TIO.putStrLn ":"
    _ ->
      return ()

outputFooter :: Log.Config -> IO ()
outputFooter cfg = do
  let eoe = Log.endOfEntry cfg
  if eoe == ""
    then return ()
    else putStrLn eoe

outputLogLevel :: Log.Config -> LogLevel -> IO ()
outputLogLevel cfg l =
  withSGR cfg (logLevelToSGR l) $ do
    TIO.putStr $ logLevelToText l
    TIO.putStr ": "

outputLogText :: T.Text -> IO T.Text -> IO ()
outputLogText str padComp = do
  pad <- padComp
  TIO.putStrLn $ stylizeLogText str pad

logLevelToPad :: LogLevel -> IO T.Text
logLevelToPad level = do
  return $ T.replicate (T.length (logLevelToText level) + 2) " "

stylizeLogText :: T.Text -> T.Text -> T.Text
stylizeLogText str pad = do
  let ls = T.lines str
  if null ls
    then str
    else T.intercalate "\n" $ head ls : map (pad <>) (tail ls)

withSGR :: Log.Config -> [SGR] -> IO () -> IO ()
withSGR cfg arg f = do
  if Log.shouldColorize cfg
    then setSGR arg >> f >> setSGR [Reset]
    else f