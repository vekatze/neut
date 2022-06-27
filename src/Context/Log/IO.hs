module Context.Log.IO (logContextIO) where

import Context.Log
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Entity.FilePos
import Entity.Global
import Entity.Log
import System.Console.ANSI

logContextIO :: LogContext IO
logContextIO =
  LogContext
    { printLog = printLogIO
    }

printLogIO :: Log -> IO ()
printLogIO (mpos, l, t) = do
  outputLogLocation mpos
  outputLogLevel l
  outputLogText t (logLevelToPad l)
  outputFooter

outputLogLocation :: Maybe FilePos -> IO ()
outputLogLocation mpos = do
  case mpos of
    Just pos ->
      withSGR [SetConsoleIntensity BoldIntensity] $ do
        TIO.putStr $ T.pack (showFilePos pos)
        TIO.putStrLn ":"
    _ ->
      return ()

outputFooter :: IO ()
outputFooter = do
  eoe <- readIORef endOfEntryRef
  if eoe == ""
    then return ()
    else putStrLn eoe

outputLogLevel :: LogLevel -> IO ()
outputLogLevel l =
  withSGR (logLevelToSGR l) $ do
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

withSGR :: [SGR] -> IO () -> IO ()
withSGR arg f = do
  b <- readIORef shouldColorizeRef
  if b
    then setSGR arg >> f >> setSGR [Reset]
    else f
