module Case.Main.Log
  ( printLogIO,
    Context,
  )
where

import Context.Env qualified as Env
import Control.Monad.IO.Class
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Entity.FilePos
import Entity.Log
import System.Console.ANSI

class (MonadIO m, Env.Context m) => Context m

printLogIO :: Context m => Log -> m ()
printLogIO (mpos, l, t) = do
  outputLogLocation mpos
  outputLogLevel l
  outputLogText t (logLevelToPad l)
  outputFooter

outputLogLocation :: Context m => Maybe FilePos -> m ()
outputLogLocation mpos = do
  case mpos of
    Just pos ->
      withSGR [SetConsoleIntensity BoldIntensity] $ do
        liftIO $ TIO.putStr $ T.pack (showFilePos pos)
        liftIO $ TIO.putStrLn ":"
    _ ->
      return ()

outputFooter :: Context m => m ()
outputFooter = do
  eoe <- Env.getEndOfEntry
  if eoe == ""
    then return ()
    else liftIO $ TIO.putStrLn eoe

outputLogLevel :: Context m => LogLevel -> m ()
outputLogLevel l =
  withSGR (logLevelToSGR l) $ do
    liftIO $ TIO.putStr $ logLevelToText l
    liftIO $ TIO.putStr ": "

outputLogText :: Context m => T.Text -> m T.Text -> m ()
outputLogText str padComp = do
  pad <- padComp
  liftIO $ TIO.putStrLn $ stylizeLogText str pad

logLevelToPad :: Context m => LogLevel -> m T.Text
logLevelToPad level = do
  return $ T.replicate (T.length (logLevelToText level) + 2) " "

stylizeLogText :: T.Text -> T.Text -> T.Text
stylizeLogText str pad = do
  let ls = T.lines str
  if null ls
    then str
    else T.intercalate "\n" $ head ls : map (pad <>) (tail ls)

withSGR :: Context m => [SGR] -> m () -> m ()
withSGR arg f = do
  shouldColorize <- Env.getShouldColorize
  if shouldColorize
    then liftIO (setSGR arg) >> f >> liftIO (setSGR [Reset])
    else f
