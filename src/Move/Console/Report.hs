module Move.Console.Report
  ( Handle,
    new,
    printCritical',
    printCritical,
    printError',
    printError,
    printErrorList,
    printFail',
    printFail,
    printNote',
    printNote,
    printPass',
    printPass,
    printRemark,
    printRemarkList,
    printString,
    printWarning',
    printWarning,
    printStdOut,
    printStdErr,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import Data.ByteString qualified as B
import Data.IORef
import Data.Text qualified as T
import Data.Text.Encoding
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Rule.FilePos
import Rule.FilePos qualified as FilePos
import Rule.Hint
import Rule.Log qualified as L
import Rule.Remark qualified as R
import System.Console.ANSI.Codes
import System.IO hiding (Handle)

data Handle
  = Handle
  { shouldColorizeStdoutRef :: IORef Bool,
    shouldColorizeStderrRef :: IORef Bool
  }

new :: App Handle
new = do
  shouldColorizeStdoutRef <- asks App.shouldColorizeStdout
  shouldColorizeStderrRef <- asks App.shouldColorizeStderr
  return $ Handle {..}

printString :: String -> IO ()
printString =
  liftIO . putStrLn

printRemark :: Handle -> R.Remark -> IO ()
printRemark =
  printRemarkIO

printRemarkList :: Handle -> [R.Remark] -> IO ()
printRemarkList h remarkList = do
  foldr ((>>) . printRemark h) (return ()) remarkList

printErrorList :: Handle -> [R.Remark] -> IO ()
printErrorList h remarkList = do
  foldr ((>>) . printErrorIO h) (return ()) remarkList

printNote :: Handle -> Hint -> T.Text -> IO ()
printNote h =
  printRemarkWithFilePos h R.Note

printNote' :: Handle -> T.Text -> IO ()
printNote' h =
  printRemarkWithoutFilePos h R.Note

printWarning :: Handle -> Hint -> T.Text -> IO ()
printWarning h =
  printRemarkWithFilePos h R.Warning

printWarning' :: Handle -> T.Text -> IO ()
printWarning' h =
  printRemarkWithoutFilePos h R.Warning

printError :: Handle -> Hint -> T.Text -> IO ()
printError h =
  printRemarkWithFilePos h R.Error

printError' :: Handle -> T.Text -> IO ()
printError' h =
  printRemarkWithoutFilePos h R.Error

printCritical :: Handle -> Hint -> T.Text -> IO ()
printCritical h =
  printRemarkWithFilePos h R.Critical

printCritical' :: Handle -> T.Text -> IO ()
printCritical' h =
  printRemarkWithoutFilePos h R.Critical

printPass :: Handle -> Hint -> T.Text -> IO ()
printPass h =
  printRemarkWithFilePos h R.Pass

printPass' :: Handle -> T.Text -> IO ()
printPass' h =
  printRemarkWithoutFilePos h R.Pass

printFail :: Handle -> Hint -> T.Text -> IO ()
printFail h =
  printRemarkWithFilePos h R.Fail

printFail' :: Handle -> T.Text -> IO ()
printFail' h =
  printRemarkWithoutFilePos h R.Fail

printRemarkWithFilePos :: Handle -> R.RemarkLevel -> Hint -> T.Text -> IO ()
printRemarkWithFilePos h level m txt = do
  printRemark h (FilePos.fromHint m, True, level, txt)

printRemarkWithoutFilePos :: Handle -> R.RemarkLevel -> T.Text -> IO ()
printRemarkWithoutFilePos h level txt =
  printRemark h (Nothing, True, level, txt)

printRemarkIO :: Handle -> R.Remark -> IO ()
printRemarkIO h (mpos, shouldInsertPadding, l, t) = do
  let locText = getRemarkLocation mpos
  let levelText = getRemarkLevel l
  let remarkText = L.pack' $ getRemarkText t (remarkLevelToPad shouldInsertPadding l)
  b <- readIORef $ shouldColorizeStdoutRef h
  let colorSpec = if b then L.Colorful else L.Colorless
  printStdOut colorSpec $ locText <> levelText <> remarkText

printErrorIO :: Handle -> R.Remark -> IO ()
printErrorIO h (mpos, shouldInsertPadding, l, t) = do
  let locText = getRemarkLocation mpos
  let levelText = getRemarkLevel l
  let remarkText = L.pack' $ getRemarkText t (remarkLevelToPad shouldInsertPadding l)
  b <- readIORef $ shouldColorizeStderrRef h
  let colorSpec = if b then L.Colorful else L.Colorless
  printStdErr colorSpec $ locText <> levelText <> remarkText

getRemarkLocation :: Maybe FilePos -> L.Log
getRemarkLocation mpos = do
  case mpos of
    Just pos -> do
      L.pack [SetConsoleIntensity BoldIntensity] $ T.pack (showFilePos pos ++ "\n")
    _ ->
      L.Nil

getRemarkLevel :: R.RemarkLevel -> L.Log
getRemarkLevel l =
  L.pack (R.remarkLevelToSGR l) (R.remarkLevelToText l <> ": ")

getRemarkText :: T.Text -> T.Text -> T.Text
getRemarkText str padComp = do
  let remarkText = stylizeRemarkText str padComp
  remarkText <> "\n"

remarkLevelToPad :: R.ShouldInsertPadding -> R.RemarkLevel -> T.Text
remarkLevelToPad shouldInsertPadding level = do
  if shouldInsertPadding
    then T.replicate (T.length (R.remarkLevelToText level) + 2) " "
    else ""

stylizeRemarkText :: T.Text -> T.Text -> T.Text
stylizeRemarkText str pad = do
  let ls = T.lines str
  if null ls
    then str
    else T.intercalate "\n" $ head ls : map (pad <>) (tail ls)

printStdOut :: L.ColorSpec -> L.Log -> IO ()
printStdOut c l = do
  B.hPutStr stdout $ encodeUtf8 $ L.unpack c l

printStdErr :: L.ColorSpec -> L.Log -> IO ()
printStdErr c l = do
  B.hPutStr stderr $ encodeUtf8 $ L.unpack c l
