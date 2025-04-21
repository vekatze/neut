module Move.Console.Report
  ( printCritical',
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
    getColorSpecStdOut,
    getColorSpecStdErr,
    printStdOut,
    printStdErr,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding
import Move.Context.App
import Move.Context.App.Internal
import Rule.FilePos
import Rule.FilePos qualified as FilePos
import Rule.Hint
import Rule.Log qualified as L
import Rule.Remark qualified as R
import System.Console.ANSI.Codes
import System.IO

-- temporary
getColorSpecStdOut :: App L.ColorSpec
getColorSpecStdOut = do
  b <- readRef' shouldColorizeStdout
  if b
    then return L.Colorful
    else return L.Colorless

-- temporary
getColorSpecStdErr :: App L.ColorSpec
getColorSpecStdErr = do
  b <- readRef' shouldColorizeStderr
  if b
    then return L.Colorful
    else return L.Colorless

printString :: String -> IO ()
printString =
  liftIO . putStrLn

printRemark :: L.ColorSpec -> R.Remark -> IO ()
printRemark =
  printRemarkIO

printRemarkList :: L.ColorSpec -> [R.Remark] -> IO ()
printRemarkList c remarkList = do
  foldr ((>>) . printRemark c) (return ()) remarkList

printErrorList :: L.ColorSpec -> [R.Remark] -> IO ()
printErrorList c remarkList = do
  foldr ((>>) . printErrorIO c) (return ()) remarkList

printNote :: L.ColorSpec -> Hint -> T.Text -> IO ()
printNote c =
  printRemarkWithFilePos c R.Note

printNote' :: L.ColorSpec -> T.Text -> IO ()
printNote' c =
  printRemarkWithoutFilePos c R.Note

printWarning :: L.ColorSpec -> Hint -> T.Text -> IO ()
printWarning c =
  printRemarkWithFilePos c R.Warning

printWarning' :: L.ColorSpec -> T.Text -> IO ()
printWarning' c =
  printRemarkWithoutFilePos c R.Warning

printError :: L.ColorSpec -> Hint -> T.Text -> IO ()
printError c =
  printRemarkWithFilePos c R.Error

printError' :: L.ColorSpec -> T.Text -> IO ()
printError' c =
  printRemarkWithoutFilePos c R.Error

printCritical :: L.ColorSpec -> Hint -> T.Text -> IO ()
printCritical c =
  printRemarkWithFilePos c R.Critical

printCritical' :: L.ColorSpec -> T.Text -> IO ()
printCritical' c =
  printRemarkWithoutFilePos c R.Critical

printPass :: L.ColorSpec -> Hint -> T.Text -> IO ()
printPass c =
  printRemarkWithFilePos c R.Pass

printPass' :: L.ColorSpec -> T.Text -> IO ()
printPass' c =
  printRemarkWithoutFilePos c R.Pass

printFail :: L.ColorSpec -> Hint -> T.Text -> IO ()
printFail c =
  printRemarkWithFilePos c R.Fail

printFail' :: L.ColorSpec -> T.Text -> IO ()
printFail' c =
  printRemarkWithoutFilePos c R.Fail

printRemarkWithFilePos :: L.ColorSpec -> R.RemarkLevel -> Hint -> T.Text -> IO ()
printRemarkWithFilePos c level m txt = do
  printRemark c (FilePos.fromHint m, True, level, txt)

printRemarkWithoutFilePos :: L.ColorSpec -> R.RemarkLevel -> T.Text -> IO ()
printRemarkWithoutFilePos c level txt =
  printRemark c (Nothing, True, level, txt)

printRemarkIO :: L.ColorSpec -> R.Remark -> IO ()
printRemarkIO c (mpos, shouldInsertPadding, l, t) = do
  let locText = getRemarkLocation mpos
  let levelText = getRemarkLevel l
  let remarkText = L.pack' $ getRemarkText t (remarkLevelToPad shouldInsertPadding l)
  printStdOut c $ locText <> levelText <> remarkText

printErrorIO :: L.ColorSpec -> R.Remark -> IO ()
printErrorIO c (mpos, shouldInsertPadding, l, t) = do
  let locText = getRemarkLocation mpos
  let levelText = getRemarkLevel l
  let remarkText = L.pack' $ getRemarkText t (remarkLevelToPad shouldInsertPadding l)
  printStdErr c $ locText <> levelText <> remarkText

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
