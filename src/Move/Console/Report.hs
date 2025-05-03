module Move.Console.Report
  ( Handle,
    new,
    printErrorList,
    printNote',
    printRemark,
    printRemarkList,
    printWarning',
  )
where

import Color.Move.Print qualified as Color
import Color.Rule.Handle qualified as Color
import Color.Rule.Text qualified as Color
import Data.Text qualified as T
import Rule.FilePos
import Rule.Remark qualified as R
import System.Console.ANSI.Codes

data Handle
  = Handle
  { colorHandle :: Color.Handle,
    endOfEntry :: T.Text
  }

new :: Color.Handle -> T.Text -> Handle
new colorHandle endOfEntry = do
  Handle {..}

printRemark :: Handle -> R.Remark -> IO ()
printRemark =
  printRemarkIO

printRemarkList :: Handle -> [R.Remark] -> IO ()
printRemarkList h remarkList = do
  foldr ((>>) . printRemark h) (return ()) remarkList

printErrorList :: Handle -> [R.Remark] -> IO ()
printErrorList h remarkList = do
  foldr ((>>) . printErrorIO h) (return ()) remarkList

printNote' :: Handle -> T.Text -> IO ()
printNote' h =
  printRemarkWithoutFilePos h R.Note

printWarning' :: Handle -> T.Text -> IO ()
printWarning' h =
  printRemarkWithoutFilePos h R.Warning

printRemarkWithoutFilePos :: Handle -> R.RemarkLevel -> T.Text -> IO ()
printRemarkWithoutFilePos h level txt =
  printRemark h (Nothing, True, level, txt)

printRemarkIO :: Handle -> R.Remark -> IO ()
printRemarkIO h (mpos, shouldInsertPadding, l, t) = do
  let locText = getRemarkLocation mpos
  let levelText = getRemarkLevel l
  let remarkText = Color.pack' $ getRemarkText t (remarkLevelToPad shouldInsertPadding l)
  footerText <- Color.pack' <$> getFooter h
  Color.printStdOut (colorHandle h) $ locText <> levelText <> remarkText <> footerText

printErrorIO :: Handle -> R.Remark -> IO ()
printErrorIO h (mpos, shouldInsertPadding, l, t) = do
  let locText = getRemarkLocation mpos
  let levelText = getRemarkLevel l
  let remarkText = Color.pack' $ getRemarkText t (remarkLevelToPad shouldInsertPadding l)
  footerText <- Color.pack' <$> getFooter h
  Color.printStdErr (colorHandle h) $ locText <> levelText <> remarkText <> footerText

getRemarkLocation :: Maybe FilePos -> Color.Text
getRemarkLocation mpos = do
  case mpos of
    Just pos -> do
      Color.pack [SetConsoleIntensity BoldIntensity] $ T.pack (showFilePos pos ++ "\n")
    _ ->
      Color.empty

getFooter :: Handle -> IO T.Text
getFooter h = do
  let eoe = getEndOfEntry h
  if eoe == ""
    then return ""
    else return $ eoe <> "\n"

getRemarkLevel :: R.RemarkLevel -> Color.Text
getRemarkLevel l =
  Color.pack (R.remarkLevelToSGR l) (R.remarkLevelToText l <> ": ")

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

getEndOfEntry :: Handle -> T.Text
getEndOfEntry =
  endOfEntry
