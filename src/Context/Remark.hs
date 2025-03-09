module Context.Remark
  ( initialize,
    insertRemark,
    getRemarkList,
    setRemarkList,
    insertToGlobalRemarkList,
    setGlobalRemarkList,
    getGlobalRemarkList,
    printString,
    printRemark,
    printRemarkList,
    printErrorList,
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
  )
where

import Context.App
import Context.App.Internal
import Context.Color qualified as Color
import Control.Monad.IO.Class
import Data.Text qualified as T
import Entity.FilePos
import Entity.FilePos qualified as FilePos
import Entity.Hint
import Entity.Log qualified as L
import Entity.Remark qualified as R
import System.Console.ANSI.Codes

initialize :: App ()
initialize = do
  writeRef' remarkList []

printString :: String -> App ()
printString =
  liftIO . putStrLn

printRemark :: R.Remark -> App ()
printRemark =
  printRemarkIO

printRemarkList :: [R.Remark] -> App ()
printRemarkList remarkList = do
  foldr ((>>) . printRemark) (return ()) remarkList

printErrorList :: [R.Remark] -> App ()
printErrorList remarkList = do
  foldr ((>>) . printErrorIO) (return ()) remarkList

printNote :: Hint -> T.Text -> App ()
printNote =
  printRemarkWithFilePos R.Note

printNote' :: T.Text -> App ()
printNote' =
  printRemarkWithoutFilePos R.Note

printWarning :: Hint -> T.Text -> App ()
printWarning =
  printRemarkWithFilePos R.Warning

printWarning' :: T.Text -> App ()
printWarning' =
  printRemarkWithoutFilePos R.Warning

printError :: Hint -> T.Text -> App ()
printError =
  printRemarkWithFilePos R.Error

printError' :: T.Text -> App ()
printError' =
  printRemarkWithoutFilePos R.Error

printCritical :: Hint -> T.Text -> App ()
printCritical =
  printRemarkWithFilePos R.Critical

printCritical' :: T.Text -> App ()
printCritical' =
  printRemarkWithoutFilePos R.Critical

printPass :: Hint -> T.Text -> App ()
printPass =
  printRemarkWithFilePos R.Pass

printPass' :: T.Text -> App ()
printPass' =
  printRemarkWithoutFilePos R.Pass

printFail :: Hint -> T.Text -> App ()
printFail =
  printRemarkWithFilePos R.Fail

printFail' :: T.Text -> App ()
printFail' =
  printRemarkWithoutFilePos R.Fail

printRemarkWithFilePos :: R.RemarkLevel -> Hint -> T.Text -> App ()
printRemarkWithFilePos level m txt = do
  printRemark (FilePos.fromHint m, True, level, txt)

printRemarkWithoutFilePos :: R.RemarkLevel -> T.Text -> App ()
printRemarkWithoutFilePos level txt =
  printRemark (Nothing, True, level, txt)

printRemarkIO :: R.Remark -> App ()
printRemarkIO (mpos, shouldInsertPadding, l, t) = do
  let locText = getRemarkLocation mpos
  let levelText = getRemarkLevel l
  let remarkText = L.pack' $ getRemarkText t (remarkLevelToPad shouldInsertPadding l)
  footerText <- L.pack' <$> getFooter
  Color.printStdOut $ locText <> levelText <> remarkText <> footerText

printErrorIO :: R.Remark -> App ()
printErrorIO (mpos, shouldInsertPadding, l, t) = do
  let locText = getRemarkLocation mpos
  let levelText = getRemarkLevel l
  let remarkText = L.pack' $ getRemarkText t (remarkLevelToPad shouldInsertPadding l)
  footerText <- L.pack' <$> getFooter
  Color.printStdErr $ locText <> levelText <> remarkText <> footerText

getRemarkLocation :: Maybe FilePos -> L.Log
getRemarkLocation mpos = do
  case mpos of
    Just pos -> do
      L.pack [SetConsoleIntensity BoldIntensity] $ T.pack (showFilePos pos ++ "\n")
    _ ->
      L.Nil

getFooter :: App T.Text
getFooter = do
  eoe <- getEndOfEntry
  if eoe == ""
    then return ""
    else return $ eoe <> "\n"

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

setEndOfEntry :: T.Text -> App ()
setEndOfEntry =
  writeRef' endOfEntry

getEndOfEntry :: App T.Text
getEndOfEntry =
  readRef' endOfEntry

insertRemark :: R.Remark -> App ()
insertRemark r = do
  modifyRef' remarkList $ (:) r

getRemarkList :: App [R.Remark]
getRemarkList = do
  readRef' remarkList

setRemarkList :: [R.Remark] -> App ()
setRemarkList = do
  writeRef' remarkList

insertToGlobalRemarkList :: [R.Remark] -> App ()
insertToGlobalRemarkList remarkList = do
  modifyRef' globalRemarkList $ (++) remarkList

setGlobalRemarkList :: [R.Remark] -> App ()
setGlobalRemarkList = do
  writeRef' globalRemarkList

getGlobalRemarkList :: App [R.Remark]
getGlobalRemarkList = do
  readRef' globalRemarkList
