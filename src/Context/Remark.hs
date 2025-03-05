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
    setShouldColorize,
    getShouldColorize,
    withSGR,
  )
where

import Context.App
import Context.App.Internal
import Control.Monad.IO.Class
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding
import Entity.FilePos
import Entity.FilePos qualified as FilePos
import Entity.Hint
import Entity.Remark qualified as R
import System.Console.ANSI.Codes
import System.IO

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
  locText <- getRemarkLocation mpos
  levelText <- getRemarkLevel l
  remarkText <- getRemarkText t (remarkLevelToPad shouldInsertPadding l)
  footerText <- getFooter
  liftIO $ B.putStr $ encodeUtf8 $ locText <> levelText <> remarkText <> footerText

printErrorIO :: R.Remark -> App ()
printErrorIO (mpos, shouldInsertPadding, l, t) = do
  locText <- getRemarkLocation mpos
  levelText <- getRemarkLevel l
  remarkText <- getRemarkText t (remarkLevelToPad shouldInsertPadding l)
  footerText <- getFooter
  liftIO $ B.hPutStr stderr $ encodeUtf8 $ locText <> levelText <> remarkText <> footerText

getRemarkLocation :: Maybe FilePos -> App T.Text
getRemarkLocation mpos = do
  case mpos of
    Just pos -> do
      withSGR [SetConsoleIntensity BoldIntensity] $ T.pack $ showFilePos pos ++ "\n"
    _ ->
      return ""

getFooter :: App T.Text
getFooter = do
  eoe <- getEndOfEntry
  if eoe == ""
    then return ""
    else return $ eoe <> "\n"

getRemarkLevel :: R.RemarkLevel -> App T.Text
getRemarkLevel l =
  withSGR (R.remarkLevelToSGR l) $ do
    R.remarkLevelToText l <> ": "

getRemarkText :: T.Text -> App T.Text -> App T.Text
getRemarkText str padComp = do
  remarkText <- stylizeRemarkText str <$> padComp
  return $ remarkText <> "\n"

remarkLevelToPad :: R.ShouldInsertPadding -> R.RemarkLevel -> App T.Text
remarkLevelToPad shouldInsertPadding level = do
  if shouldInsertPadding
    then return $ T.replicate (T.length (R.remarkLevelToText level) + 2) " "
    else return ""

stylizeRemarkText :: T.Text -> T.Text -> T.Text
stylizeRemarkText str pad = do
  let ls = T.lines str
  if null ls
    then str
    else T.intercalate "\n" $ head ls : map (pad <>) (tail ls)

withSGR :: [SGR] -> T.Text -> App T.Text
withSGR arg str = do
  shouldColorize <- getShouldColorize
  if shouldColorize
    then return $ T.pack (setSGRCode arg) <> str <> T.pack (setSGRCode [Reset])
    else return str

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
