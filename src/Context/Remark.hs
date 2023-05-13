module Context.Remark
  ( initialize,
    insertRemark,
    getRemarkList,
    printString,
    printRemark,
    printRemarkList,
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
import Entity.Remark qualified as R
import System.Console.ANSI

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
  printRemark (Just (FilePos.fromHint m), True, level, txt)

printRemarkWithoutFilePos :: R.RemarkLevel -> T.Text -> App ()
printRemarkWithoutFilePos level txt =
  printRemark (Nothing, True, level, txt)

printRemarkIO :: R.Remark -> App ()
printRemarkIO (mpos, shouldInsertPadding, l, t) = do
  outputRemarkLocation mpos
  outputRemarkLevel l
  outputRemarkText t (remarkLevelToPad shouldInsertPadding l)
  outputFooter

outputRemarkLocation :: Maybe FilePos -> App ()
outputRemarkLocation mpos = do
  case mpos of
    Just pos ->
      withSGR [SetConsoleIntensity BoldIntensity] $ do
        liftIO $ TIO.putStr $ T.pack (showFilePos pos)
        liftIO $ TIO.putStr "\n"
    _ ->
      return ()

outputFooter :: App ()
outputFooter = do
  eoe <- getEndOfEntry
  if eoe == ""
    then return ()
    else liftIO $ TIO.putStrLn eoe

outputRemarkLevel :: R.RemarkLevel -> App ()
outputRemarkLevel l =
  withSGR (R.remarkLevelToSGR l) $ do
    liftIO $ TIO.putStr $ R.remarkLevelToText l
    liftIO $ TIO.putStr ": "

outputRemarkText :: T.Text -> App T.Text -> App ()
outputRemarkText str padComp = do
  pad <- padComp
  liftIO $ TIO.putStrLn $ stylizeRemarkText str pad

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

insertRemark :: R.Remark -> App ()
insertRemark r = do
  modifyRef' remarkList $ (:) r

getRemarkList :: App [R.Remark]
getRemarkList = do
  readRef' remarkList
