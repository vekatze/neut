module Context.Log
  ( Context (..),
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
  )
where

import qualified Data.Text as T
import qualified Entity.FilePos as FilePos
import Entity.Hint
import Entity.Log

newtype Context = Context
  { printLog :: Log -> IO ()
  }

printNote :: Context -> Hint -> T.Text -> IO ()
printNote =
  printLogWithFilePos LogLevelNote

printNote' :: Context -> T.Text -> IO ()
printNote' =
  printLogWithoutFilePos LogLevelNote

printWarning :: Context -> Hint -> T.Text -> IO ()
printWarning =
  printLogWithFilePos LogLevelWarning

printWarning' :: Context -> T.Text -> IO ()
printWarning' =
  printLogWithoutFilePos LogLevelWarning

printError :: Context -> Hint -> T.Text -> IO ()
printError =
  printLogWithFilePos LogLevelError

printError' :: Context -> T.Text -> IO ()
printError' =
  printLogWithoutFilePos LogLevelError

printCritical :: Context -> Hint -> T.Text -> IO ()
printCritical =
  printLogWithFilePos LogLevelCritical

printCritical' :: Context -> T.Text -> IO ()
printCritical' =
  printLogWithoutFilePos LogLevelCritical

printPass :: Context -> Hint -> T.Text -> IO ()
printPass =
  printLogWithFilePos LogLevelPass

printPass' :: Context -> T.Text -> IO ()
printPass' =
  printLogWithoutFilePos LogLevelPass

printFail :: Context -> Hint -> T.Text -> IO ()
printFail =
  printLogWithFilePos LogLevelFail

printFail' :: Context -> T.Text -> IO ()
printFail' =
  printLogWithoutFilePos LogLevelFail

printLogWithFilePos :: LogLevel -> Context -> Hint -> T.Text -> IO ()
printLogWithFilePos level logCtx m txt = do
  printLog logCtx (Just (FilePos.fromHint m), level, txt)

printLogWithoutFilePos :: LogLevel -> Context -> T.Text -> IO ()
printLogWithoutFilePos level logCtx txt =
  printLog logCtx (Nothing, level, txt)
