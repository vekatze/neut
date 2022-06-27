module Context.Log
  ( LogContext (..),
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

newtype LogContext m = LogContext
  { printLog :: Log -> m ()
  }

printNote :: LogContext m -> Hint -> T.Text -> m ()
printNote =
  printLogWithFilePos LogLevelNote

printNote' :: LogContext m -> T.Text -> m ()
printNote' =
  printLogWithoutFilePos LogLevelNote

printWarning :: LogContext m -> Hint -> T.Text -> m ()
printWarning =
  printLogWithFilePos LogLevelWarning

printWarning' :: LogContext m -> T.Text -> m ()
printWarning' =
  printLogWithoutFilePos LogLevelWarning

printError :: LogContext m -> Hint -> T.Text -> m ()
printError =
  printLogWithFilePos LogLevelError

printError' :: LogContext m -> T.Text -> m ()
printError' =
  printLogWithoutFilePos LogLevelError

printCritical :: LogContext m -> Hint -> T.Text -> m ()
printCritical =
  printLogWithFilePos LogLevelCritical

printCritical' :: LogContext m -> T.Text -> m ()
printCritical' =
  printLogWithoutFilePos LogLevelCritical

printPass :: LogContext m -> Hint -> T.Text -> m ()
printPass =
  printLogWithFilePos LogLevelPass

printPass' :: LogContext m -> T.Text -> m ()
printPass' =
  printLogWithoutFilePos LogLevelPass

printFail :: LogContext m -> Hint -> T.Text -> m ()
printFail =
  printLogWithFilePos LogLevelFail

printFail' :: LogContext m -> T.Text -> m ()
printFail' =
  printLogWithoutFilePos LogLevelFail

printLogWithFilePos :: LogLevel -> LogContext m -> Hint -> T.Text -> m ()
printLogWithFilePos level logCtx m txt =
  printLog logCtx (Just (FilePos.fromHint m), level, txt)

printLogWithoutFilePos :: LogLevel -> LogContext m -> T.Text -> m ()
printLogWithoutFilePos level logCtx txt =
  printLog logCtx (Nothing, level, txt)
