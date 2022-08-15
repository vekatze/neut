module Context.Log
  ( Context (..),
    Config (..),
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

import qualified Context.Env as Env
import qualified Data.Text as T
import qualified Entity.FilePos as FilePos
import Entity.Hint
import Entity.Log

data Config = Config
  { shouldColorize :: Bool,
    endOfEntry :: T.Text
  }

class Env.Context m => Context m where
  printLog :: Log -> m ()

printNote :: Context m => Hint -> T.Text -> m ()
printNote =
  printLogWithFilePos LogLevelNote

printNote' :: Context m => T.Text -> m ()
printNote' =
  printLogWithoutFilePos LogLevelNote

printWarning :: Context m => Hint -> T.Text -> m ()
printWarning =
  printLogWithFilePos LogLevelWarning

printWarning' :: Context m => T.Text -> m ()
printWarning' =
  printLogWithoutFilePos LogLevelWarning

printError :: Context m => Hint -> T.Text -> m ()
printError =
  printLogWithFilePos LogLevelError

printError' :: Context m => T.Text -> m ()
printError' =
  printLogWithoutFilePos LogLevelError

printCritical :: Context m => Hint -> T.Text -> m ()
printCritical =
  printLogWithFilePos LogLevelCritical

printCritical' :: Context m => T.Text -> m ()
printCritical' =
  printLogWithoutFilePos LogLevelCritical

printPass :: Context m => Hint -> T.Text -> m ()
printPass =
  printLogWithFilePos LogLevelPass

printPass' :: Context m => T.Text -> m ()
printPass' =
  printLogWithoutFilePos LogLevelPass

printFail :: Context m => Hint -> T.Text -> m ()
printFail =
  printLogWithFilePos LogLevelFail

printFail' :: Context m => T.Text -> m ()
printFail' =
  printLogWithoutFilePos LogLevelFail

printLogWithFilePos :: Context m => LogLevel -> Hint -> T.Text -> m ()
printLogWithFilePos level m txt = do
  printLog (Just (FilePos.fromHint m), level, txt)

printLogWithoutFilePos :: Context m => LogLevel -> T.Text -> m ()
printLogWithoutFilePos level txt =
  printLog (Nothing, level, txt)
