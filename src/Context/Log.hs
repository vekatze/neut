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

import Context.Env qualified as Env
import Data.Text qualified as T
import Entity.Config.Log
import Entity.FilePos qualified as FilePos
import Entity.Hint
import Entity.Log qualified as L

class Env.Context m => Context m where
  printLog :: L.Log -> m ()

printNote :: Context m => Hint -> T.Text -> m ()
printNote =
  printLogWithFilePos L.Note

printNote' :: Context m => T.Text -> m ()
printNote' =
  printLogWithoutFilePos L.Note

printWarning :: Context m => Hint -> T.Text -> m ()
printWarning =
  printLogWithFilePos L.Warning

printWarning' :: Context m => T.Text -> m ()
printWarning' =
  printLogWithoutFilePos L.Warning

printError :: Context m => Hint -> T.Text -> m ()
printError =
  printLogWithFilePos L.Error

printError' :: Context m => T.Text -> m ()
printError' =
  printLogWithoutFilePos L.Error

printCritical :: Context m => Hint -> T.Text -> m ()
printCritical =
  printLogWithFilePos L.Critical

printCritical' :: Context m => T.Text -> m ()
printCritical' =
  printLogWithoutFilePos L.Critical

printPass :: Context m => Hint -> T.Text -> m ()
printPass =
  printLogWithFilePos L.Pass

printPass' :: Context m => T.Text -> m ()
printPass' =
  printLogWithoutFilePos L.Pass

printFail :: Context m => Hint -> T.Text -> m ()
printFail =
  printLogWithFilePos L.Fail

printFail' :: Context m => T.Text -> m ()
printFail' =
  printLogWithoutFilePos L.Fail

printLogWithFilePos :: Context m => L.LogLevel -> Hint -> T.Text -> m ()
printLogWithFilePos level m txt = do
  printLog (Just (FilePos.fromHint m), level, txt)

printLogWithoutFilePos :: Context m => L.LogLevel -> T.Text -> m ()
printLogWithoutFilePos level txt =
  printLog (Nothing, level, txt)
