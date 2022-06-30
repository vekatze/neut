module Context.Throw
  ( Context (..),
    raiseError,
    raiseError',
    raiseCritical,
    raiseCritical',
    raiseSyntaxError,
  )
where

import qualified Data.Text as T
import Entity.FilePos
import Entity.Hint
import Entity.Log

data Context = Context
  { throw :: forall a. Error -> IO a,
    try :: forall a. IO a -> IO (Either Error a)
  }

raiseError :: Context -> Hint -> T.Text -> IO a
raiseError ctx m text =
  throw ctx $ Error [logError (Entity.FilePos.fromHint m) text]

raiseError' :: Context -> T.Text -> IO a
raiseError' ctx text =
  throw ctx $ Error [logError' text]

raiseCritical :: Context -> Hint -> T.Text -> IO a
raiseCritical ctx m text =
  throw ctx $ Error [logCritical (Entity.FilePos.fromHint m) text]

raiseCritical' :: Context -> T.Text -> IO a
raiseCritical' ctx text =
  throw ctx $ Error [logCritical' text]

raiseSyntaxError :: Context -> Hint -> T.Text -> IO a
raiseSyntaxError ctx m form =
  raiseError ctx m $ "couldn't match the input with the expected form: " <> form
