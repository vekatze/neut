module Context.Throw
  ( Context (..),
    raiseError,
    raiseError',
    raiseCritical,
    raiseCritical',
    raiseSyntaxError,
  )
where

import Data.Text qualified as T
import Entity.FilePos
import Entity.Hint
import Entity.Log qualified as L

class Monad m => Context m where
  throw :: forall a. L.Error -> m a
  try :: forall a. m a -> m (Either L.Error a)
  run :: m a -> m a

raiseError :: Context m => Hint -> T.Text -> m a
raiseError m text =
  throw $ L.MakeError [L.logError (Entity.FilePos.fromHint m) text]

raiseError' :: Context m => T.Text -> m a
raiseError' text =
  throw $ L.MakeError [L.logError' text]

raiseCritical :: Context m => Hint -> T.Text -> m a
raiseCritical m text =
  throw $ L.MakeError [L.logCritical (Entity.FilePos.fromHint m) text]

raiseCritical' :: Context m => T.Text -> m a
raiseCritical' text =
  throw $ L.MakeError [L.logCritical' text]

raiseSyntaxError :: Context m => Hint -> T.Text -> m a
raiseSyntaxError m form =
  raiseError m $ "couldn't match the input with the expected form: " <> form
