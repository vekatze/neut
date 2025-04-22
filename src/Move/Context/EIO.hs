module Move.Context.EIO
  ( EIO,
    toApp,
    raiseError,
    raiseError',
    raiseCritical,
    raiseCritical',
  )
where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as T
import Move.Context.App (App)
import Move.Context.Throw (throw)
import Rule.Error qualified as E
import Rule.Hint (Hint)

type EIO = ExceptT E.Error IO

-- temporary
toApp :: EIO a -> App a
toApp comp = do
  errOrResult <- liftIO $ runExceptT comp
  case errOrResult of
    Left err ->
      throw err
    Right result ->
      return result

raiseError :: Hint -> T.Text -> EIO a
raiseError m t =
  throwError $ E.newError m t

raiseError' :: T.Text -> EIO a
raiseError' t =
  throwError $ E.newError' t

raiseCritical :: Hint -> T.Text -> EIO a
raiseCritical m t =
  throwError $ E.newCritical m t

raiseCritical' :: T.Text -> EIO a
raiseCritical' t =
  throwError $ E.newCritical' t
