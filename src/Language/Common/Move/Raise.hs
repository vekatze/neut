module Language.Common.Move.Raise
  ( raiseError,
    raiseError',
    raiseCritical,
    raiseCritical',
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Text qualified as T
import Error.Rule.EIO
import Language.Common.Rule.Error qualified as E
import Language.Common.Rule.Hint (Hint)

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
