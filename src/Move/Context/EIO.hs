module Move.Context.EIO (EIO, toApp) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Context.App (App)
import Move.Context.Throw (throw)
import Rule.Error qualified as E

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
