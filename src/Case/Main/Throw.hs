module Case.Main.Throw
  ( throw,
    try,
    run,
    Context,
  )
where

import qualified Context.Log as Log
import qualified Context.Throw as Throw
import qualified Control.Exception.Safe as Safe
import Control.Monad.IO.Class
import Entity.Log
import System.Exit

class
  ( Throw.Context m,
    Log.Context m,
    MonadIO m,
    Safe.MonadThrow m,
    Safe.MonadCatch m
  ) =>
  Context m

throw :: Context m => Error -> m a
throw =
  Safe.throw

try :: Context m => m a -> m (Either Error a)
try =
  Safe.try

run :: (Throw.Context m, Log.Context m, MonadIO m) => m a -> m a
run c = do
  resultOrErr <- Throw.try c
  case resultOrErr of
    Left (Error err) ->
      foldr ((>>) . Log.printLog) (liftIO $ exitWith (ExitFailure 1)) err
    Right result ->
      return result
