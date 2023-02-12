module Context.Case.Main.Throw
  ( throw,
    try,
    run,
    Context,
  )
where

import Context.Log qualified as Log
import Context.Throw qualified as Throw
import Control.Exception.Safe qualified as Safe
import Control.Monad.IO.Class
import Entity.Log qualified as L
import System.Exit

class
  ( Throw.Context m,
    Log.Context m,
    MonadIO m,
    Safe.MonadThrow m,
    Safe.MonadCatch m
  ) =>
  Context m

throw :: Context m => L.Error -> m a
throw =
  Safe.throw

try :: Context m => m a -> m (Either L.Error a)
try =
  Safe.try

run :: (Throw.Context m, Log.Context m, MonadIO m) => m a -> m a
run c = do
  resultOrErr <- Throw.try c
  case resultOrErr of
    Left (L.MakeError err) ->
      foldr ((>>) . Log.printLog) (liftIO $ exitWith (ExitFailure 1)) err
    Right result ->
      return result
