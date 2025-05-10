module Aux.Error.Move.Run
  ( runEIO,
    run,
    forP,
    forP_,
    liftMaybe,
    raiseError,
    raiseError',
    raiseCritical,
    raiseCritical',
  )
where

import Aux.Error.Rule.EIO
import Aux.Error.Rule.Error qualified as E
import Aux.Logger.Move.Log qualified as Logger
import Aux.Logger.Rule.Handle qualified as Logger
import Aux.Logger.Rule.Hint
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (lefts, partitionEithers)
import Data.Text qualified as T
import System.Exit
import UnliftIO.Async (pooledForConcurrently)

runEIO :: EIO a -> IO (Either E.Error a)
runEIO =
  runExceptT

run :: Logger.Handle -> EIO a -> IO a
run loggerHandle c = do
  resultOrErr <- liftIO $ runEIO c
  case resultOrErr of
    Left (E.MakeError err) -> do
      liftIO $ Logger.printErrorList loggerHandle err
      liftIO $ exitWith (ExitFailure 1)
    Right result ->
      return result

forP :: [a] -> (a -> EIO b) -> EIO [b]
forP xs f = do
  xs' <- liftIO $ pooledForConcurrently xs (runEIO . f)
  let (errors, results) = partitionEithers xs'
  if null errors
    then return results
    else throwError $ foldl (<>) (E.MakeError []) errors

forP_ :: [a] -> (a -> EIO ()) -> EIO ()
forP_ xs f = do
  xs' <- liftIO $ pooledForConcurrently xs (runEIO . f)
  let errors = lefts xs'
  if null errors
    then return ()
    else throwError $ foldl (<>) (E.MakeError []) errors

liftMaybe :: Maybe a -> EIO a
liftMaybe m =
  case m of
    Nothing ->
      throwError (E.MakeError [])
    Just v ->
      return v

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
