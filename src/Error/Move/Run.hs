module Error.Move.Run
  ( runEIO,
    run,
    forP,
    liftMaybe,
  )
where

import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (partitionEithers)
import Error.Rule.EIO
import Error.Rule.Error qualified as E
import Logger.Move.Log qualified as Logger
import Logger.Rule.Handle qualified as Logger
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

liftMaybe :: Maybe a -> EIO a
liftMaybe m =
  case m of
    Nothing ->
      throwError (E.MakeError [])
    Just v ->
      return v
