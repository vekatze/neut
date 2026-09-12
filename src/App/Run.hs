module App.Run
  ( runApp,
    tryApp,
    run,
    forP,
    forP_,
    liftMaybe,
    raiseError,
    raiseError',
    raiseCritical,
    raiseCritical',
    onFailure,
  )
where

import App.App
import App.Error qualified as E
import Control.Exception (IOException, catch, onException)
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (lefts, partitionEithers)
import Data.Text qualified as T
import Logger.Handle qualified as Logger
import Logger.Hint
import Logger.Print qualified as Logger
import System.Exit
import UnliftIO.Async (pooledForConcurrently)

runApp :: App a -> IO (Either E.Error a)
runApp =
  runExceptT

tryApp :: App a -> IO (Either E.Error a)
tryApp act =
  runApp act `catch` (return . Left . asError)

run :: Logger.Handle -> App a -> IO a
run loggerHandle c = do
  resultOrErr <- tryApp c
  case resultOrErr of
    Left (E.MakeError err) -> do
      liftIO $ Logger.printLogList loggerHandle err
      liftIO $ exitWith (ExitFailure 1)
    Right result ->
      return result

asError :: IOException -> E.Error
asError e =
  E.newError' $ T.pack $ show e

forP :: [a] -> (a -> App b) -> App [b]
forP xs f = do
  xs' <- liftIO $ pooledForConcurrently xs (runApp . f)
  let (errors, results) = partitionEithers xs'
  if null errors
    then return results
    else throwError $ foldl (<>) (E.MakeError []) errors

forP_ :: [a] -> (a -> App ()) -> App ()
forP_ xs f = do
  xs' <- liftIO $ pooledForConcurrently xs (runApp . f)
  let errors = lefts xs'
  if null errors
    then return ()
    else throwError $ foldl (<>) (E.MakeError []) errors

liftMaybe :: Maybe a -> App a
liftMaybe m =
  case m of
    Nothing ->
      throwError (E.MakeError [])
    Just v ->
      return v

raiseError :: Hint -> T.Text -> App a
raiseError m t =
  throwError $ E.newError m t

raiseError' :: T.Text -> App a
raiseError' t =
  throwError $ E.newError' t

raiseCritical :: Hint -> T.Text -> App a
raiseCritical m t =
  throwError $ E.newCritical m t

raiseCritical' :: T.Text -> App a
raiseCritical' t =
  throwError $ E.newCritical' t

onFailure :: App a -> IO () -> App a
onFailure act cleanup = do
  resultOrError <- liftIO $ runApp act `onException` cleanup
  case resultOrError of
    Left err -> do
      liftIO cleanup
      throwError err
    Right result ->
      return result
