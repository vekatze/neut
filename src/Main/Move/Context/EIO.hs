module Main.Move.Context.EIO
  ( EIO,
    run,
    runEIO,
    forP,
    raiseError,
    raiseError',
    raiseCritical,
    raiseCritical',
    liftMaybe,
    collectLogs,
  )
where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (partitionEithers)
import Data.Text qualified as T
import Language.Common.Rule.Error qualified as E
import Language.Common.Rule.Hint (Hint)
import Logger.Move.Log qualified as Logger
import Logger.Rule.Handle qualified as Logger
import Logger.Rule.Log qualified as L
import Main.Move.Context.GlobalRemark qualified as GlobalRemark
import System.Exit
import UnliftIO.Async (pooledForConcurrently)

type EIO = ExceptT E.Error IO

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

collectLogs :: GlobalRemark.Handle -> EIO () -> IO [L.Log]
collectLogs h c = do
  resultOrErr <- runEIO c
  remarkList <- liftIO $ GlobalRemark.get h
  case resultOrErr of
    Left (E.MakeError logList) ->
      return $ logList ++ remarkList
    Right _ ->
      return remarkList

forP :: [a] -> (a -> EIO b) -> EIO [b]
forP xs f = do
  ys' <- liftIO $ pooledForConcurrently xs (runEIO . f)
  let (errors, results) = partitionEithers ys'
  if null errors
    then return results
    else throwError $ foldl (<>) (E.MakeError []) errors

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

liftMaybe :: Maybe a -> EIO a
liftMaybe m =
  case m of
    Nothing ->
      throwError (E.MakeError [])
    Just v ->
      return v
