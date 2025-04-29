module Move.Context.Throw
  ( throw,
    execute,
    run,
    runMaybe,
    runEither,
    collectLogs,
    raiseError,
    raiseError',
    raiseCritical,
  )
where

import Control.Exception.Safe qualified as Safe
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Text qualified as T
import Move.Console.Report qualified as Report
import Move.Context.App
import Move.UI.Handle.GlobalRemark qualified as GlobalRemark
import Rule.Error qualified as E
import Rule.Hint
import Rule.Remark qualified as R
import System.Exit

throw :: forall a. E.Error -> App a
throw =
  Safe.throw

execute :: App a -> App (Either E.Error a)
execute c = do
  Safe.try $ wrappingExternalExceptions c

run :: Report.Handle -> App a -> App a
run reportHandle c = do
  resultOrErr <- execute c
  case resultOrErr of
    Left (E.MakeError err) -> do
      liftIO $ Report.printErrorList reportHandle err
      liftIO $ exitWith (ExitFailure 1)
    Right result ->
      return result

runMaybe :: App a -> App (Maybe a)
runMaybe c = do
  resultOrErr <- execute c
  case resultOrErr of
    Left _ -> do
      return Nothing
    Right result ->
      return $ Just result

runEither :: GlobalRemark.Handle -> App a -> App (Either [R.Remark] a)
runEither h c = do
  resultOrErr <- execute c
  remarkList <- liftIO $ GlobalRemark.get h
  case resultOrErr of
    Left (E.MakeError logList) ->
      return $ Left $ logList ++ remarkList
    Right v ->
      return $ Right v

collectLogs :: GlobalRemark.Handle -> App () -> App [R.Remark]
collectLogs h c = do
  resultOrErr <- execute c
  remarkList <- liftIO $ GlobalRemark.get h
  case resultOrErr of
    Left (E.MakeError logList) ->
      return $ logList ++ remarkList
    Right _ ->
      return remarkList

wrappingExternalExceptions :: App a -> App a
wrappingExternalExceptions comp = do
  catches
    comp
    [ Handler $ \(err :: E.Error) -> do
        throw err,
      Handler $ \(someException :: SomeException) -> do
        raiseError' $ T.pack $ show someException
    ]

raiseError :: Hint -> T.Text -> App a
raiseError m text =
  throw $ E.newError m text

raiseError' :: T.Text -> App a
raiseError' text =
  throw $ E.newError' text

raiseCritical :: Hint -> T.Text -> App a
raiseCritical m text =
  throw $ E.newCritical m text
