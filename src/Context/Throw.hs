module Context.Throw
  ( throw,
    run,
    collectLogs,
    raiseError,
    raiseError',
    raiseCritical,
    raiseCritical',
    raiseSyntaxError,
    liftEither,
  )
where

import Context.App
import Context.Remark qualified as Remark
import Control.Exception.Safe qualified as Safe
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Text qualified as T
import Entity.Error qualified as E
import Entity.Hint
import Entity.Remark qualified as R
import System.Exit

throw :: forall a. E.Error -> App a
throw =
  Safe.throw

run :: App a -> App a
run c = do
  resultOrErr <- Safe.try $ wrappingExternalExceptions c
  case resultOrErr of
    Left (E.MakeError err) -> do
      Remark.printErrorList err
      liftIO $ exitWith (ExitFailure 1)
    Right result ->
      return result

collectLogs :: App () -> App [R.Remark]
collectLogs c = do
  resultOrErr <- Safe.try $ wrappingExternalExceptions c
  remarkList <- Remark.getGlobalRemarkList
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

raiseCritical' :: T.Text -> App a
raiseCritical' text =
  throw $ E.newCritical' text

raiseSyntaxError :: Hint -> T.Text -> App a
raiseSyntaxError m form =
  raiseError m $ "couldn't match the input with the expected form: " <> form

liftEither :: Either E.Error a -> App a
liftEither errOrResult =
  case errOrResult of
    Left err ->
      throw err
    Right result ->
      return result
