module Context.Throw
  ( throw,
    try,
    run,
    run',
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
import Control.Monad.IO.Class
import Data.Text qualified as T
import Entity.Error qualified as E
import Entity.Hint
import Entity.Remark qualified as R
import System.Exit

throw :: forall a. E.Error -> App a
throw =
  Safe.throw

try :: App a -> App (Either E.Error a)
try =
  Safe.try

run :: App a -> App a
run c = do
  resultOrErr <- try c
  case resultOrErr of
    Left (E.MakeError err) -> do
      Remark.printRemarkList err
      liftIO $ exitWith (ExitFailure 1)
    Right result ->
      return result

run' :: App a -> App a
run' c = do
  resultOrErr <- try c
  case resultOrErr of
    Left (E.MakeError err) -> do
      let err' = map R.deactivatePadding err
      foldr ((>>) . Remark.printRemark) (liftIO $ exitWith (ExitFailure 1)) err'
    Right result ->
      return result

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
