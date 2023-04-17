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
import Entity.FilePos
import Entity.Hint
import Entity.Remark qualified as R
import System.Exit

throw :: forall a. R.Error -> App a
throw =
  Safe.throw

try :: App a -> App (Either R.Error a)
try =
  Safe.try

run :: App a -> App a
run c = do
  resultOrErr <- try c
  case resultOrErr of
    Left (R.MakeError err) -> do
      Remark.printRemarkList err
      liftIO $ exitWith (ExitFailure 1)
    Right result ->
      return result

run' :: App a -> App a
run' c = do
  resultOrErr <- try c
  case resultOrErr of
    Left (R.MakeError err) -> do
      let err' = map R.deactivatePadding err
      foldr ((>>) . Remark.printRemark) (liftIO $ exitWith (ExitFailure 1)) err'
    Right result ->
      return result

raiseError :: Hint -> T.Text -> App a
raiseError m text =
  throw $ R.MakeError [R.remarkError (Entity.FilePos.fromHint m) text]

raiseError' :: T.Text -> App a
raiseError' text =
  throw $ R.MakeError [R.remarkError' text]

raiseCritical :: Hint -> T.Text -> App a
raiseCritical m text =
  throw $ R.MakeError [R.remarkCritical (Entity.FilePos.fromHint m) text]

raiseCritical' :: T.Text -> App a
raiseCritical' text =
  throw $ R.MakeError [R.remarkCritical' text]

raiseSyntaxError :: Hint -> T.Text -> App a
raiseSyntaxError m form =
  raiseError m $ "couldn't match the input with the expected form: " <> form

liftEither :: Either R.Error a -> App a
liftEither errOrResult =
  case errOrResult of
    Left err ->
      throw err
    Right result ->
      return result
