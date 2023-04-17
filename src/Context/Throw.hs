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
import Context.Log qualified as Log
import Control.Exception.Safe qualified as Safe
import Control.Monad.IO.Class
import Data.Text qualified as T
import Entity.FilePos
import Entity.Hint
import Entity.Log qualified as L
import System.Exit

throw :: forall a. L.Error -> App a
throw =
  Safe.throw

try :: App a -> App (Either L.Error a)
try =
  Safe.try

run :: App a -> App a
run c = do
  resultOrErr <- try c
  case resultOrErr of
    Left (L.MakeError err) -> do
      Log.printLogList err
      liftIO $ exitWith (ExitFailure 1)
    Right result ->
      return result

run' :: App a -> App a
run' c = do
  resultOrErr <- try c
  case resultOrErr of
    Left (L.MakeError err) -> do
      let err' = map L.deactivatePadding err
      foldr ((>>) . Log.printLog) (liftIO $ exitWith (ExitFailure 1)) err'
    Right result ->
      return result

raiseError :: Hint -> T.Text -> App a
raiseError m text =
  throw $ L.MakeError [L.logError (Entity.FilePos.fromHint m) text]

raiseError' :: T.Text -> App a
raiseError' text =
  throw $ L.MakeError [L.logError' text]

raiseCritical :: Hint -> T.Text -> App a
raiseCritical m text =
  throw $ L.MakeError [L.logCritical (Entity.FilePos.fromHint m) text]

raiseCritical' :: T.Text -> App a
raiseCritical' text =
  throw $ L.MakeError [L.logCritical' text]

raiseSyntaxError :: Hint -> T.Text -> App a
raiseSyntaxError m form =
  raiseError m $ "couldn't match the input with the expected form: " <> form

liftEither :: Either L.Error a -> App a
liftEither errOrResult =
  case errOrResult of
    Left err ->
      throw err
    Right result ->
      return result
