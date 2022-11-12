module Context.Throw
  ( Context (..),
    raiseError,
    raiseError',
    raiseCritical,
    raiseCritical',
    raiseSyntaxError,
  )
where

-- import qualified Context.Env as Env
import qualified Data.Text as T
import Entity.FilePos
import Entity.Hint
import qualified Entity.Log as L

class Monad m => Context m where
  throw :: forall a. L.Error -> m a
  try :: forall a. m a -> m (Either L.Error a)
  run :: m a -> m a

-- run :: Context m => (Log -> m ()) -> m a -> m a
-- run printer c = do
--   resultOrErr <- try c
--   case resultOrErr of
--     Left (Error err) ->
--       foldr ((>>) . printer) (exitWith (ExitFailure 1)) err
--     Right result ->
--       return result

raiseError :: Context m => Hint -> T.Text -> m a
raiseError m text =
  throw $ L.MakeError [L.logError (Entity.FilePos.fromHint m) text]

raiseError' :: Context m => T.Text -> m a
raiseError' text =
  throw $ L.MakeError [L.logError' text]

raiseCritical :: Context m => Hint -> T.Text -> m a
raiseCritical m text =
  throw $ L.MakeError [L.logCritical (Entity.FilePos.fromHint m) text]

raiseCritical' :: Context m => T.Text -> m a
raiseCritical' text =
  throw $ L.MakeError [L.logCritical' text]

raiseSyntaxError :: Context m => Hint -> T.Text -> m a
raiseSyntaxError m form =
  raiseError m $ "couldn't match the input with the expected form: " <> form

-- raiseError :: Context -> Hint -> T.Text -> IO a
-- raiseError ctx m text =
--   throw ctx $ Error [logError (Entity.FilePos.fromHint m) text]

-- raiseError' :: Context -> T.Text -> IO a
-- raiseError' ctx text =
--   throw ctx $ Error [logError' text]

-- raiseCritical :: Context -> Hint -> T.Text -> IO a
-- raiseCritical ctx m text =
--   throw ctx $ Error [logCritical (Entity.FilePos.fromHint m) text]

-- raiseCritical' :: Context -> T.Text -> IO a
-- raiseCritical' ctx text =
--   throw ctx $ Error [logCritical' text]

-- raiseSyntaxError :: Context -> Hint -> T.Text -> IO a
-- raiseSyntaxError ctx m form =
--   raiseError ctx m $ "couldn't match the input with the expected form: " <> form

-- raiseIfProcessFailed :: Context -> T.Text -> ExitCode -> Handle -> IO ()
-- raiseIfProcessFailed ctx procName exitCode h =
--   case exitCode of
--     ExitSuccess ->
--       return ()
--     ExitFailure i -> do
--       errStr <- TIO.hGetContents h
--       raiseError' ctx $
--         "the child process `"
--           <> procName
--           <> "` failed with the following message (exitcode = "
--           <> T.pack (show i)
--           <> "):\n"
--           <> errStr
