module Context.Throw
  ( Context (..),
    Config (..),
    raiseError,
    raiseError',
    raiseCritical,
    raiseCritical',
    raiseSyntaxError,
    raiseIfProcessFailed,
    run,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Entity.FilePos
import Entity.Hint
import Entity.Log
import GHC.IO.Handle
import System.Exit

data Context = Context
  { throw :: forall a. Error -> IO a,
    try :: forall a. IO a -> IO (Either Error a)
  }

data Config = Config {}

run :: Context -> (Log -> IO ()) -> IO a -> IO a
run throwCtx printer c = do
  resultOrErr <- try throwCtx c
  case resultOrErr of
    Left (Error err) ->
      foldr ((>>) . printer) (exitWith (ExitFailure 1)) err
    Right result ->
      return result

raiseError :: Context -> Hint -> T.Text -> IO a
raiseError ctx m text =
  throw ctx $ Error [logError (Entity.FilePos.fromHint m) text]

raiseError' :: Context -> T.Text -> IO a
raiseError' ctx text =
  throw ctx $ Error [logError' text]

raiseCritical :: Context -> Hint -> T.Text -> IO a
raiseCritical ctx m text =
  throw ctx $ Error [logCritical (Entity.FilePos.fromHint m) text]

raiseCritical' :: Context -> T.Text -> IO a
raiseCritical' ctx text =
  throw ctx $ Error [logCritical' text]

raiseSyntaxError :: Context -> Hint -> T.Text -> IO a
raiseSyntaxError ctx m form =
  raiseError ctx m $ "couldn't match the input with the expected form: " <> form

raiseIfProcessFailed :: Context -> T.Text -> ExitCode -> Handle -> IO ()
raiseIfProcessFailed ctx procName exitCode h =
  case exitCode of
    ExitSuccess ->
      return ()
    ExitFailure i -> do
      errStr <- TIO.hGetContents h
      raiseError' ctx $
        "the child process `"
          <> procName
          <> "` failed with the following message (exitcode = "
          <> T.pack (show i)
          <> "):\n"
          <> errStr
