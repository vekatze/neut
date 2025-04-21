module Rule.ProcessRunner.Rule
  ( Spec (..),
    CommandError (..),
    ProcessError (..),
    Input (..),
    Runner (..),
    toCompilerError,
  )
where

import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as L
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Rule.Error (newError')
import Rule.Error qualified as E
import System.Process (CmdSpec (..))

data Spec = Spec
  { cmdspec :: CmdSpec,
    cwd :: Maybe FilePath
  }

data CommandError = CommandError
  { spec :: Spec,
    exitCode :: Int,
    errStr :: B.ByteString
  }

data ProcessError
  = CommandExecutionError CommandError
  | SetupError T.Text

data Input
  = Strict B.ByteString
  | Lazy L.ByteString

type Output =
  B.ByteString

data Runner m = Runner
  { run00 :: Spec -> m (Either ProcessError ()),
    run01 :: Spec -> m (Either ProcessError Output),
    run10 :: Spec -> Input -> m (Either ProcessError ()),
    run11 :: Spec -> Input -> m (Either ProcessError Output)
  }

toCompilerError :: ProcessError -> E.Error
toCompilerError err =
  case err of
    CommandExecutionError (CommandError {spec = Spec {cmdspec}, exitCode, errStr}) -> do
      let name = case cmdspec of ShellCommand c -> c; RawCommand cmdName _ -> cmdName
      newError' $
        "The child process `"
          <> T.pack name
          <> "` failed with the following message (exitcode = "
          <> T.pack (show exitCode)
          <> "):\n"
          <> indent (decodeUtf8 errStr)
    SetupError e ->
      newError' e

indent :: T.Text -> T.Text
indent t =
  T.intercalate "\n" $ map ("  " <>) $ T.splitOn "\n" t
