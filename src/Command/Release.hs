module Command.Release (release) where

import Control.Monad (when)
import Data.Log (raiseError')
import Data.Spec (Spec (specLocation), getMainSpec)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.IO.Exception (ExitCode (..))
import Path
  ( parent,
    stripProperPrefix,
    toFilePath,
  )
import Path.IO (doesFileExist, ensureDir, resolveDir, resolveFile)
import System.IO (Handle)
import System.Process
  ( CreateProcess (std_err),
    StdStream (CreatePipe),
    createProcess,
    proc,
    waitForProcess,
  )

release :: T.Text -> IO ()
release identifier = do
  spec <- getMainSpec
  let projRootPath = parent $ specLocation spec
  let basePath = parent projRootPath
  relProjPath <- stripProperPrefix basePath projRootPath
  releaseDirPath <- resolveDir projRootPath "release"
  ensureDir releaseDirPath
  releaseFilePath <- resolveFile releaseDirPath $ T.unpack $ identifier <> ".tar.zst"
  releaseExists <- doesFileExist releaseFilePath
  when releaseExists $ do
    raiseError' $ "the release `" <> identifier <> "` already exists"
  let tarCmd =
        proc
          "tar"
          [ "-c",
            "--zstd",
            "-f",
            toFilePath releaseFilePath,
            "-C",
            toFilePath basePath,
            toFilePath relProjPath <> "/source",
            toFilePath relProjPath <> "/module.ens"
          ]
  (_, _, Just tarErrorHandler, handler) <- createProcess tarCmd {std_err = CreatePipe}
  tarExitCode <- waitForProcess handler
  raiseIfFailure "tar" tarExitCode tarErrorHandler

raiseIfFailure :: T.Text -> ExitCode -> Handle -> IO ()
raiseIfFailure procName exitCode h =
  case exitCode of
    ExitSuccess ->
      return ()
    ExitFailure i -> do
      errStr <- TIO.hGetContents h
      raiseError' $
        "the child process `"
          <> procName
          <> "` failed with the following message (exitcode = "
          <> T.pack (show i)
          <> "):\n"
          <> errStr
