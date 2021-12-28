module Command.Release (release) where

import Control.Monad (when)
import Data.Log (raiseError')
import Data.Module (Module (moduleLocation), getMainModule, getReleaseDir, getSourceDir)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.IO.Exception (ExitCode (..))
import Path
  ( Abs,
    File,
    Path,
    parent,
    stripProperPrefix,
    toFilePath,
  )
import Path.IO (doesFileExist, ensureDir, resolveFile)
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
  mainModule <- getMainModule
  let moduleRootDir = parent $ moduleLocation mainModule
  releaseFile <- getReleaseFile mainModule identifier
  let tarRootDir = parent moduleRootDir
  relModuleSourceDir <- stripProperPrefix tarRootDir $ getSourceDir mainModule
  relModuleFile <- stripProperPrefix tarRootDir $ moduleLocation mainModule
  let tarCmd =
        proc
          "tar"
          [ "-c",
            "--zstd",
            "-f",
            toFilePath releaseFile,
            "-C",
            toFilePath tarRootDir,
            toFilePath relModuleSourceDir,
            toFilePath relModuleFile
          ]
  (_, _, Just tarErrorHandler, handler) <- createProcess tarCmd {std_err = CreatePipe}
  tarExitCode <- waitForProcess handler
  raiseIfFailure "tar" tarExitCode tarErrorHandler

getReleaseFile :: Module -> T.Text -> IO (Path Abs File)
getReleaseFile targetModule identifier = do
  let releaseDir = getReleaseDir targetModule
  ensureDir releaseDir
  releaseFile <- resolveFile releaseDir $ T.unpack $ identifier <> ".tar.zst"
  releaseExists <- doesFileExist releaseFile
  when releaseExists $ do
    raiseError' $ "the release `" <> identifier <> "` already exists"
  return releaseFile

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
