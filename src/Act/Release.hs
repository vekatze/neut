module Act.Release (release) where

import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Entity.Log (raiseError')
import Entity.Module (Module (moduleExtraContents, moduleLocation), SomePath, getMainModule, getReleaseDir, getSourceDir)
import GHC.IO.Exception (ExitCode (..))
import Path
  ( Abs,
    Dir,
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
  extra <- mapM (arrangeExtraContentPath tarRootDir) $ moduleExtraContents mainModule
  let tarCmd =
        proc
          "tar"
          $ [ "-c",
              "--zstd",
              "-f",
              toFilePath releaseFile,
              "-C",
              toFilePath tarRootDir,
              toFilePath relModuleSourceDir,
              toFilePath relModuleFile
            ]
            ++ extra
  (_, _, Just tarErrorHandler, handler) <- createProcess tarCmd {std_err = CreatePipe}
  tarExitCode <- waitForProcess handler
  raiseIfFailure "tar" tarExitCode tarErrorHandler

arrangeExtraContentPath :: Path Abs Dir -> SomePath -> IO FilePath
arrangeExtraContentPath tarRootDir somePath =
  case somePath of
    Left dirPath ->
      toFilePath <$> stripProperPrefix tarRootDir dirPath
    Right filePath ->
      toFilePath <$> stripProperPrefix tarRootDir filePath

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
