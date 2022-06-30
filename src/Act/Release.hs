module Act.Release (release) where

import Context.App
import qualified Context.Throw as Throw
import Control.Monad
import Data.Function
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Entity.Module
import GHC.IO.Exception
import Path
import Path.IO
import System.IO
import System.Process

release :: Axis -> T.Text -> IO ()
release axis identifier = do
  mainModule <- getMainModule (axis & throw)
  let moduleRootDir = parent $ moduleLocation mainModule
  releaseFile <- getReleaseFile axis mainModule identifier
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
  raiseIfFailure axis "tar" tarExitCode tarErrorHandler

arrangeExtraContentPath :: Path Abs Dir -> SomePath -> IO FilePath
arrangeExtraContentPath tarRootDir somePath =
  case somePath of
    Left dirPath ->
      toFilePath <$> stripProperPrefix tarRootDir dirPath
    Right filePath ->
      toFilePath <$> stripProperPrefix tarRootDir filePath

getReleaseFile :: Axis -> Module -> T.Text -> IO (Path Abs File)
getReleaseFile axis targetModule identifier = do
  let releaseDir = getReleaseDir targetModule
  ensureDir releaseDir
  releaseFile <- resolveFile releaseDir $ T.unpack $ identifier <> ".tar.zst"
  releaseExists <- doesFileExist releaseFile
  when releaseExists $ do
    axis & throw & Throw.raiseError' $ "the release `" <> identifier <> "` already exists"
  return releaseFile

raiseIfFailure :: Axis -> T.Text -> ExitCode -> Handle -> IO ()
raiseIfFailure axis procName exitCode h =
  case exitCode of
    ExitSuccess ->
      return ()
    ExitFailure i -> do
      errStr <- TIO.hGetContents h
      axis & throw & Throw.raiseError' $
        "the child process `"
          <> procName
          <> "` failed with the following message (exitcode = "
          <> T.pack (show i)
          <> "):\n"
          <> errStr
