module Act.Release
  ( release,
    Config (..),
  )
where

import qualified Context.Log as Log
import qualified Context.Mode as Mode
import qualified Context.Throw as Throw
import Control.Monad
import Data.Function
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Entity.Module
import qualified Entity.Module.Reflect as Module
import GHC.IO.Exception
import Path
import Path.IO
import System.IO
import System.Process

data Config = Config
  { getReleaseName :: T.Text,
    throwCfg :: Throw.Config,
    logCfg :: Log.Config
  }

release :: Mode.Mode -> Config -> IO ()
release mode cfg = do
  throwCtx <- Mode.throwCtx mode $ throwCfg cfg
  logCtx <- Mode.logCtx mode $ logCfg cfg
  Throw.run throwCtx (Log.printLog logCtx) $ do
    mainModule <- Module.fromCurrentPath throwCtx
    let moduleRootDir = parent $ moduleLocation mainModule
    releaseFile <- getReleaseFile throwCtx mainModule (getReleaseName cfg)
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
    raiseIfFailure throwCtx "tar" tarExitCode tarErrorHandler

arrangeExtraContentPath :: Path Abs Dir -> SomePath -> IO FilePath
arrangeExtraContentPath tarRootDir somePath =
  case somePath of
    Left dirPath ->
      toFilePath <$> stripProperPrefix tarRootDir dirPath
    Right filePath ->
      toFilePath <$> stripProperPrefix tarRootDir filePath

getReleaseFile :: Throw.Context -> Module -> T.Text -> IO (Path Abs File)
getReleaseFile throwCtx targetModule releaseName = do
  let releaseDir = getReleaseDir targetModule
  ensureDir releaseDir
  releaseFile <- resolveFile releaseDir $ T.unpack $ releaseName <> ".tar.zst"
  releaseExists <- doesFileExist releaseFile
  when releaseExists $ do
    throwCtx & Throw.raiseError' $ "the release `" <> releaseName <> "` already exists"
  return releaseFile

raiseIfFailure :: Throw.Context -> T.Text -> ExitCode -> Handle -> IO ()
raiseIfFailure throwCtx procName exitCode h =
  case exitCode of
    ExitSuccess ->
      return ()
    ExitFailure i -> do
      errStr <- TIO.hGetContents h
      Throw.raiseError' throwCtx $
        "the child process `"
          <> procName
          <> "` failed with the following message (exitcode = "
          <> T.pack (show i)
          <> "):\n"
          <> errStr
