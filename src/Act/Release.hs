module Act.Release
  ( release,
    Config (..),
    Context,
  )
where

import qualified Context.Env as Env
import qualified Context.External as External
import qualified Context.Log as Log
import qualified Context.Path as Path
import qualified Context.Throw as Throw
import Control.Monad
import qualified Data.Text as T
import Entity.Module
import qualified Entity.Module.Reflect as Module
import Path
import qualified Scene.Parse.Core as ParseCore

data Config = Config
  { getReleaseName :: T.Text,
    logCfg :: Log.Config
  }

class
  ( Throw.Context m,
    Log.Context m,
    Path.Context m,
    External.Context m,
    ParseCore.Context m
  ) =>
  Context m

release :: Context m => Config -> m ()
release cfg = do
  Env.setEndOfEntry $ Log.endOfEntry $ logCfg cfg
  Env.setShouldColorize $ Log.shouldColorize $ logCfg cfg
  Throw.run $ do
    mainModule <- Module.fromCurrentPath
    let moduleRootDir = parent $ moduleLocation mainModule
    releaseFile <- getReleaseFile mainModule (getReleaseName cfg)
    let tarRootDir = parent moduleRootDir
    relModuleSourceDir <- Path.stripPrefix tarRootDir $ getSourceDir mainModule
    relModuleFile <- Path.stripPrefix tarRootDir $ moduleLocation mainModule
    extraContents <- mapM (arrangeExtraContentPath tarRootDir) $ moduleExtraContents mainModule
    External.run "tar" $
      [ "-c",
        "--zstd",
        "-f",
        toFilePath releaseFile,
        "-C",
        toFilePath tarRootDir,
        toFilePath relModuleSourceDir,
        toFilePath relModuleFile
      ]
        ++ extraContents

arrangeExtraContentPath :: Context m => Path Abs Dir -> SomePath -> m FilePath
arrangeExtraContentPath tarRootDir somePath =
  case somePath of
    Left dirPath ->
      toFilePath <$> Path.stripPrefix tarRootDir dirPath
    Right filePath ->
      toFilePath <$> Path.stripPrefix tarRootDir filePath

getReleaseFile :: Context m => Module -> T.Text -> m (Path Abs File)
getReleaseFile targetModule releaseName = do
  let releaseDir = getReleaseDir targetModule
  Path.ensureDir releaseDir
  releaseFile <- Path.resolveFile releaseDir $ T.unpack $ releaseName <> ".tar.zst"
  releaseExists <- Path.doesFileExist releaseFile
  when releaseExists $ do
    Throw.raiseError' $ "the release `" <> releaseName <> "` already exists"
  return releaseFile
