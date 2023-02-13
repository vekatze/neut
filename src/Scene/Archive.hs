module Scene.Archive
  ( Context,
    archive,
  )
where

import Context.Env qualified as Env
import Context.External qualified as External
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Control.Monad
import Control.Monad.Catch
import Data.Text qualified as T
import Entity.Module
import Path
import Prelude hiding (log)

class
  ( Env.Context m,
    Throw.Context m,
    Path.Context m,
    External.Context m,
    MonadThrow m
  ) =>
  Context m

archive :: Context m => T.Text -> [FilePath] -> m ()
archive releaseName contents = do
  mainModule <- Env.getMainModule
  let moduleRootDir = parent $ moduleLocation mainModule
  outputPath <- toFilePath <$> getReleaseFile mainModule releaseName
  let tarRootDir = toFilePath $ parent moduleRootDir
  External.run "tar" $ ["-c", "--zstd", "-f", outputPath, "-C", tarRootDir] ++ contents

getReleaseFile :: Context m => Module -> T.Text -> m (Path Abs File)
getReleaseFile targetModule releaseName = do
  let releaseDir = getReleaseDir targetModule
  Path.ensureDir releaseDir
  releaseFile <- Path.resolveFile releaseDir $ T.unpack $ releaseName <> ".tar.zst"
  releaseExists <- Path.doesFileExist releaseFile
  when releaseExists $ do
    Throw.raiseError' $ "the release `" <> releaseName <> "` already exists"
  return releaseFile
