module Scene.Archive (archive) where

import Context.App
import Context.External qualified as External
import Context.Module qualified as Module
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Control.Monad
import Data.Text qualified as T
import Entity.Module
import Path
import Prelude hiding (log)

archive :: T.Text -> [FilePath] -> App ()
archive releaseName contents = do
  mainModule <- Module.getMainModule
  let moduleRootDir = parent $ moduleLocation mainModule
  outputPath <- toFilePath <$> getReleaseFile mainModule releaseName
  let tarRootDir = toFilePath $ parent moduleRootDir
  External.run "tar" $ ["-c", "--zstd", "-f", outputPath, "-C", tarRootDir] ++ contents

getReleaseFile :: Module -> T.Text -> App (Path Abs File)
getReleaseFile targetModule releaseName = do
  let releaseDir = getReleaseDir targetModule
  Path.ensureDir releaseDir
  releaseFile <- Path.resolveFile releaseDir $ T.unpack $ releaseName <> ".tar.zst"
  releaseExists <- Path.doesFileExist releaseFile
  when releaseExists $ do
    Throw.raiseError' $ "the release `" <> releaseName <> "` already exists"
  return releaseFile
