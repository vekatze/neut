module Scene.Archive (archive) where

import Context.App
import Context.External qualified as External
import Context.Module qualified as Module
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Control.Monad
import Data.List
import Data.Text qualified as T
import Entity.Const
import Entity.Module
import Entity.PackageVersion qualified as PV
import Path
import System.IO
import Prelude hiding (log)

archive :: PV.PackageVersion -> [FilePath] -> App ()
archive packageVersion contents = do
  mainModule <- Module.getMainModule
  outputPath <- toFilePath <$> getReleaseFile mainModule (PV.reify packageVersion)
  let moduleRootDir = parent $ moduleLocation mainModule
  let tarRootDir = toFilePath $ parent moduleRootDir
  External.run "tar" $ ["-c", "--zstd", "-f", outputPath, "-C", tarRootDir] ++ contents

getReleaseFile :: Module -> T.Text -> App (Path Abs File)
getReleaseFile targetModule versionText = do
  let releaseDir = getReleaseDir targetModule
  Path.ensureDir releaseDir
  releaseFile <- Path.resolveFile releaseDir $ T.unpack $ versionText <> packageFileExtension
  releaseExists <- Path.doesFileExist releaseFile
  when releaseExists $ do
    Throw.raiseError' $ "the release `" <> versionText <> "` already exists"
  return releaseFile
