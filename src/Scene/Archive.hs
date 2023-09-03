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
  outputPath <- toFilePath <$> getArchiveFile mainModule (PV.reify packageVersion)
  let moduleRootDir = parent $ moduleLocation mainModule
  let tarRootDir = toFilePath $ parent moduleRootDir
  External.run "tar" $ ["-c", "--zstd", "-f", outputPath, "-C", tarRootDir] ++ contents

getArchiveFile :: Module -> T.Text -> App (Path Abs File)
getArchiveFile targetModule versionText = do
  let archiveDir = getArchiveDir targetModule
  Path.ensureDir archiveDir
  archiveFile <- Path.resolveFile archiveDir $ T.unpack $ versionText <> packageFileExtension
  archiveExists <- Path.doesFileExist archiveFile
  when archiveExists $ do
    Throw.raiseError' $ "the archive `" <> versionText <> "` already exists"
  return archiveFile
