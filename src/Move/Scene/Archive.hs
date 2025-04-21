module Move.Scene.Archive (archive) where

import Move.Context.App
import Move.Context.Env qualified as Env
import Move.Context.External qualified as External
import Move.Context.Module qualified as Module
import Move.Context.Path qualified as Path
import Move.Context.Throw qualified as Throw
import Control.Monad
import Data.Text qualified as T
import Rule.Const
import Rule.Ens qualified as E
import Rule.Module
import Rule.PackageVersion qualified as PV
import Path
import Path.IO
import Prelude hiding (log)

archive :: PV.PackageVersion -> E.FullEns -> Path Abs Dir -> [SomePath Rel] -> App ()
archive packageVersion fullEns moduleRootDir contents = do
  withSystemTempDir "archive" $ \tempRootDir -> do
    Module.saveEns (tempRootDir </> moduleFile) fullEns
    copyModuleContents tempRootDir moduleRootDir contents
    makeReadOnly tempRootDir
    makeArchiveFromTempDir packageVersion tempRootDir

makeArchiveFromTempDir :: PV.PackageVersion -> Path Abs Dir -> App ()
makeArchiveFromTempDir packageVersion tempRootDir = do
  (_, files) <- listDirRecurRel tempRootDir
  let newContents = map toFilePath files
  mainModule <- Env.getMainModule
  outputPath <- toFilePath <$> getArchiveFilePath mainModule (PV.reify packageVersion)
  External.run "tar" $ ["-c", "--zstd", "-f", outputPath, "-C", toFilePath tempRootDir] ++ newContents

copyModuleContents :: Path Abs Dir -> Path Abs Dir -> [SomePath Rel] -> App ()
copyModuleContents tempRootDir moduleRootDir contents = do
  forM_ contents $ \content -> do
    case content of
      Left dirPath -> do
        copyDirRecur (moduleRootDir </> dirPath) (tempRootDir </> dirPath)
      Right filePath -> do
        ensureDir $ parent $ tempRootDir </> filePath
        copyFile (moduleRootDir </> filePath) (tempRootDir </> filePath)

makeReadOnly :: Path Abs Dir -> App ()
makeReadOnly tempRootDir = do
  (_, filePathList) <- listDirRecur tempRootDir
  forM_ filePathList $ \filePath -> do
    p <- getPermissions filePath
    setPermissions filePath $ p {writable = False}

getArchiveFilePath :: Module -> T.Text -> App (Path Abs File)
getArchiveFilePath targetModule versionText = do
  let archiveDir = getArchiveDir targetModule
  Path.ensureDir archiveDir
  archiveFile <- Path.resolveFile archiveDir $ T.unpack $ versionText <> packageFileExtension
  archiveExists <- Path.doesFileExist archiveFile
  when archiveExists $ do
    Throw.raiseError' $ "The archive `" <> versionText <> "` already exists"
  return archiveFile
