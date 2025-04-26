module Move.Scene.Archive (archive) where

import Control.Monad
import Data.Text qualified as T
import Move.Context.App
import Move.Context.EIO (EIO, raiseError', toApp)
import Move.Context.Env qualified as Env
import Move.Context.External qualified as External
import Move.Context.Module qualified as Module
import Path
import Path.IO
import Rule.Const
import Rule.Ens qualified as E
import Rule.Module
import Rule.PackageVersion qualified as PV
import Prelude hiding (log)

archive :: PV.PackageVersion -> E.FullEns -> Path Abs Dir -> [SomePath Rel] -> App ()
archive packageVersion fullEns moduleRootDir contents = do
  withSystemTempDir "archive" $ \tempRootDir -> do
    Module.saveEns (tempRootDir </> moduleFile) fullEns
    toApp $ copyModuleContents tempRootDir moduleRootDir contents
    toApp $ makeReadOnly tempRootDir
    makeArchiveFromTempDir packageVersion tempRootDir

makeArchiveFromTempDir :: PV.PackageVersion -> Path Abs Dir -> App ()
makeArchiveFromTempDir packageVersion tempRootDir = do
  (_, files) <- listDirRecurRel tempRootDir
  let newContents = map toFilePath files
  mainModule <- Env.getMainModule
  outputPath <- toApp $ toFilePath <$> getArchiveFilePath mainModule (PV.reify packageVersion)
  h <- External.new
  toApp $ External.run h "tar" $ ["-c", "--zstd", "-f", outputPath, "-C", toFilePath tempRootDir] ++ newContents

copyModuleContents :: Path Abs Dir -> Path Abs Dir -> [SomePath Rel] -> EIO ()
copyModuleContents tempRootDir moduleRootDir contents = do
  forM_ contents $ \content -> do
    case content of
      Left dirPath -> do
        copyDirRecur (moduleRootDir </> dirPath) (tempRootDir </> dirPath)
      Right filePath -> do
        ensureDir $ parent $ tempRootDir </> filePath
        copyFile (moduleRootDir </> filePath) (tempRootDir </> filePath)

makeReadOnly :: Path Abs Dir -> EIO ()
makeReadOnly tempRootDir = do
  (_, filePathList) <- listDirRecur tempRootDir
  forM_ filePathList $ \filePath -> do
    p <- getPermissions filePath
    setPermissions filePath $ p {writable = False}

getArchiveFilePath :: MainModule -> T.Text -> EIO (Path Abs File)
getArchiveFilePath (MainModule mainModule) versionText = do
  let archiveDir = getArchiveDir mainModule
  ensureDir archiveDir
  archiveFile <- resolveFile archiveDir $ T.unpack $ versionText <> packageFileExtension
  archiveExists <- doesFileExist archiveFile
  when archiveExists $ do
    raiseError' $ "The archive `" <> versionText <> "` already exists"
  return archiveFile
