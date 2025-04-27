module Move.Scene.Archive
  ( Handle,
    new,
    archive,
  )
where

import Control.Monad
import Data.Text qualified as T
import Move.Context.App
import Move.Context.EIO (EIO, raiseError')
import Move.Context.Env qualified as Env
import Move.Context.External qualified as External
import Move.Scene.Module.Save qualified as ModuleSave
import Path
import Path.IO
import Rule.Const
import Rule.Ens qualified as E
import Rule.Module
import Rule.PackageVersion qualified as PV
import Prelude hiding (log)

data Handle
  = Handle
  { externalHandle :: External.Handle,
    moduleSaveHandle :: ModuleSave.Handle,
    envHandle :: Env.Handle
  }

new :: App Handle
new = do
  externalHandle <- External.new
  moduleSaveHandle <- ModuleSave.new
  envHandle <- Env.new
  return $ Handle {..}

archive :: Handle -> PV.PackageVersion -> E.FullEns -> Path Abs Dir -> [SomePath Rel] -> EIO ()
archive h packageVersion fullEns moduleRootDir contents = do
  withSystemTempDir "archive" $ \tempRootDir -> do
    ModuleSave.save (moduleSaveHandle h) (tempRootDir </> moduleFile) fullEns
    copyModuleContents tempRootDir moduleRootDir contents
    makeReadOnly tempRootDir
    makeArchiveFromTempDir h packageVersion tempRootDir

makeArchiveFromTempDir :: Handle -> PV.PackageVersion -> Path Abs Dir -> EIO ()
makeArchiveFromTempDir h packageVersion tempRootDir = do
  (_, files) <- listDirRecurRel tempRootDir
  let newContents = map toFilePath files
  mainModule <- Env.getMainModule (envHandle h)
  outputPath <- toFilePath <$> getArchiveFilePath mainModule (PV.reify packageVersion)
  External.run (externalHandle h) "tar" $
    ["-c", "--zstd", "-f", outputPath, "-C", toFilePath tempRootDir] ++ newContents

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
