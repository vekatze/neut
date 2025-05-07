module Command.Archive.Move.Internal.Archive
  ( Handle,
    new,
    archive,
  )
where

import Command.Archive.Rule.PackageVersion qualified as PV
import Command.Common.Move.SaveModule qualified as SaveModule
import Control.Monad
import Data.Text qualified as T
import Ens.Rule.Ens qualified as E
import Error.Rule.EIO (EIO)
import Kernel.Common.Rule.Const
import Kernel.Common.Rule.Module
import Kernel.Move.Context.Global.Env qualified as Env
import Kernel.Move.Context.ProcessRunner qualified as ProcessRunner
import Language.Common.Move.Raise (raiseError')
import Path
import Path.IO
import Prelude hiding (log)

data Handle = Handle
  { processRunnerHandle :: ProcessRunner.Handle,
    saveModuleHandle :: SaveModule.Handle,
    envHandle :: Env.Handle
  }

new :: ProcessRunner.Handle -> SaveModule.Handle -> Env.Handle -> Handle
new processRunnerHandle saveModuleHandle envHandle = do
  Handle {..}

archive :: Handle -> PV.PackageVersion -> E.FullEns -> Path Abs Dir -> [SomePath Rel] -> EIO ()
archive h packageVersion fullEns moduleRootDir contents = do
  withSystemTempDir "archive" $ \tempRootDir -> do
    SaveModule.save (saveModuleHandle h) (tempRootDir </> moduleFile) fullEns
    copyModuleContents tempRootDir moduleRootDir contents
    makeReadOnly tempRootDir
    makeArchiveFromTempDir h packageVersion tempRootDir

makeArchiveFromTempDir :: Handle -> PV.PackageVersion -> Path Abs Dir -> EIO ()
makeArchiveFromTempDir h packageVersion tempRootDir = do
  (_, files) <- listDirRecurRel tempRootDir
  let newContents = map toFilePath files
  let mainModule = Env.getMainModule (envHandle h)
  outputPath <- toFilePath <$> getArchiveFilePath mainModule (PV.reify packageVersion)
  ProcessRunner.run (processRunnerHandle h) "tar" $
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
