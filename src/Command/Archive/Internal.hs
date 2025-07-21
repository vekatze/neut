module Command.Archive.Internal
  ( Handle,
    new,
    archive,
  )
where

import App.App (App)
import App.Run (raiseError')
import Command.Archive.PackageVersion.PackageVersion qualified as PV
import Command.Common.SaveModule qualified as SaveModule
import Control.Monad
import Data.Text qualified as T
import Ens.Ens qualified as E
import Kernel.Common.Const
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Module
import Kernel.Common.RunProcess qualified as RunProcess
import Path
import Path.IO
import Prelude hiding (log)

data Handle = Handle
  { runProcessHandle :: RunProcess.Handle,
    saveModuleHandle :: SaveModule.Handle,
    envHandle :: Env.Handle
  }

new :: RunProcess.Handle -> SaveModule.Handle -> Env.Handle -> Handle
new runProcessHandle saveModuleHandle envHandle = do
  Handle {..}

archive :: Handle -> PV.PackageVersion -> E.FullEns -> Path Abs Dir -> [SomePath Rel] -> App ()
archive h packageVersion fullEns moduleRootDir contents = do
  withSystemTempDir "archive" $ \tempRootDir -> do
    SaveModule.save (saveModuleHandle h) (tempRootDir </> moduleFile) fullEns
    copyModuleContents tempRootDir moduleRootDir contents
    makeArchiveFromTempDir h packageVersion tempRootDir

makeArchiveFromTempDir :: Handle -> PV.PackageVersion -> Path Abs Dir -> App ()
makeArchiveFromTempDir h packageVersion tempRootDir = do
  (_, files) <- listDirRecurRel tempRootDir
  let newContents = map toFilePath files
  let mainModule = Env.getMainModule (envHandle h)
  outputPath <- toFilePath <$> getArchiveFilePath mainModule (PV.reify packageVersion)
  RunProcess.run (runProcessHandle h) "tar" $
    ["-c", "--zstd", "-f", outputPath, "-C", toFilePath tempRootDir] ++ newContents

copyModuleContents :: Path Abs Dir -> Path Abs Dir -> [SomePath Rel] -> App ()
copyModuleContents tempRootDir moduleRootDir contents = do
  forM_ contents $ \content -> do
    case content of
      Left dirPath -> do
        copyDirRecur (moduleRootDir </> dirPath) (tempRootDir </> dirPath)
      Right filePath -> do
        ensureDir $ parent $ tempRootDir </> filePath
        copyFile (moduleRootDir </> filePath) (tempRootDir </> filePath)

getArchiveFilePath :: MainModule -> T.Text -> App (Path Abs File)
getArchiveFilePath (MainModule mainModule) versionText = do
  let archiveDir = getArchiveDir mainModule
  ensureDir archiveDir
  archiveFile <- resolveFile archiveDir $ T.unpack $ versionText <> packageFileExtension
  archiveExists <- doesFileExist archiveFile
  when archiveExists $ do
    raiseError' $ "The archive `" <> versionText <> "` already exists"
  return archiveFile
