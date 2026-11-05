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
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as T
import Ens.Ens qualified as E
import Kernel.Common.Const
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Module
import Language.Common.ModuleDigest qualified as MD
import Logger.Handle qualified as Logger
import Logger.Print qualified as Logger
import Path
import Path.Archive (writeArchive)
import Path.IO
import System.FilePath qualified as FP
import Prelude hiding (log)

data Handle = Handle
  { saveModuleHandle :: SaveModule.Handle,
    envHandle :: Env.Handle,
    loggerHandle :: Logger.Handle
  }

new :: SaveModule.Handle -> Env.Handle -> Logger.Handle -> Handle
new saveModuleHandle envHandle loggerHandle = do
  Handle {..}

archive :: Handle -> PV.PackageVersion -> E.FullEns -> Path Abs Dir -> [SomePath Rel] -> App ()
archive h packageVersion fullEns moduleRootDir contents = do
  withSystemTempDir "archive" $ \tempRootDir -> do
    SaveModule.save (saveModuleHandle h) (tempRootDir </> moduleFile) fullEns
    copyModuleContents tempRootDir moduleRootDir contents
    (_, files) <- listDirRecurRel tempRootDir
    let mainModule = Env.getMainModule (envHandle h)
    outputPath <- getArchiveFilePath mainModule (PV.reify packageVersion)
    archiveBytes <- writeArchive tempRootDir files outputPath
    let digest = MD.fromByteString archiveBytes
    liftIO $ Logger.printNote' (loggerHandle h) $ "Created an archive: " <> PV.reify packageVersion <> " (" <> MD.reify digest <> ")"

copyModuleContents :: Path Abs Dir -> Path Abs Dir -> [SomePath Rel] -> App ()
copyModuleContents tempRootDir moduleRootDir contents = do
  forM_ contents $ \content -> do
    case content of
      Left dirPath -> do
        ensureNotSymlink moduleRootDir dirPath
        (dirs, files) <- listDirRecurRel (moduleRootDir </> dirPath)
        mapM_ (ensureNotSymlink moduleRootDir . (dirPath </>)) dirs
        mapM_ (ensureNotSymlink moduleRootDir . (dirPath </>)) files
        copyDirRecur (moduleRootDir </> dirPath) (tempRootDir </> dirPath)
      Right filePath -> do
        ensureNotSymlink moduleRootDir filePath
        ensureDir $ parent $ tempRootDir </> filePath
        copyFile (moduleRootDir </> filePath) (tempRootDir </> filePath)

ensureNotSymlink :: Path Abs Dir -> Path Rel t -> App ()
ensureNotSymlink moduleRootDir path = do
  pathIsSymlink <- isSymlink $ moduleRootDir </> path
  when pathIsSymlink $ do
    raiseError' $ "Archived files must not be symbolic links, but found: " <> T.pack (FP.dropTrailingPathSeparator $ toFilePath path)

getArchiveFilePath :: MainModule -> T.Text -> App (Path Abs File)
getArchiveFilePath (MainModule mainModule) versionText = do
  let archiveDir = getArchiveDir mainModule
  ensureDir archiveDir
  archiveFile <- resolveFile archiveDir $ T.unpack $ versionText <> packageFileExtension
  archiveExists <- doesFileExist archiveFile
  when archiveExists $ do
    raiseError' $ "The archive `" <> versionText <> "` already exists"
  return archiveFile
