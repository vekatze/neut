module Command.Common.Build.Link
  ( Handle,
    new,
    link,
  )
where

import App.App (App)
import Console.Handle qualified as Console
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString qualified as B
import Data.Containers.ListUtils (nubOrdOn)
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Kernel.Common.Allocator (Allocator (Mimalloc), mimallocArchive)
import Kernel.Common.Artifact qualified as A
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.Path qualified as Path
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.Module
import Kernel.Common.OS qualified as OS
import Kernel.Common.OutputKind qualified as OK
import Kernel.Common.Platform qualified as P
import Kernel.Common.RunProcess qualified as RunProcess
import Kernel.Common.Source qualified as Source
import Kernel.Common.Target
import Logger.Debug qualified as Logger
import Logger.Handle qualified as Logger
import Path
import Path.IO
import ProgressIndicator.ShowProgress qualified as Indicator
import System.Console.ANSI
import System.Process (CmdSpec (RawCommand))

data Handle = Handle
  { loggerHandle :: Logger.Handle,
    envHandle :: Env.Handle,
    pathHandle :: Path.Handle,
    platformHandle :: Platform.Handle,
    consoleHandle :: Console.Handle
  }

new :: Global.Handle -> Handle
new (Global.Handle {..}) = do
  Handle {..}

link :: Handle -> MainTarget -> Bool -> Bool -> A.ArtifactTime -> [Source.Source] -> App ()
link h target shouldSkipLink didPerformForeignCompilation artifactTime sourceList = do
  executablePath <- Path.getExecutableOutputPath (pathHandle h) target
  isExecutableAvailable <- doesFileExist executablePath
  let freshExecutableAvailable = isJust (A.objectTime artifactTime) && isExecutableAvailable
  if shouldSkipLink || (not didPerformForeignCompilation && freshExecutableAvailable)
    then liftIO $ Logger.report (loggerHandle h) "Skipped linking object files"
    else link' h target sourceList

link' :: Handle -> MainTarget -> [Source.Source] -> App ()
link' h target sourceList = do
  mainObject <- snd <$> Path.getOutputPathForEntryPoint (pathHandle h) OK.Object target
  outputPath <- Path.getExecutableOutputPath (pathHandle h) target
  ensureDir $ parent outputPath
  objectPathList <- mapM (Path.sourceToOutputPath (pathHandle h) (Main target) OK.Object) sourceList
  let moduleList = nubOrdOn moduleID $ map Source.sourceModule sourceList
  foreignDirList <- mapM (Path.getForeignDir (pathHandle h) (Main target)) moduleList
  foreignObjectList <- concat <$> mapM getForeignDirContent foreignDirList
  mAllocatorLibrary <- getAllocatorLibraryIfNecessary h target
  let objects = mainObject : objectPathList ++ foreignObjectList ++ maybeToList mAllocatorLibrary
  clang <- liftIO Platform.getClang
  let targetTriple = Platform.getClangTargetTriple (platformHandle h)
  let userLinkOptions = getLinkOption target
  let baseModule = extractModule $ Env.getMainModule (envHandle h)
  ltoCacheDir <- Path.getLtoCacheDir (pathHandle h) (Main target) baseModule
  ltoOption <- liftIO $ getLtoOption h clang userLinkOptions ltoCacheDir
  let linkOptions = clangLinkOpt targetTriple objects outputPath (ltoOption ++ userLinkOptions)
  let numOfObjects = length objects
  let workingTitle = getWorkingTitle numOfObjects
  let completedTitle = getCompletedTitle numOfObjects
  progressBarHandle <- liftIO $ Indicator.new (consoleHandle h) (loggerHandle h) Nothing workingTitle completedTitle barColor
  let runProcessHandle = RunProcess.new (loggerHandle h)
  RunProcess.run runProcessHandle clang linkOptions
  liftIO $ Indicator.close progressBarHandle

getWorkingTitle :: Int -> T.Text
getWorkingTitle numOfObjects = do
  let suffix = if numOfObjects <= 1 then "" else "s"
  "Linking " <> T.pack (show numOfObjects) <> " object" <> suffix

getCompletedTitle :: Int -> T.Text
getCompletedTitle numOfObjects = do
  let suffix = if numOfObjects <= 1 then "" else "s"
  "Linked " <> T.pack (show numOfObjects) <> " object" <> suffix

barColor :: [SGR]
barColor = do
  [SetColor Foreground Vivid Green]

data LinkerFamily
  = LldFamily
  | GnuFamily
  | UnknownFamily

getLtoOption :: Handle -> String -> [String] -> Path Abs Dir -> IO [String]
getLtoOption h clang userLinkOptions ltoCacheDir = do
  let cacheDir = toFilePath ltoCacheDir
  case P.os (Platform.getPlatform (platformHandle h)) of
    OS.Darwin ->
      return ["-Xlinker", "-cache_path_lto", "-Xlinker", cacheDir]
    OS.Linux ->
      case findUserSpecifiedLinker userLinkOptions of
        Just linker ->
          return $ getLtoCacheOption (classifyLinker linker) cacheDir
        Nothing -> do
          lldIsAvailable <- checkIfLldIsAvailable h clang
          if lldIsAvailable
            then return $ "-fuse-ld=lld" : getLtoCacheOption LldFamily cacheDir
            else return $ getLtoCacheOption GnuFamily cacheDir

findUserSpecifiedLinker :: [String] -> Maybe T.Text
findUserSpecifiedLinker userLinkOptions = do
  let linkerNames = mapMaybe (extractLinkerName . T.pack) userLinkOptions
  listToMaybe $ reverse linkerNames

extractLinkerName :: T.Text -> Maybe T.Text
extractLinkerName option =
  case T.stripPrefix "-fuse-ld=" option of
    Just linker ->
      return $ T.takeWhileEnd (/= '/') linker
    Nothing -> do
      linkerPath <- T.stripPrefix "--ld-path=" option
      return $ T.takeWhileEnd (/= '/') linkerPath

classifyLinker :: T.Text -> LinkerFamily
classifyLinker linker
  | "lld" `T.isInfixOf` linker || "mold" `T.isInfixOf` linker =
      LldFamily
  | "ld" `T.isInfixOf` linker || "bfd" `T.isInfixOf` linker =
      GnuFamily
  | otherwise =
      UnknownFamily

getLtoCacheOption :: LinkerFamily -> FilePath -> [String]
getLtoCacheOption linkerFamily cacheDir =
  case linkerFamily of
    LldFamily ->
      ["-Xlinker", "--thinlto-cache-dir=" ++ cacheDir]
    GnuFamily ->
      ["-Xlinker", "-plugin-opt", "-Xlinker", "cache-dir=" ++ cacheDir]
    UnknownFamily ->
      []

checkIfLldIsAvailable :: Handle -> String -> IO Bool
checkIfLldIsAvailable h clang = do
  let runProcessHandle = RunProcess.new (loggerHandle h)
  let spec = RunProcess.Spec {RunProcess.cmdspec = RawCommand clang ["-print-prog-name=ld.lld"], RunProcess.cwd = Nothing}
  value <- RunProcess.run01 runProcessHandle spec
  case value of
    Right output ->
      return $ "/" `T.isPrefixOf` T.strip (decodeUtf8Lenient output)
    Left _ ->
      return False

getForeignDirContent :: Path Abs Dir -> App [Path Abs File]
getForeignDirContent foreignDir = do
  b <- doesDirExist foreignDir
  if b
    then snd <$> listDirRecur foreignDir
    else return []

getAllocatorLibraryIfNecessary :: Handle -> MainTarget -> App (Maybe (Path Abs File))
getAllocatorLibraryIfNecessary h target = do
  allocator <- Env.getAllocatorByTarget (envHandle h) (Main target)
  case allocator of
    Mimalloc -> do
      let baseModule = extractModule $ Env.getMainModule (envHandle h)
      allocatorDir <- Path.getAllocatorDir (pathHandle h) (Main target) baseModule
      archivePath <- resolveFile allocatorDir "libmimalloc.a"
      archiveExists <- doesFileExist archivePath
      if archiveExists
        then return ()
        else liftIO $ B.writeFile (toFilePath archivePath) mimallocArchive
      return $ Just archivePath
    _ ->
      return Nothing

clangLinkOpt :: String -> [Path Abs File] -> Path Abs File -> [String] -> [String]
clangLinkOpt targetTriple objectPathList outputPath additionalOptions = do
  let pathList = map toFilePath objectPathList
  [ "-target",
    targetTriple,
    "-O2",
    "-flto=thin",
    "-pthread",
    "-o",
    toFilePath outputPath
    ]
    ++ pathList
    ++ additionalOptions
    ++ ["-lm"]
