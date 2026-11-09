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
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Kernel.Common.Allocator (Allocator (Mimalloc), allocatorLinkOption, mimallocArchive)
import Kernel.Common.Artifact qualified as A
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.Path qualified as Path
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.Module
import Kernel.Common.OS qualified as OS
import Kernel.Common.OutputKind qualified as OK
import Kernel.Common.RunProcess qualified as RunProcess
import Kernel.Common.Source qualified as Source
import Kernel.Common.Target
import Logger.Debug qualified as Logger
import Logger.Handle qualified as Logger
import Path
import Path.IO
import Path.Read (readTextFromPath)
import Path.Write (writeText)
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

link :: Handle -> MainTarget -> Bool -> Bool -> A.ArtifactTime -> [(Module, Foreign)] -> [T.Text] -> [Source.Source] -> App ()
link h target shouldSkipLink didPerformForeignCompilation artifactTime moduleForeignList transitiveLinkOption sourceList = do
  if shouldSkipLink
    then liftIO $ Logger.report (loggerHandle h) "Skipped linking object files (--skip-link was specified)"
    else do
      objects <- getObjectList h target moduleForeignList sourceList
      let linkResponse = getLinkResponse objects transitiveLinkOption
      linkResponseFilePath <- Path.getLinkResponseFilePath (pathHandle h) target
      previousLinkResponse <- readLinkResponse linkResponseFilePath
      executablePath <- Path.getExecutableOutputPath (pathHandle h) target
      isExecutableAvailable <- doesFileExist executablePath
      let freshExecutableAvailable = isJust (A.objectTime artifactTime) && isExecutableAvailable
      if not didPerformForeignCompilation && freshExecutableAvailable && previousLinkResponse == Just linkResponse
        then liftIO $ Logger.report (loggerHandle h) "Skipped linking object files (the executable is fresh)"
        else link' h target linkResponseFilePath linkResponse (length objects)

getObjectList :: Handle -> MainTarget -> [(Module, Foreign)] -> [Source.Source] -> App [Path Abs File]
getObjectList h target moduleForeignList sourceList = do
  mainObject <- snd <$> Path.getOutputPathForEntryPoint (pathHandle h) OK.Object target
  objectPathList <- mapM (Path.sourceToOutputPath (pathHandle h) (Main target) OK.Object) sourceList
  foreignObjectList <- concat <$> mapM (getLinkedOutputList h target) moduleForeignList
  allocator <- Env.getAllocatorByTarget (Main target)
  mAllocatorLibrary <- getAllocatorLibraryIfNecessary h target allocator
  return $ mainObject : objectPathList ++ foreignObjectList ++ maybeToList mAllocatorLibrary

getLinkResponse :: [Path Abs File] -> [T.Text] -> T.Text
getLinkResponse objects transitiveLinkOption =
  T.unlines $ map quoteResponseFileArgument $ map (T.pack . toFilePath) objects ++ transitiveLinkOption

readLinkResponse :: Path Abs File -> App (Maybe T.Text)
readLinkResponse linkResponseFilePath = do
  responseExists <- doesFileExist linkResponseFilePath
  if responseExists
    then Just <$> readTextFromPath linkResponseFilePath
    else return Nothing

link' :: Handle -> MainTarget -> Path Abs File -> T.Text -> Int -> App ()
link' h target linkResponseFilePath linkResponse numOfObjects = do
  outputPath <- Path.getExecutableOutputPath (pathHandle h) target
  ensureDir $ parent outputPath
  allocator <- Env.getAllocatorByTarget (Main target)
  let clang = Platform.getClang (platformHandle h)
  let targetTriple = Platform.getClangTargetTriple (platformHandle h)
  let userLinkOptions = getLinkOption target
  let baseModule = extractModule $ Env.getMainModule (envHandle h)
  ltoCacheDir <- Path.getLtoCacheDir (pathHandle h) (Main target) baseModule
  ltoOption <- liftIO $ getLtoOption h clang userLinkOptions ltoCacheDir
  liftIO $ writeText linkResponseFilePath linkResponse
  liftIO $ Logger.report (loggerHandle h) $ "Created a response file at: " <> T.pack (toFilePath linkResponseFilePath)
  let allocatorOption = allocatorLinkOption (Platform.getArch (platformHandle h)) allocator
  sysrootOption <- liftIO $ Platform.getSysrootOption (loggerHandle h) (platformHandle h)
  let toolchainOption = Platform.getToolchainOption (platformHandle h)
  let linkOptions = clangLinkOpt targetTriple (Platform.getOS (platformHandle h)) linkResponseFilePath outputPath (ltoOption ++ allocatorOption ++ sysrootOption ++ toolchainOption ++ userLinkOptions)
  let workingTitle = getWorkingTitle numOfObjects
  let completedTitle = getCompletedTitle numOfObjects
  let runProcessHandle = RunProcess.new (loggerHandle h)
  Indicator.with (consoleHandle h) (loggerHandle h) Nothing workingTitle completedTitle barColor $ \_ ->
    RunProcess.run runProcessHandle clang linkOptions

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
  case Platform.getOS (platformHandle h) of
    OS.Darwin ->
      return ["-Xlinker", "-cache_path_lto", "-Xlinker", cacheDir]
    OS.Wasip1 ->
      return $ getLtoCacheOption LldFamily cacheDir
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

getLinkedOutputList :: Handle -> MainTarget -> (Module, Foreign) -> App [Path Abs File]
getLinkedOutputList h target (baseModule, foreignInfo) = do
  foreignDir <- Path.getForeignDir (pathHandle h) (Main target) baseModule
  return $ map (foreignDir </>) $ linkedOutput foreignInfo

getAllocatorLibraryIfNecessary :: Handle -> MainTarget -> Allocator -> App (Maybe (Path Abs File))
getAllocatorLibraryIfNecessary h target allocator = do
  case allocator of
    Mimalloc -> do
      let baseModule = extractModule $ Env.getMainModule (envHandle h)
      allocatorDir <- Path.getAllocatorDir (pathHandle h) (Main target) baseModule
      archivePath <- resolveFile allocatorDir "libmimalloc.a"
      archiveExists <- doesFileExist archivePath
      if archiveExists
        then return ()
        else liftIO $ B.writeFile (toFilePath archivePath) $ mimallocArchive $ Platform.getArch (platformHandle h)
      return $ Just archivePath
    _ ->
      return Nothing

quoteResponseFileArgument :: T.Text -> T.Text
quoteResponseFileArgument argument =
  "\"" <> T.concatMap escapeResponseFileChar argument <> "\""

escapeResponseFileChar :: Char -> T.Text
escapeResponseFileChar c =
  case c of
    '\\' ->
      "\\\\"
    '"' ->
      "\\\""
    _ ->
      T.singleton c

clangLinkOpt :: String -> OS.OS -> Path Abs File -> Path Abs File -> [String] -> [String]
clangLinkOpt targetTriple os linkResponseFilePath outputPath additionalOptions = do
  let threadOption =
        case os of
          OS.Wasip1 ->
            []
          _ ->
            ["-pthread"]
  let stackOption =
        case os of
          OS.Wasip1 ->
            ["-Wl,-z,stack-size=" ++ show wasiStackSize]
          _ ->
            []
  [ "-target",
    targetTriple,
    "-O2",
    "-flto=thin"
    ]
    ++ threadOption
    ++ [ "-o",
         toFilePath outputPath
       ]
    ++ ["@" ++ toFilePath linkResponseFilePath]
    ++ stackOption
    ++ additionalOptions
    ++ ["-lm"]

wasiStackSize :: Int
wasiStackSize =
  8 * 1024 * 1024
