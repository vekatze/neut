module Move.Context.Env
  ( Handle,
    new,
    getArch,
    getDataSizeValue,
    getBuildMode,
    getDataSize,
    getMainModule,
    getOS,
    getPlatform,
    setBuildMode,
    getSilentMode,
    getMainDefiniteDescriptionByTarget,
    getPlatformPrefix,
    getBaseBuildDir,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Data.Version qualified as V
import Move.Console.Report qualified as Report
import Move.Context.EIO (EIO, raiseError, raiseError', run)
import Move.Scene.Module.Reflect (getCurrentModuleFilePath)
import Move.Scene.Module.Reflect qualified as ModuleReflect
import Path
import Paths_neut
import Rule.Arch qualified as Arch
import Rule.BaseName qualified as BN
import Rule.BuildMode qualified as BM
import Rule.DataSize qualified as DS
import Rule.DefiniteDescription qualified as DD
import Rule.Hint
import Rule.LocalLocator qualified as LL
import Rule.Module
import Rule.Module qualified as Module
import Rule.ModuleID qualified as MID
import Rule.OS qualified as O
import Rule.Platform qualified as P
import Rule.SourceLocator qualified as SL
import Rule.StrictGlobalLocator qualified as SGL
import Rule.Target qualified as Target
import System.Info qualified as SI

data Handle
  = Handle
  { arch :: Arch.Arch,
    os :: O.OS,
    baseSize :: DS.DataSize,
    buildModeRef :: IORef BM.BuildMode,
    enableSilentMode :: Bool,
    mainModule :: MainModule
  }

new :: Report.Handle -> Bool -> Maybe (Path Abs File) -> IO Handle
new reportHandle enableSilentMode moduleFilePathOrNone = do
  buildModeRef <- newIORef BM.Develop
  run reportHandle $ do
    let moduleReflectHandle = ModuleReflect.new undefined
    mainModule <-
      MainModule
        <$> case moduleFilePathOrNone of
          Just moduleFilePath ->
            ModuleReflect.fromFilePath moduleReflectHandle moduleFilePath
          Nothing -> do
            getCurrentModuleFilePath >>= ModuleReflect.fromFilePath moduleReflectHandle
    arch <- getArch' Nothing
    baseSize <- Arch.dataSizeOf <$> getArch' Nothing
    os <- getOS' Nothing
    return $ Handle {..}

getMainModule :: Handle -> MainModule
getMainModule =
  mainModule

setBuildMode :: Handle -> BM.BuildMode -> IO ()
setBuildMode h =
  writeIORef (buildModeRef h)

getBuildMode :: Handle -> IO BM.BuildMode
getBuildMode h =
  readIORef (buildModeRef h)

getDataSizeValue :: Handle -> Int
getDataSizeValue h =
  DS.reify $ baseSize h

getArch :: Handle -> Arch.Arch
getArch =
  arch

getOS :: Handle -> O.OS
getOS =
  os

getPlatform :: Handle -> P.Platform
getPlatform h = do
  let arch = getArch h
  let os = getOS h
  P.Platform {arch, os}

getDataSize :: Handle -> DS.DataSize
getDataSize =
  baseSize

getArch' :: Maybe Hint -> EIO Arch.Arch
getArch' mm = do
  case SI.arch of
    "amd64" ->
      return Arch.Amd64
    "x86_64" ->
      return Arch.Amd64
    "arm64" ->
      return Arch.Arm64
    "aarch64" ->
      return Arch.Arm64
    arch ->
      case mm of
        Just m ->
          raiseError m $ "Unknown architecture: " <> T.pack arch
        Nothing ->
          raiseError' $ "Unknown architecture: " <> T.pack arch

getOS' :: Maybe Hint -> EIO O.OS
getOS' mm = do
  case SI.os of
    "linux" ->
      return O.Linux
    "darwin" ->
      return O.Darwin
    os ->
      case mm of
        Just m ->
          raiseError m $ "Unknown OS: " <> T.pack os
        Nothing ->
          raiseError' $ "Unknown OS: " <> T.pack os

getSilentMode :: Handle -> Bool
getSilentMode =
  enableSilentMode

getMainDefiniteDescriptionByTarget :: Handle -> Target.MainTarget -> EIO DD.DefiniteDescription
getMainDefiniteDescriptionByTarget h targetOrZen = do
  let mainModule = getMainModule h
  case targetOrZen of
    Target.Named target _ -> do
      case Map.lookup target (Module.moduleTarget $ extractModule mainModule) of
        Nothing ->
          raiseError' $ "No such target is defined: " <> target
        Just targetSummary -> do
          relPathToDD (SL.reify $ Target.entryPoint targetSummary) BN.mainName
    Target.Zen path _ -> do
      relPath <- Module.getRelPathFromSourceDir (extractModule mainModule) path
      relPathToDD relPath BN.zenName

relPathToDD :: Path Rel File -> BN.BaseName -> EIO DD.DefiniteDescription
relPathToDD relPath baseName = do
  sourceLocator <- SL.SourceLocator <$> removeExtension relPath
  let sgl = SGL.StrictGlobalLocator {moduleID = MID.Main, sourceLocator = sourceLocator}
  let ll = LL.new baseName
  return $ DD.new sgl ll

removeExtension :: Path a File -> EIO (Path a File)
removeExtension path =
  case splitExtension path of
    Just (path', _) ->
      return path'
    Nothing ->
      raiseError' $ "File extension is missing in `" <> T.pack (toFilePath path) <> "`"

getPlatformPrefix :: Handle -> EIO (Path Rel Dir)
getPlatformPrefix h = do
  let p = getPlatform h
  parseRelDir $ T.unpack $ P.reify p

getBaseBuildDir :: Handle -> Module -> EIO (Path Abs Dir)
getBaseBuildDir h baseModule = do
  platformPrefix <- getPlatformPrefix h
  versionDir <- parseRelDir $ "compiler-" ++ V.showVersion version
  let moduleRootDir = getModuleRootDir baseModule
  return $ moduleRootDir </> moduleCacheDir baseModule </> $(mkRelDir "build") </> platformPrefix </> versionDir
