module Move.Context.Env
  ( Handle,
    new,
    PathMap,
    getArch,
    getArch',
    getDataSizeValue,
    getDataSize''',
    getBaseSize,
    getBaseSize',
    getBuildMode,
    getDataSize,
    getDataSize',
    getDataSize'',
    getMainModule,
    getOS,
    getPlatform,
    setBuildMode,
    setMainModule,
    setSilentMode,
    getSilentMode,
    getMainDefiniteDescriptionByTarget,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Data.Time
import Move.Console.Report qualified as Report
import Move.Context.EIO (EIO, raiseCritical', raiseError, raiseError', run)
import Path
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
    baseSize :: DS.DataSize,
    buildModeRef :: IORef BM.BuildMode,
    enableSilentModeRef :: IORef Bool,
    mainModuleRef :: IORef (Maybe MainModule)
  }

new :: Report.Handle -> IO Handle
new reportHandle = do
  buildModeRef <- newIORef BM.Develop
  enableSilentModeRef <- newIORef False
  mainModuleRef <- newIORef Nothing
  run reportHandle $ do
    arch <- getArch Nothing
    baseSize <- getDataSize'
    return $ Handle {..}

getMainModule :: Handle -> EIO MainModule
getMainModule h =
  readIORefOrFail "mainModule" (mainModuleRef h)

setMainModule :: Handle -> MainModule -> IO ()
setMainModule h m =
  writeIORef (mainModuleRef h) (Just m)

setBuildMode :: Handle -> BM.BuildMode -> IO ()
setBuildMode h =
  writeIORef (buildModeRef h)

getBuildMode :: Handle -> IO BM.BuildMode
getBuildMode h =
  readIORef (buildModeRef h)

type PathMap = Map.HashMap (Path Abs File) UTCTime

getDataSizeValue :: Handle -> Int
getDataSizeValue h =
  DS.reify $ baseSize h

getDataSize''' :: Handle -> DS.DataSize
getDataSize''' =
  baseSize

getArch' :: Handle -> Arch.Arch
getArch' =
  arch

getDataSize :: Hint -> EIO DS.DataSize
getDataSize m = do
  getDataSize'' (Just m)

getDataSize' :: EIO DS.DataSize
getDataSize' = do
  getDataSize'' Nothing

getDataSize'' :: Maybe Hint -> EIO DS.DataSize
getDataSize'' mm = do
  Arch.dataSizeOf <$> getArch mm

getBaseSize :: Hint -> EIO Int
getBaseSize m = do
  DS.reify <$> getDataSize m

getBaseSize' :: EIO Int
getBaseSize' = do
  DS.reify <$> getDataSize'

getArch :: Maybe Hint -> EIO Arch.Arch
getArch mm = do
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

getOS :: Maybe Hint -> EIO O.OS
getOS mm = do
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

getPlatform :: Maybe Hint -> EIO P.Platform
getPlatform mm = do
  arch <- getArch mm
  os <- getOS mm
  return $ P.Platform {arch, os}

setSilentMode :: Handle -> Bool -> IO ()
setSilentMode h =
  writeIORef (enableSilentModeRef h)

getSilentMode :: Handle -> IO Bool
getSilentMode h =
  readIORef (enableSilentModeRef h)

readIORefOrFail :: T.Text -> IORef (Maybe a) -> EIO a
readIORefOrFail name ref = do
  mValue <- liftIO $ readIORef ref
  case mValue of
    Just a ->
      return a
    Nothing ->
      raiseCritical' $ "[compiler bug] `" <> name <> "` is uninitialized"

getMainDefiniteDescriptionByTarget :: Handle -> Target.MainTarget -> EIO DD.DefiniteDescription
getMainDefiniteDescriptionByTarget h targetOrZen = do
  mainModule <- getMainModule h
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
