module Move.Context.Env
  ( Handle,
    new,
    getBuildMode,
    getMainModule,
    setBuildMode,
    getSilentMode,
    getMainDefiniteDescriptionByTarget,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Logger.Rule.Handle qualified as Logger
import Move.Context.EIO (EIO, raiseError', run)
import Move.Scene.Module.Reflect (getCurrentModuleFilePath)
import Move.Scene.Module.Reflect qualified as ModuleReflect
import Path
import Rule.BaseName qualified as BN
import Rule.BuildMode qualified as BM
import Rule.DefiniteDescription qualified as DD
import Rule.LocalLocator qualified as LL
import Rule.Module
import Rule.Module qualified as Module
import Rule.ModuleID qualified as MID
import Rule.SourceLocator qualified as SL
import Rule.StrictGlobalLocator qualified as SGL
import Rule.Target qualified as Target

data Handle
  = Handle
  { buildModeRef :: IORef BM.BuildMode,
    enableSilentMode :: Bool,
    mainModule :: MainModule
  }

new :: Logger.Handle -> Bool -> Maybe (Path Abs File) -> IO Handle
new loggerHandle enableSilentMode moduleFilePathOrNone = do
  buildModeRef <- newIORef BM.Develop
  run loggerHandle $ do
    let moduleReflectHandle = ModuleReflect.new undefined
    mainModule <-
      MainModule
        <$> case moduleFilePathOrNone of
          Just moduleFilePath ->
            ModuleReflect.fromFilePath moduleReflectHandle moduleFilePath
          Nothing -> do
            getCurrentModuleFilePath >>= ModuleReflect.fromFilePath moduleReflectHandle
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
