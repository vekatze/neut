module Kernel.Common.Handle.Global.Env
  ( Handle,
    new,
    getBuildMode,
    getMainModule,
    getSilentMode,
    setBuildMode,
    getMainDefiniteDescriptionByTarget,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Error.EIO (EIO)
import Error.Run (raiseError', run)
import Kernel.Common.BuildMode qualified as BM
import Kernel.Common.Module
import Kernel.Common.Module qualified as Module
import Kernel.Common.Module.FromPath qualified as ModuleReflect
import Kernel.Common.RuleHandle.Global.Env
import Kernel.Common.Target qualified as Target
import Language.Common.BaseName qualified as BN
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.LocalLocator qualified as LL
import Language.Common.ModuleID qualified as MID
import Language.Common.SourceLocator qualified as SL
import Language.Common.StrictGlobalLocator qualified as SGL
import Logger.Handle qualified as Logger
import Path

new :: Logger.Handle -> Bool -> Maybe (Path Abs File) -> IO Handle
new loggerHandle _enableSilentMode moduleFilePathOrNone = do
  _buildModeRef <- newIORef BM.Develop
  run loggerHandle $ do
    _mainModule <-
      MainModule
        <$> case moduleFilePathOrNone of
          Just moduleFilePath ->
            ModuleReflect.fromFilePath moduleFilePath
          Nothing -> do
            ModuleReflect.fromCurrentPath
    return $ Handle {..}

setBuildMode :: Handle -> BM.BuildMode -> IO ()
setBuildMode h =
  writeIORef (_buildModeRef h)

getBuildMode :: Handle -> IO BM.BuildMode
getBuildMode h =
  readIORef (_buildModeRef h)

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
