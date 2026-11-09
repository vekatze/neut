module Kernel.Common.Handle.Global.Env
  ( Handle (..),
    new,
    getMainModule,
    getMainModuleDir,
    getMainDefiniteDescriptionByTarget,
    getAllocatorByTarget,
  )
where

import App.App (App)
import App.Error qualified as E
import App.Run (raiseCritical', raiseError', runApp)
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Kernel.Common.Allocator (Allocator)
import Kernel.Common.Handle.Global.Reuse qualified as Reuse
import Kernel.Common.Module
import Kernel.Common.Module qualified as Module
import Kernel.Common.Module.FindModuleFile (getCurrentModuleFilePath)
import Kernel.Common.Module.FromPath qualified as ModuleReflect
import Kernel.Common.Target qualified as Target
import Kernel.Common.TargetConfig qualified as TC
import Language.Common.BaseName qualified as BN
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.LocalLocator qualified as LL
import Language.Common.ModuleID qualified as MID
import Language.Common.SourceLocator qualified as SL
import Language.Common.StrictGlobalLocator qualified as SGL
import Path

newtype Handle = Handle
  { _mainModule :: MainModule
  }

getMainModule :: Handle -> MainModule
getMainModule =
  _mainModule

getMainModuleDir :: Handle -> FilePath
getMainModuleDir h =
  toFilePath $ getModuleRootDir $ extractModule $ getMainModule h

new :: Reuse.Handle -> Maybe (Path Abs File) -> IO (Either E.Error Handle)
new reuseHandle moduleFilePathOrNone = do
  runApp $ do
    moduleFilePath <- maybe getCurrentModuleFilePath return moduleFilePathOrNone
    _mainModule <- MainModule <$> Reuse.rememberModule reuseHandle moduleFilePath (ModuleReflect.fromFilePath moduleFilePath)
    return $ Handle {..}

getMainDefiniteDescriptionByTarget :: Handle -> Target.MainTarget -> App DD.DefiniteDescription
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

getAllocatorByTarget :: Target.Target -> App Allocator
getAllocatorByTarget target =
  case target of
    Target.Peripheral {} ->
      raiseCritical' "Kernel.Common.Handle.Global.Env.getAllocatorByTarget: unexpected peripheral target"
    Target.PeripheralSingle {} ->
      raiseCritical' "Kernel.Common.Handle.Global.Env.getAllocatorByTarget: unexpected peripheral-single target"
    Target.Main mainTarget ->
      return $ TC.allocator $ Target.getTargetConfig mainTarget

relPathToDD :: Path Rel File -> BN.BaseName -> App DD.DefiniteDescription
relPathToDD relPath baseName = do
  relPath' <- removeExtension relPath
  sourceLocator <- case SL.fromPath relPath' of
    Just locator ->
      return locator
    Nothing ->
      raiseError' $ "The source path `" <> T.pack (toFilePath relPath) <> "` contains the reserved segment `this`"
  let sgl = SGL.new MID.Main sourceLocator
  let ll = LL.new baseName
  return $ DD.new sgl ll

removeExtension :: Path a File -> App (Path a File)
removeExtension path =
  case splitExtension path of
    Just (path', _) ->
      return path'
    Nothing ->
      raiseError' $ "File extension is missing in `" <> T.pack (toFilePath path) <> "`"
