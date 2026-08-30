module Command.Common.Build.Execute
  ( Handle,
    new,
    execute,
  )
where

import App.App (App)
import App.Run (raiseError')
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.Module qualified as GlobalModule
import Kernel.Common.Handle.Global.Path qualified as Path
import Kernel.Common.Module qualified as M
import Kernel.Common.RunProcess qualified as RunProcess
import Kernel.Common.Target
import Language.Common.ModuleAlias qualified as MA
import Language.Common.ModuleID qualified as MID
import Path
import System.Exit (ExitCode (..), exitWith)

data Handle = Handle
  { pathHandle :: Path.Handle,
    envHandle :: Env.Handle,
    runProcessHandle :: RunProcess.Handle
  }

new :: Global.Handle -> Handle
new (Global.Handle {..}) = do
  let runProcessHandle = RunProcess.new loggerHandle
  Handle {..}

execute :: Handle -> MainTarget -> [String] -> App ()
execute h target args = do
  outputPath <- Path.getExecutableOutputPath (pathHandle h) target
  case getExecuteCommand target of
    Nothing ->
      run h (toFilePath outputPath) args
    Just commandTemplate -> do
      subst <- makeSubst h outputPath
      let command = map (T.unpack . applySubst subst) commandTemplate
      case command of
        [] ->
          raiseError' "The `execute` field must not be empty"
        program : programArgs ->
          run h program (programArgs ++ args)

run :: Handle -> FilePath -> [String] -> App ()
run h program args = do
  exitCode <- liftIO $ RunProcess.runProcess (runProcessHandle h) program args
  case exitCode of
    ExitSuccess ->
      return ()
    ExitFailure {} ->
      liftIO $ exitWith exitCode

makeSubst :: Handle -> Path Abs File -> App [(T.Text, T.Text)]
makeSubst h outputPath = do
  let mainModule = Env.getMainModule (envHandle h)
  let moduleRootDir = M.getModuleRootDir (M.extractModule mainModule)
  let dependencyList = Map.toList $ M.moduleDependency (M.extractModule mainModule)
  dependencySubst <- mapM (makeDependencySubst mainModule) dependencyList
  return $
    [ ("{{executable}}", T.pack (toFilePath outputPath)),
      ("{{module-root}}", T.pack (toFilePath moduleRootDir))
    ]
      ++ dependencySubst

makeDependencySubst :: M.MainModule -> (MA.ModuleAlias, M.Dependency) -> App (T.Text, T.Text)
makeDependencySubst mainModule (alias, dependency) = do
  let moduleID = MID.Library $ M.dependencyDigest dependency
  dependencyDir <- GlobalModule.getModuleDirByID mainModule Nothing moduleID
  return ("{{module:" <> MA.reify alias <> "}}", T.pack (toFilePath dependencyDir))

applySubst :: [(T.Text, T.Text)] -> T.Text -> T.Text
applySubst subst t =
  case subst of
    [] ->
      t
    (from, to) : rest ->
      T.replace from to (applySubst rest t)
