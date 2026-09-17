module Command.Common.Build.Execute
  ( Handle,
    new,
    execute,
  )
where

import App.App (App)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as T
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.Module qualified as GlobalModule
import Kernel.Common.Handle.Global.Path qualified as Path
import Kernel.Common.Module qualified as M
import Kernel.Common.Placeholder qualified as Placeholder
import Kernel.Common.RunProcess qualified as RunProcess
import Kernel.Common.Target
import Path
import System.Exit (ExitCode (..), exitWith)

data Handle = Handle
  { pathHandle :: Path.Handle,
    envHandle :: Env.Handle,
    moduleHandle :: GlobalModule.Handle,
    runProcessHandle :: RunProcess.Handle
  }

new :: Global.Handle -> Handle
new (Global.Handle {..}) = do
  let runProcessHandle = RunProcess.new loggerHandle
  Handle {..}

execute :: Handle -> MainTarget -> [String] -> App ()
execute h target args = do
  outputPath <- Path.getExecutableOutputPath (pathHandle h) target
  command <- resolveCommand h target outputPath
  run h $ T.unwords $ command : map (Placeholder.quote . T.pack) args

resolveCommand :: Handle -> MainTarget -> Path Abs File -> App T.Text
resolveCommand h target outputPath = do
  let mainModule = Env.getMainModule (envHandle h)
  case getExecuteCommand target of
    Nothing ->
      return $ Placeholder.quote $ T.pack (toFilePath outputPath)
    Just commandTemplate ->
      Placeholder.expand M.keyExecute (resolver h mainModule target outputPath) commandTemplate

resolver :: Handle -> M.MainModule -> MainTarget -> Path Abs File -> Placeholder.Resolver
resolver h mainModule target outputPath hint name = do
  case name of
    "executable" ->
      return $ Just $ Placeholder.quote $ T.pack (toFilePath outputPath)
    "target" ->
      return $ fmap Placeholder.quote $ getTargetName target
    _ ->
      Placeholder.mainModuleResolver (moduleHandle h) mainModule hint name

run :: Handle -> T.Text -> App ()
run h command = do
  exitCode <- liftIO $ RunProcess.runShellCommand (runProcessHandle h) (T.unpack command)
  case exitCode of
    ExitSuccess ->
      return ()
    ExitFailure {} ->
      liftIO $ exitWith exitCode
