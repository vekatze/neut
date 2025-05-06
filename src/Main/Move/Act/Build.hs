module Main.Move.Act.Build
  ( Handle,
    new,
    build,
    toBuildConfig,
  )
where

import CommandParser.Rule.Config.Build
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as T
import Error.Rule.EIO (EIO)
import Language.Common.Move.Raise (raiseError')
import Main.Move.Context.Env qualified as Env
import Main.Move.Context.LLVM qualified as LLVM
import Main.Move.Context.Path qualified as Path
import Main.Move.Scene.Build qualified as Build
import Main.Move.Scene.Fetch qualified as Fetch
import Main.Move.Scene.Init.Base qualified as Base
import Main.Rule.Module
import Main.Rule.Target
import Prelude hiding (log)

newtype Handle = Handle
  { baseHandle :: Base.Handle
  }

new ::
  Base.Handle ->
  Handle
new baseHandle = do
  Handle {..}

build :: Handle -> Config -> EIO ()
build h cfg = do
  setup h cfg
  let buildHandle = Build.new (toBuildConfig cfg) (baseHandle h)
  target <- getMainTarget h $ targetName cfg
  let mainModule = Env.getMainModule (Base.envHandle (baseHandle h))
  Build.buildTarget buildHandle mainModule (Main target)

setup :: Handle -> Config -> EIO ()
setup h cfg = do
  LLVM.ensureSetupSanity cfg
  let mainModule = Env.getMainModule (Base.envHandle (baseHandle h))
  Path.ensureNotInDependencyDir mainModule
  liftIO $ Env.setBuildMode (Base.envHandle (baseHandle h)) $ buildMode cfg
  let fetchHandle = Fetch.new (baseHandle h)
  Fetch.fetch fetchHandle mainModule

toBuildConfig :: Config -> Build.Config
toBuildConfig cfg = do
  Build.Config
    { outputKindList = outputKindList cfg,
      shouldSkipLink = shouldSkipLink cfg,
      shouldExecute = shouldExecute cfg,
      installDir = installDir cfg,
      executeArgs = args cfg
    }

getMainTarget :: Handle -> T.Text -> EIO MainTarget
getMainTarget h targetName = do
  let mainModule = Env.getMainModule (Base.envHandle (baseHandle h))
  case getTarget (extractModule mainModule) targetName of
    Just target ->
      return target
    Nothing ->
      raiseError' $ "No such target exists: " <> targetName
