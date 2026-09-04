module Command.Check.Check
  ( Handle,
    new,
    check,
  )
where

import App.App (App)
import App.Run (raiseError')
import Command.Common.Check qualified as Check
import Command.Common.Fetch qualified as Fetch
import CommandParser.Config.Check
import Control.Monad.IO.Class (MonadIO (liftIO))
import Kernel.Common.CreateGlobalHandle qualified as Global
import Data.Text qualified as T
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Module (extractModule, getTarget)
import Kernel.Common.Target
import Logger.Print qualified as Logger

newtype Handle = Handle
  { globalHandle :: Global.Handle
  }

new :: Global.Handle -> Handle
new globalHandle = do
  Handle {..}

check :: Handle -> Config -> App ()
check h cfg = do
  setup h
  mainTarget <- mapM (getMainTarget h) (targetName cfg)
  let checkHandle = Check.new (globalHandle h)
  logs <-
    if shouldCheckAllDependencies cfg
      then Check.checkAllOrFail checkHandle mainTarget
      else Check.checkOrFail checkHandle mainTarget
  liftIO $ Logger.printLogList (Global.loggerHandle (globalHandle h)) logs

getMainTarget :: Handle -> T.Text -> App MainTarget
getMainTarget h name = do
  let mainModule = Env.getMainModule (Global.envHandle (globalHandle h))
  case getTarget (extractModule mainModule) name of
    Just target ->
      return target
    Nothing ->
      raiseError' $ "No such target exists: " <> name

setup :: Handle -> App ()
setup h = do
  let fetchHandle = Fetch.new (globalHandle h)
  Fetch.fetch fetchHandle $ Env.getMainModule (Global.envHandle (globalHandle h))
