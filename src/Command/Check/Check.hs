module Command.Check.Check
  ( Handle,
    new,
    check,
  )
where

import Command.Common.Check qualified as Check
import Command.Common.Fetch qualified as Fetch
import CommandParser.Config.Check
import Control.Monad.IO.Class (MonadIO (liftIO))
import Error.EIO (EIO)
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Logger.MoveLog qualified as Logger

newtype Handle = Handle
  { globalHandle :: Global.Handle
  }

new :: Global.Handle -> Handle
new globalHandle = do
  Handle {..}

check :: Handle -> Config -> EIO ()
check h cfg = do
  setup h
  let checkHandle = Check.new (globalHandle h)
  logs <-
    if shouldCheckAllDependencies cfg
      then Check.checkAll checkHandle
      else Check.check checkHandle
  liftIO $ Logger.printErrorList (Global.loggerHandle (globalHandle h)) logs

setup :: Handle -> EIO ()
setup h = do
  let fetchHandle = Fetch.new (globalHandle h)
  Fetch.fetch fetchHandle $ Env.getMainModule (Global.envHandle (globalHandle h))
