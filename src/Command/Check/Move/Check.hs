module Command.Check.Move.Check
  ( Handle,
    new,
    check,
  )
where

import Command.Common.Move.Check qualified as Check
import Command.Common.Move.Fetch qualified as Fetch
import Control.Monad.IO.Class (MonadIO (liftIO))
import Kernel.Common.Move.CreateGlobalHandle qualified as Global
import Kernel.Common.Rule.Handle.Global.Env qualified as Env
import Library.CommandParser.Rule.Config.Check
import Library.Error.Rule.EIO (EIO)
import Library.Logger.Move.Log qualified as Logger
import Library.Logger.Rule.Log qualified as L

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
  if shouldInsertPadding cfg
    then liftIO $ Logger.printErrorList (Global.loggerHandle (globalHandle h)) logs
    else liftIO $ Logger.printErrorList (Global.loggerHandle (globalHandle h)) $ map deactivatePadding logs

setup :: Handle -> EIO ()
setup h = do
  let fetchHandle = Fetch.new (globalHandle h)
  Fetch.fetch fetchHandle $ Env.getMainModule (Global.envHandle (globalHandle h))

deactivatePadding :: L.Log -> L.Log
deactivatePadding l =
  l {L.shouldInsertPadding = False}
