module Command.Check.Move.Check
  ( Handle,
    new,
    check,
  )
where

import Command.Common.Move.Check qualified as Check
import Command.Common.Move.Fetch qualified as Fetch
import CommandParser.Rule.Config.Check
import Control.Monad.IO.Class (MonadIO (liftIO))
import Error.Rule.EIO (EIO)
import Kernel.Move.Context.Env qualified as Env
import Kernel.Move.Scene.Init.Global qualified as Global
import Logger.Move.Log qualified as Logger
import Logger.Rule.Log qualified as L

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
