module Move.Act.Check
  ( Handle,
    new,
    check,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Logger.Move.Log qualified as Logger
import Logger.Rule.Log qualified as L
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Scene.Check qualified as Check
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Init.Base qualified as Base
import Rule.Config.Check

newtype Handle
  = Handle
  { baseHandle :: Base.Handle
  }

new :: Base.Handle -> Handle
new baseHandle = do
  Handle {..}

check :: Handle -> Config -> EIO ()
check h cfg = do
  setup h
  let checkHandle = Check.new (baseHandle h)
  logs <-
    if shouldCheckAllDependencies cfg
      then Check.checkAll checkHandle
      else Check.check checkHandle
  if shouldInsertPadding cfg
    then liftIO $ Logger.printErrorList (Base.loggerHandle (baseHandle h)) logs
    else liftIO $ Logger.printErrorList (Base.loggerHandle (baseHandle h)) $ map deactivatePadding logs

setup :: Handle -> EIO ()
setup h = do
  let fetchHandle = Fetch.new (baseHandle h)
  Fetch.fetch fetchHandle $ Env.getMainModule (Base.envHandle (baseHandle h))

deactivatePadding :: L.Log -> L.Log
deactivatePadding l =
  l {L.shouldInsertPadding = False}
