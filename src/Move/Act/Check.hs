module Move.Act.Check
  ( Handle,
    new,
    check,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Console.Report qualified as Report
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Scene.Check qualified as Check
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Init.Base qualified as Base
import Rule.Config.Check
import Rule.Remark qualified as Remark

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
    then liftIO $ Report.printErrorList (Base.reportHandle (baseHandle h)) logs
    else liftIO $ Report.printErrorList (Base.reportHandle (baseHandle h)) $ map Remark.deactivatePadding logs

setup :: Handle -> EIO ()
setup h = do
  let fetchHandle = Fetch.new (baseHandle h)
  Fetch.fetch fetchHandle $ Env.getMainModule (Base.envHandle (baseHandle h))
