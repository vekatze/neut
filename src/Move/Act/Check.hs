module Move.Act.Check (check) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Console.Report qualified as Report
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Scene.Check qualified as Check
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Initialize qualified as Initialize
import Rule.Config.Check
import Rule.Remark qualified as Remark

check :: Config -> App ()
check cfg = do
  setup cfg
  logs <-
    if shouldCheckAllDependencies cfg
      then Check.checkAll
      else Check.check
  hr <- Report.new
  if shouldInsertPadding cfg
    then liftIO $ Report.printErrorList hr logs
    else liftIO $ Report.printErrorList hr $ map Remark.deactivatePadding logs

setup :: Config -> App ()
setup cfg = do
  Initialize.initializeCompiler (remarkCfg cfg)
  h <- Fetch.new
  Env.getMainModule >>= toApp . Fetch.fetch h
