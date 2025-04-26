module Move.Act.Check (check) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Console.Report qualified as Report
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Scene.Check qualified as Check
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Init.Compiler qualified as InitCompiler
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
  hc <- InitCompiler.new
  toApp $ InitCompiler.initializeCompiler hc (remarkCfg cfg)
  h <- Fetch.new
  he <- Env.new
  toApp (Env.getMainModule he) >>= toApp . Fetch.fetch h
