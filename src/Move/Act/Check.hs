module Move.Act.Check (check) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Console.Report qualified as Report
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Check qualified as Check
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Init.Compiler qualified as InitCompiler
import Rule.Config.Check
import Rule.Remark qualified as Remark

check :: Config -> App ()
check cfg = do
  g <- Gensym.new
  hck <- Check.new g
  setup cfg g
  logs <-
    if shouldCheckAllDependencies cfg
      then Check.checkAll hck
      else Check.check hck
  hr <- Report.new
  if shouldInsertPadding cfg
    then liftIO $ Report.printErrorList hr logs
    else liftIO $ Report.printErrorList hr $ map Remark.deactivatePadding logs

setup :: Config -> Gensym.Handle -> App ()
setup cfg gensymHandle = do
  hc <- InitCompiler.new gensymHandle
  toApp $ InitCompiler.initializeCompiler hc (remarkCfg cfg)
  h <- Fetch.new gensymHandle
  he <- Env.new
  toApp (Env.getMainModule he) >>= toApp . Fetch.fetch h
