module Move.Act.Check
  ( Handle,
    new,
    check,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Console.Report qualified as Report
import Move.Context.Antecedent qualified as Antecedent
import Move.Context.App
import Move.Context.Color qualified as Color
import Move.Context.EIO (EIO, toApp)
import Move.Context.Env qualified as Env
import Move.Context.Locator qualified as Locator
import Move.Context.Tag qualified as Tag
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Check qualified as Check
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Init.Compiler qualified as InitCompiler
import Rule.Config.Check
import Rule.Remark qualified as Remark

data Handle
  = Handle
  { reportHandle :: Report.Handle,
    initCompilerHandle :: InitCompiler.Handle,
    fetchHandle :: Fetch.Handle,
    envHandle :: Env.Handle,
    checkHandle :: Check.Handle
  }

new :: Env.Handle -> Gensym.Handle -> Color.Handle -> Locator.Handle -> Tag.Handle -> Antecedent.Handle -> App Handle
new envHandle gensymHandle colorHandle locatorHandle tagHandle antecedentHandle = do
  reportHandle <- Report.new
  initCompilerHandle <- InitCompiler.new envHandle gensymHandle colorHandle
  fetchHandle <- Fetch.new envHandle gensymHandle colorHandle
  checkHandle <- Check.new envHandle gensymHandle colorHandle locatorHandle tagHandle antecedentHandle
  return $ Handle {..}

check :: Handle -> Config -> App ()
check h cfg = do
  toApp $ setup h cfg
  logs <-
    if shouldCheckAllDependencies cfg
      then Check.checkAll (checkHandle h)
      else Check.check (checkHandle h)
  if shouldInsertPadding cfg
    then liftIO $ Report.printErrorList (reportHandle h) logs
    else liftIO $ Report.printErrorList (reportHandle h) $ map Remark.deactivatePadding logs

setup :: Handle -> Config -> EIO ()
setup h cfg = do
  InitCompiler.initializeCompiler (initCompilerHandle h) (remarkCfg cfg)
  Env.getMainModule (envHandle h) >>= Fetch.fetch (fetchHandle h)
