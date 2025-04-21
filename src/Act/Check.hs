module Act.Check (check) where

import Context.App
import Context.Env qualified as Env
import Context.Remark qualified as Remark
import Rule.Config.Check
import Rule.Remark qualified as Remark
import Scene.Check qualified as Check
import Scene.Fetch qualified as Fetch
import Scene.Initialize qualified as Initialize

check :: Config -> App ()
check cfg = do
  setup cfg
  logs <-
    if shouldCheckAllDependencies cfg
      then Check.checkAll
      else Check.check
  if shouldInsertPadding cfg
    then Remark.printErrorList logs
    else Remark.printErrorList $ map Remark.deactivatePadding logs

setup :: Config -> App ()
setup cfg = do
  Initialize.initializeCompiler (remarkCfg cfg)
  Env.getMainModule >>= Fetch.fetch
