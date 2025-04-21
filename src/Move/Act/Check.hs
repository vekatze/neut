module Move.Act.Check (check) where

import Move.Context.App
import Move.Context.Env qualified as Env
import Move.Context.Remark qualified as Remark
import Rule.Config.Check
import Rule.Remark qualified as Remark
import Move.Scene.Check qualified as Check
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Initialize qualified as Initialize

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
