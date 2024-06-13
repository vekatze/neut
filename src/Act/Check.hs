module Act.Check (check) where

import Context.App
import Context.Module qualified as Module
import Context.Remark qualified as Remark
import Entity.Config.Check
import Entity.Remark qualified as Remark
import Scene.Check qualified as Check
import Scene.Fetch qualified as Fetch
import Scene.Initialize qualified as Initialize

check :: Config -> App ()
check cfg = do
  setup cfg
  logs <- Check.check
  if shouldInsertPadding cfg
    then Remark.printErrorList logs
    else Remark.printErrorList $ map Remark.deactivatePadding logs

setup :: Config -> App ()
setup cfg = do
  Initialize.initializeCompiler (remarkCfg cfg)
  Module.getMainModule >>= Fetch.fetch
