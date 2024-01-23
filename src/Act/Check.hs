module Act.Check (check) where

import Context.App
import Context.Remark qualified as Remark
import Entity.Config.Check
import Entity.Remark qualified as Remark
import Scene.Check qualified as Check
import Scene.Initialize qualified as Initialize

check :: Config -> App ()
check cfg = do
  Initialize.initializeCompiler (remarkCfg cfg) Nothing
  logs <- Check.check
  if shouldInsertPadding cfg
    then Remark.printErrorList logs
    else Remark.printErrorList $ map Remark.deactivatePadding logs
