module Act.Check (check) where

import Context.App
import Context.Remark qualified as Remark
import Context.Throw qualified as Throw
import Entity.Config.Check
import Scene.Check qualified as Check
import Scene.Initialize qualified as Initialize

check :: Config -> App ()
check cfg = do
  let runner = if shouldInsertPadding cfg then id else Throw.run'
  logs <- runner $ do
    Initialize.initializeCompiler (remarkCfg cfg) Nothing
    Check.check (mFilePathString cfg)
  Remark.printErrorList logs
