module Scene.Collect
  ( collectTargetList,
    collectModuleFiles,
  )
where

import Context.App
import Context.Module qualified as Module
import Data.Maybe
import Entity.Module
import Entity.Target
import Path
import Prelude hiding (log)

collectTargetList :: Maybe ConcreteTarget -> App [ConcreteTarget]
collectTargetList mTarget = do
  flip getTargetList mTarget <$> Module.getMainModule

collectModuleFiles :: Module -> (Path Abs Dir, [SomePath Rel])
collectModuleFiles baseModule = do
  let moduleRootDir = parent $ moduleLocation baseModule
  let relModuleSourceDir = Left $ moduleSourceDir baseModule
  let foreignContents = map Left $ moduleForeignDirList baseModule
  let extraContents = moduleExtraContents baseModule
  (moduleRootDir, relModuleSourceDir : foreignContents ++ extraContents)
