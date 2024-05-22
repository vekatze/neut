module Scene.Collect
  ( getConcreteTarget,
    collectModuleFiles,
  )
where

import Context.App
import Context.Module qualified as Module
import Context.Throw qualified as Throw
import Data.Maybe
import Data.Text qualified as T
import Entity.Module
import Entity.Target
import Path
import Prelude hiding (log)

getConcreteTarget :: T.Text -> App ConcreteTarget
getConcreteTarget targetName = do
  targetOrNone <- flip getTarget targetName <$> Module.getMainModule
  case targetOrNone of
    Just target ->
      return target
    Nothing ->
      Throw.raiseError' $ "no such target exists: " <> targetName

collectModuleFiles :: Module -> (Path Abs Dir, [SomePath Rel])
collectModuleFiles baseModule = do
  let moduleRootDir = parent $ moduleLocation baseModule
  let relModuleSourceDir = Left $ moduleSourceDir baseModule
  let foreignContents = input $ moduleForeign baseModule
  let extraContents = moduleExtraContents baseModule
  (moduleRootDir, relModuleSourceDir : foreignContents ++ extraContents)
