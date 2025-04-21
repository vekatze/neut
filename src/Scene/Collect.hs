module Scene.Collect
  ( getMainTarget,
    collectModuleFiles,
  )
where

import Context.App
import Context.Env qualified as Env
import Context.Throw qualified as Throw
import Data.HashMap.Strict qualified as Map
import Data.Maybe
import Data.Text qualified as T
import Rule.Module
import Rule.Target
import Path
import Prelude hiding (log)

getMainTarget :: T.Text -> App MainTarget
getMainTarget targetName = do
  targetOrNone <- flip getTarget targetName <$> Env.getMainModule
  case targetOrNone of
    Just target ->
      return target
    Nothing ->
      Throw.raiseError' $ "No such target exists: " <> targetName

collectModuleFiles :: Module -> (Path Abs Dir, [SomePath Rel])
collectModuleFiles baseModule = do
  let moduleRootDir = parent $ moduleLocation baseModule
  let relModuleSourceDir = Left $ moduleSourceDir baseModule
  let foreignContents = input $ moduleForeign baseModule
  let extraContents = moduleExtraContents baseModule
  let staticContents = map (\(_, path) -> Right path) $ Map.toList $ moduleStaticFiles baseModule
  (moduleRootDir, relModuleSourceDir : foreignContents ++ staticContents ++ extraContents)
