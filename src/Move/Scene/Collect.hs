module Move.Scene.Collect
  ( getMainTarget,
    collectModuleFiles,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.Maybe
import Data.Text qualified as T
import Move.Context.App
import Move.Context.Env qualified as Env
import Move.Context.Throw qualified as Throw
import Path
import Rule.Module
import Rule.Target
import Prelude hiding (log)

getMainTarget :: T.Text -> App MainTarget
getMainTarget targetName = do
  mainModule <- Env.getMainModule
  case getTarget (extractModule mainModule) targetName of
    Just target ->
      return target
    Nothing ->
      Throw.raiseError' $ "No such target exists: " <> targetName

collectModuleFiles :: MainModule -> (Path Abs Dir, [SomePath Rel])
collectModuleFiles (MainModule baseModule) = do
  let moduleRootDir = parent $ moduleLocation baseModule
  let relModuleSourceDir = Left $ moduleSourceDir baseModule
  let foreignContents = input $ moduleForeign baseModule
  let extraContents = moduleExtraContents baseModule
  let staticContents = map (\(_, path) -> Right path) $ Map.toList $ moduleStaticFiles baseModule
  (moduleRootDir, relModuleSourceDir : foreignContents ++ staticContents ++ extraContents)
