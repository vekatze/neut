module Move.Scene.Collect
  ( Handle,
    new,
    getMainTarget,
    collectModuleFiles,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.Maybe
import Data.Text qualified as T
import Move.Context.App
import Move.Context.EIO (EIO, raiseError', toApp)
import Move.Context.Env qualified as Env
import Path
import Rule.Module
import Rule.Target
import Prelude hiding (log)

newtype Handle
  = Handle
  { mainModule :: MainModule
  }

new :: App Handle
new = do
  he <- Env.new
  mainModule <- toApp $ Env.getMainModule he
  return $ Handle {..}

getMainTarget :: Handle -> T.Text -> EIO MainTarget
getMainTarget h targetName = do
  case getTarget (extractModule (mainModule h)) targetName of
    Just target ->
      return target
    Nothing ->
      raiseError' $ "No such target exists: " <> targetName

collectModuleFiles :: MainModule -> (Path Abs Dir, [SomePath Rel])
collectModuleFiles (MainModule baseModule) = do
  let moduleRootDir = parent $ moduleLocation baseModule
  let relModuleSourceDir = Left $ moduleSourceDir baseModule
  let foreignContents = input $ moduleForeign baseModule
  let extraContents = moduleExtraContents baseModule
  let staticContents = map (\(_, path) -> Right path) $ Map.toList $ moduleStaticFiles baseModule
  (moduleRootDir, relModuleSourceDir : foreignContents ++ staticContents ++ extraContents)
