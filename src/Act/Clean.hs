module Act.Clean
  ( clean,
    Config (..),
    Context,
  )
where

import Context.Env qualified as Env
import Context.Log qualified as Log
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Control.Monad
import Entity.Config.Clean
import Entity.Module
import Entity.Module.Reflect qualified as Module
import Scene.Parse.Core qualified as Parse
import Prelude hiding (log)

class
  ( Log.Context m,
    Throw.Context m,
    Parse.Context m,
    Path.Context m
  ) =>
  Context m

clean :: Context m => Config -> m ()
clean cfg = do
  Env.setEndOfEntry $ Log.endOfEntry $ logCfg cfg
  Env.setShouldColorize $ Log.shouldColorize $ logCfg cfg
  mainModule <- Module.fromCurrentPath
  let targetDir = getTargetDir mainModule
  b <- Path.doesDirExist targetDir
  when b $ Path.removeDirRecur $ getTargetDir mainModule
