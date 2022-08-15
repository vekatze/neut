module Act.Clean
  ( clean,
    Config (..),
    Context,
  )
where

import qualified Context.Env as Env
import qualified Context.Log as Log
import qualified Context.Path as Path
import qualified Context.Throw as Throw
import Control.Monad
import Entity.Module
import qualified Entity.Module.Reflect as Module
import qualified Scene.Parse.Core as Parse
import Prelude hiding (log)

newtype Config = Config
  { logCfg :: Log.Config
  }

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
  -- throwCtx <- Mode.throwCtx mode $ throwCfg cfg
  -- logCtx <- Mode.logCtx mode $ logCfg cfg
  Throw.run $ do
    mainModule <- Module.fromCurrentPath
    let targetDir = getTargetDir mainModule
    b <- Path.doesDirExist targetDir
    when b $ Path.removeDirRecur $ getTargetDir mainModule
