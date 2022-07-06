module Act.Clean
  ( clean,
    Config (..),
  )
where

import qualified Context.Log as Log
import qualified Context.Mode as Mode
import qualified Context.Throw as Throw
import Control.Monad
import Entity.Module
import qualified Entity.Module.Reflect as Module
import Path.IO
import Prelude hiding (log)

data Config = Config
  { logCfg :: Log.Config,
    throwCfg :: Throw.Config
  }

clean :: Mode.Mode -> Config -> IO ()
clean mode cfg = do
  throwCtx <- Mode.throwCtx mode $ throwCfg cfg
  logCtx <- Mode.logCtx mode $ logCfg cfg
  Throw.run throwCtx (Log.printLog logCtx) $ do
    mainModule <- Module.fromCurrentPath throwCtx
    let targetDir = getTargetDir mainModule
    b <- doesDirExist targetDir
    when b $ removeDirRecur $ getTargetDir mainModule
