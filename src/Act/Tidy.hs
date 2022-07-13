module Act.Tidy
  ( tidy,
    Config (..),
  )
where

import qualified Context.Log as Log
import qualified Context.Mode as Mode
import qualified Context.Module as Module
import qualified Context.Path as Path
import qualified Context.Throw as Throw
import qualified Entity.Module.Reflect as Module
import qualified Scene.Fetch as F
import System.IO
import Prelude hiding (log)

data Config = Config
  { throwCfg :: Throw.Config,
    logCfg :: Log.Config
  }

tidy :: Mode.Mode -> Config -> IO ()
tidy mode cfg = do
  _throwCtx <- Mode.throwCtx mode (throwCfg cfg)
  _logCtx <- Mode.logCtx mode (logCfg cfg)
  _pathCtx <- Mode.pathCtx mode $ Path.Config {Path.throwCtx = _throwCtx}
  Throw.run _throwCtx (Log.printLog _logCtx) $ do
    mainModule <- Module.fromCurrentPath _throwCtx
    _moduleCtx <-
      Mode.moduleCtx mode $
        Module.Config
          { Module.mainModule = mainModule,
            Module.throwCtx = _throwCtx,
            Module.pathCtx = _pathCtx
          }
    let ctx = F.Context {F.throwCtx = _throwCtx, F.logCtx = _logCtx, F.moduleCtx = _moduleCtx}
    F.fetch ctx mainModule
