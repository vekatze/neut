module Act.Get
  ( get,
    Config (..),
  )
where

import qualified Context.Log as Log
import qualified Context.Mode as Mode
import qualified Context.Module as Module
import qualified Context.Path as Path
import qualified Context.Throw as Throw
import qualified Entity.Module.Reflect as Module
import Entity.ModuleAlias
import Entity.ModuleURL
import qualified Scene.Fetch as F
import System.IO
import Prelude hiding (log)

data Config = Config
  { moduleAlias :: ModuleAlias,
    moduleURL :: ModuleURL,
    throwCfg :: Throw.Config,
    logCfg :: Log.Config
  }

get :: Mode.Mode -> Config -> IO ()
get mode cfg = do
  throwCtx <- Mode.throwCtx mode (throwCfg cfg)
  logCtx <- Mode.logCtx mode (logCfg cfg)
  pathCtx <- Mode.pathCtx mode $ Path.Config {Path.throwCtx = throwCtx}
  Throw.run throwCtx (Log.printLog logCtx) $ do
    mainModule <- Module.fromCurrentPath throwCtx
    moduleCtx <-
      Mode.moduleCtx mode $
        Module.Config
          { Module.mainModule = mainModule,
            Module.throwCtx = throwCtx,
            Module.pathCtx = pathCtx
          }
    let ctx = F.Context {F.throwCtx = throwCtx, F.logCtx = logCtx, F.moduleCtx = moduleCtx}
    F.insertDependency
      ctx
      mainModule
      (moduleAlias cfg)
      (moduleURL cfg)
