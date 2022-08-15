module Act.Get
  ( get,
    Config (..),
    Context,
  )
where

import qualified Context.Env as Env
import qualified Context.Log as Log
import qualified Context.Module as Module
import qualified Context.Path as Path
import qualified Context.Throw as Throw
import qualified Data.Text as T
import qualified Entity.BaseName as BN
import qualified Entity.Module.Reflect as Module
import Entity.ModuleAlias
import Entity.ModuleURL
import qualified Scene.Fetch as F
import Prelude hiding (log)

data Config = Config
  { moduleAliasText :: T.Text,
    moduleURL :: ModuleURL,
    -- throwCfg :: Throw.Config,
    logCfg :: Log.Config
  }

class
  ( Throw.Context m,
    Log.Context m,
    Module.Context m,
    Path.Context m,
    F.Context m
  ) =>
  Context m

get :: Context m => Config -> m ()
get cfg = do
  -- throwCtx <- Mode.throwCtx mode (throwCfg cfg)
  -- logCtx <- Mode.logCtx mode (logCfg cfg)
  Env.setEndOfEntry $ Log.endOfEntry $ logCfg cfg
  Env.setShouldColorize $ Log.shouldColorize $ logCfg cfg

  -- pathCtx <- Mode.pathCtx mode $ Path.Config {Path.throwCtx = throwCtx}
  Throw.run $ do
    Module.fromCurrentPath >>= Env.setMainModule
    -- moduleCtx <-
    --   Mode.moduleCtx mode $
    --     Module.Config
    --       { Module.mainModule = mainModule,
    --         Module.throwCtx = throwCtx,
    --         Module.pathCtx = pathCtx
    --       }
    -- let ctx = F.Context {F.throwCtx = throwCtx, F.logCtx = logCtx, F.moduleCtx = moduleCtx}
    baseName <- BN.reflect' $ moduleAliasText cfg
    F.insertDependency
      (ModuleAlias baseName)
      (moduleURL cfg)
