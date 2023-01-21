module Act.Get
  ( get,
    Config (..),
    Context,
  )
where

import Context.Env qualified as Env
import Context.Log qualified as Log
import Context.Module qualified as Module
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.Module.Reflect qualified as Module
import Entity.ModuleAlias
import Entity.ModuleURL
import Scene.Fetch qualified as F
import Prelude hiding (log)

data Config = Config
  { moduleAliasText :: T.Text,
    moduleURL :: ModuleURL,
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
  Env.setEndOfEntry $ Log.endOfEntry $ logCfg cfg
  Env.setShouldColorize $ Log.shouldColorize $ logCfg cfg
  Throw.run $ do
    Module.fromCurrentPath >>= Env.setMainModule
    baseName <- BN.reflect' $ moduleAliasText cfg
    F.insertDependency
      (ModuleAlias baseName)
      (moduleURL cfg)
