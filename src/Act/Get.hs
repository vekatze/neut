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
import Entity.BaseName qualified as BN
import Entity.Config.Get
import Entity.Module.Reflect qualified as Module
import Entity.ModuleAlias
import Scene.Fetch qualified as F
import Prelude hiding (log)

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
  Module.fromCurrentPath >>= Env.setMainModule
  Env.setTargetPlatform
  baseName <- BN.reflect' $ moduleAliasText cfg
  F.insertDependency
    (ModuleAlias baseName)
    (moduleURL cfg)
