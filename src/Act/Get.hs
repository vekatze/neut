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
    Env.setTargetPlatform
    baseName <- BN.reflect' $ moduleAliasText cfg
    F.insertDependency
      (ModuleAlias baseName)
      (moduleURL cfg)
