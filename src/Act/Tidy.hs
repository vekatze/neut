module Act.Tidy
  ( tidy,
    Config (..),
    Context,
  )
where

import Context.Env qualified as Env
import Context.Log qualified as Log
import Context.Module qualified as Module
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Entity.Module.Reflect qualified as Module
import Scene.Fetch qualified as F
import Scene.Parse.Core qualified as ParseCore
import Prelude hiding (log)

newtype Config = Config
  { logCfg :: Log.Config
  }

class
  ( Throw.Context m,
    Log.Context m,
    Path.Context m,
    Module.Context m,
    Env.Context m,
    ParseCore.Context m,
    F.Context m
  ) =>
  Context m

tidy :: Context m => Config -> m ()
tidy cfg = do
  Env.setEndOfEntry $ Log.endOfEntry $ logCfg cfg
  Env.setShouldColorize $ Log.shouldColorize $ logCfg cfg
  Throw.run $ do
    mainModule <- Module.fromCurrentPath
    Env.setMainModule mainModule
    F.fetch mainModule
