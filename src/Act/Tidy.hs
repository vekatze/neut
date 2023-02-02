module Act.Tidy
  ( tidy,
    Config (..),
    Context,
  )
where

import qualified Context.Env as Env
import qualified Context.Log as Log
import qualified Context.Module as Module
import qualified Context.Path as Path
import qualified Context.Throw as Throw
import qualified Entity.Module.Reflect as Module
import qualified Scene.Fetch as F
import qualified Scene.Parse.Core as ParseCore
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
