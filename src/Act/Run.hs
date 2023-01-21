module Act.Run
  ( run,
    Config (..),
    Context,
  )
where

import Act.Build qualified as Build
import Context.Env qualified as Env
import Context.External qualified as External
import Context.Log qualified as Log
import Context.Throw qualified as Throw
import Entity.Module
import Entity.Module.Reflect qualified as Module
import Entity.Target
import Path

data Config = Config
  { target :: Target,
    mClangOptString :: Maybe String,
    shouldCancelAlloc :: Bool,
    logCfg :: Log.Config
  }

class
  ( Throw.Context m,
    Log.Context m,
    Env.Context m,
    Build.Context m,
    External.Context m
  ) =>
  Context m

toBuildConfig :: Config -> Build.Config
toBuildConfig cfg =
  Build.Config
    { Build.mTarget = Just $ target cfg,
      Build.mClangOptString = mClangOptString cfg,
      Build.shouldCancelAlloc = shouldCancelAlloc cfg,
      Build.logCfg = logCfg cfg
    }

run :: Context m => Config -> m ()
run cfg = do
  Env.setEndOfEntry $ Log.endOfEntry $ logCfg cfg
  Env.setShouldColorize $ Log.shouldColorize $ logCfg cfg
  Build.build (toBuildConfig cfg)
  mainModule <- Module.fromCurrentPath
  outputPath <- getExecutableOutputPath (target cfg) mainModule
  External.run (toFilePath outputPath) []
