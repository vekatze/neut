module Act.Run
  ( run,
    Config (..),
    Context,
  )
where

import qualified Act.Build as Build
import qualified Context.Env as Env
import qualified Context.External as External
import qualified Context.Log as Log
import qualified Context.Throw as Throw
import Entity.Module
import qualified Entity.Module.Reflect as Module
import qualified Entity.OutputKind as OK
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
      Build.logCfg = logCfg cfg,
      Build.outputKindList = [OK.Object],
      shouldSkipLink = False
    }

run :: Context m => Config -> m ()
run cfg = do
  Env.setEndOfEntry $ Log.endOfEntry $ logCfg cfg
  Env.setShouldColorize $ Log.shouldColorize $ logCfg cfg
  Build.build (toBuildConfig cfg)
  mainModule <- Module.fromCurrentPath
  outputPath <- getExecutableOutputPath (target cfg) mainModule
  External.run (toFilePath outputPath) []
