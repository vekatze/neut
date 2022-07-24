module Act.Run
  ( run,
    Config (..),
  )
where

import qualified Act.Build as Build
import qualified Context.Log as Log
import qualified Context.Mode as Mode
import qualified Context.Throw as Throw
import Entity.Module
import qualified Entity.Module.Reflect as Module
import Entity.Target
import Path
import System.Exit
import System.Process

data Config = Config
  { target :: Target,
    mClangOptString :: Maybe String,
    logCfg :: Log.Config,
    throwCfg :: Throw.Config,
    shouldCancelAlloc :: Bool
  }

toBuildConfig :: Config -> Build.Config
toBuildConfig cfg =
  Build.Config
    { Build.mTarget = Just $ target cfg,
      Build.mClangOptString = mClangOptString cfg,
      Build.logCfg = logCfg cfg,
      Build.throwCfg = throwCfg cfg,
      Build.shouldCancelAlloc = shouldCancelAlloc cfg
    }

run :: Mode.Mode -> Config -> IO ()
run mode cfg = do
  Build.build mode (toBuildConfig cfg)
  throwCtx <- Mode.throwCtx mode $ throwCfg cfg
  mainModule <- Module.fromCurrentPath throwCtx
  outputPath <- getExecutableOutputPath (target cfg) mainModule
  (_, _, _, handler) <- createProcess $ proc (toFilePath outputPath) []
  waitForProcess handler >>= exitWith
