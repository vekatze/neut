module Act.Tidy
  ( tidy,
    Config (..),
    Context,
  )
where

import Context.Env qualified as Env
import Entity.Config.Tidy
import Scene.Fetch qualified as Fetch
import Scene.Initialize qualified as Initialize
import Prelude hiding (log)

class
  ( Env.Context m,
    Initialize.Context m,
    Fetch.Context m
  ) =>
  Context m

tidy :: Context m => Config -> m ()
tidy cfg = do
  Initialize.initializeCompiler (logCfg cfg) True Nothing
  Env.getMainModule >>= Fetch.fetch
