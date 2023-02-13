module Act.Get
  ( get,
    Config (..),
    Context,
  )
where

import Entity.Config.Get
import Scene.Fetch qualified as Fetch
import Scene.Initialize qualified as Initialize
import Prelude hiding (log)

class
  ( Initialize.Context m,
    Fetch.Context m
  ) =>
  Context m

get :: Context m => Config -> m ()
get cfg = do
  Initialize.initializeCompiler (logCfg cfg) True
  Fetch.insertDependency (moduleAliasText cfg) (moduleURL cfg)
