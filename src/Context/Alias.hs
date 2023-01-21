module Context.Alias where

import Entity.GlobalLocator qualified as GL
import Entity.GlobalLocatorAlias qualified as GLA
import Entity.Hint
import Entity.StrictGlobalLocator qualified as SGL

class Monad m => Context m where
  registerGlobalLocatorAlias :: Hint -> GLA.GlobalLocatorAlias -> SGL.StrictGlobalLocator -> m ()
  resolveAlias :: Hint -> GL.GlobalLocator -> m SGL.StrictGlobalLocator
  initializeAliasMap :: m ()
