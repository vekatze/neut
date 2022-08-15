module Context.Alias where

import qualified Entity.GlobalLocator as GL
import qualified Entity.GlobalLocatorAlias as GLA
import Entity.Hint
import qualified Entity.StrictGlobalLocator as SGL

class Monad m => Context m where
  registerGlobalLocatorAlias :: Hint -> GLA.GlobalLocatorAlias -> SGL.StrictGlobalLocator -> m ()
  resolveAlias :: Hint -> GL.GlobalLocator -> m SGL.StrictGlobalLocator
  initializeAliasMap :: m ()
