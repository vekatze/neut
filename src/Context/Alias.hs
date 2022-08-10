module Context.Alias where

import qualified Context.Locator as Locator
import qualified Context.Throw as Throw
import qualified Entity.GlobalLocator as GL
import qualified Entity.GlobalLocatorAlias as GLA
import Entity.Hint
import Entity.Module
import qualified Entity.StrictGlobalLocator as SGL

-- data Config = Config
--   { currentModule :: Module,
--     mainModule :: Module,
--     throwCtx :: Throw.Context,
--     locatorCtx :: Locator.Context
--   }

class Monad m => Context m where
  registerGlobalLocatorAlias :: Hint -> GLA.GlobalLocatorAlias -> SGL.StrictGlobalLocator -> m ()
  resolveAlias :: Hint -> GL.GlobalLocator -> m SGL.StrictGlobalLocator
