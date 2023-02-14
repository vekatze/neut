module Context.Alias where

import Entity.AliasInfo
import Entity.GlobalLocator qualified as GL
import Entity.GlobalLocatorAlias qualified as GLA
import Entity.Hint
import Entity.StrictGlobalLocator qualified as SGL

class Monad m => Context m where
  registerGlobalLocatorAlias :: Hint -> GLA.GlobalLocatorAlias -> SGL.StrictGlobalLocator -> m ()
  resolveAlias :: Hint -> GL.GlobalLocator -> m SGL.StrictGlobalLocator
  initializeAliasMap :: m ()

activateAliasInfo :: Context m => [AliasInfo] -> m ()
activateAliasInfo aliasInfoList = do
  mapM_ activateAliasInfoOfCurrentFile' aliasInfoList

activateAliasInfoOfCurrentFile' :: Context m => AliasInfo -> m ()
activateAliasInfoOfCurrentFile' aliasInfo =
  case aliasInfo of
    Prefix m from to ->
      registerGlobalLocatorAlias m from to
