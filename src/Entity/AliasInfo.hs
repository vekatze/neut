module Entity.AliasInfo
  ( AliasInfo (..),
    SourceAliasMap,
    activateAliasInfo,
  )
where

import qualified Context.Alias as Alias
import qualified Data.HashMap.Strict as Map
import qualified Entity.GlobalLocatorAlias as GLA
import Entity.Hint
import qualified Entity.StrictGlobalLocator as SGL
import Path

data AliasInfo
  = Prefix Hint GLA.GlobalLocatorAlias SGL.StrictGlobalLocator
  deriving (Show)

type SourceAliasMap = Map.HashMap (Path Abs File) [AliasInfo]

activateAliasInfo :: Alias.Context m => [AliasInfo] -> m ()
activateAliasInfo aliasInfoList = do
  mapM_ activateAliasInfoOfCurrentFile' aliasInfoList

activateAliasInfoOfCurrentFile' :: Alias.Context m => AliasInfo -> m ()
activateAliasInfoOfCurrentFile' aliasInfo =
  case aliasInfo of
    Prefix m from to ->
      Alias.registerGlobalLocatorAlias m from to
