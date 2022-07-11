module Entity.AliasInfo
  ( AliasInfo (..),
    SourceAliasMap,
    activateAliasInfo,
  )
where

import qualified Context.Alias as Alias
import qualified Data.HashMap.Strict as Map
import qualified Entity.GlobalLocator as GL
import qualified Entity.GlobalLocatorAlias as GLA
import Entity.Hint
import Path

data AliasInfo
  = AliasInfoPrefix Hint GLA.GlobalLocatorAlias GL.GlobalLocator
  deriving (Show)

type SourceAliasMap = Map.HashMap (Path Abs File) [AliasInfo]

activateAliasInfo :: Alias.Context -> [AliasInfo] -> IO ()
activateAliasInfo ctx aliasInfoList = do
  mapM_ (activateAliasInfoOfCurrentFile' ctx) aliasInfoList

activateAliasInfoOfCurrentFile' :: Alias.Context -> AliasInfo -> IO ()
activateAliasInfoOfCurrentFile' ctx aliasInfo =
  case aliasInfo of
    AliasInfoPrefix m from to ->
      Alias.registerGlobalLocatorAlias ctx m from to
