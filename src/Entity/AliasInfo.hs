module Entity.AliasInfo
  ( AliasInfo (..),
    SourceAliasMap,
    activateAliasInfo,
  )
where

import qualified Context.Alias as Alias
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import Entity.Hint
import Path

data AliasInfo
  = AliasInfoUse T.Text
  | AliasInfoPrefix Hint T.Text T.Text
  deriving (Show)

type SourceAliasMap = Map.HashMap (Path Abs File) [AliasInfo]

activateAliasInfo :: Alias.Context -> [AliasInfo] -> IO ()
activateAliasInfo ctx aliasInfoList = do
  mapM_ (activateAliasInfoOfCurrentFile' ctx) aliasInfoList

activateAliasInfoOfCurrentFile' :: Alias.Context -> AliasInfo -> IO ()
activateAliasInfoOfCurrentFile' ctx aliasInfo =
  case aliasInfo of
    AliasInfoUse _ ->
      return ()
    AliasInfoPrefix m from to ->
      Alias.registerLocatorAlias ctx m from to

-- updateSourceAliasMapRef :: Path Abs File -> [AliasInfo] -> IO ()
-- updateSourceAliasMapRef path aliasInfoList = do
--   modifyIORef' sourceAliasMapRef $ Map.insert path aliasInfoList

-- {-# NOINLINE sourceAliasMapRef #-}
-- sourceAliasMapRef :: IORef (Map.HashMap (Path Abs File) [AliasInfo])
-- sourceAliasMapRef =
--   unsafePerformIO (newIORef Map.empty)
