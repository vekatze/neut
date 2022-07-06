module Entity.AliasInfo
  ( AliasInfo (..),
    activateAliasInfo,
    updateSourceAliasMapRef,
    sourceAliasMapRef,
  )
where

import qualified Context.Alias as Alias
import qualified Data.HashMap.Lazy as Map
import Data.IORef
import qualified Data.Text as T
import Entity.Hint
import Path
import System.IO.Unsafe

data AliasInfo
  = AliasInfoUse T.Text
  | AliasInfoPrefix Hint T.Text T.Text
  deriving (Show)

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

updateSourceAliasMapRef :: Path Abs File -> [AliasInfo] -> IO ()
updateSourceAliasMapRef path aliasInfoList = do
  modifyIORef' sourceAliasMapRef $ Map.insert path aliasInfoList

{-# NOINLINE sourceAliasMapRef #-}
sourceAliasMapRef :: IORef (Map.HashMap (Path Abs File) [AliasInfo])
sourceAliasMapRef =
  unsafePerformIO (newIORef Map.empty)
