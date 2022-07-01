module Entity.AliasInfo
  ( AliasInfo (..),
    activateAliasInfo,
    updateSourceAliasMapRef,
  )
where

import Context.Throw
import Data.Function
import qualified Data.HashMap.Lazy as Map
import Data.IORef
import qualified Data.Text as T
import Entity.Hint
import Entity.Namespace
import Path
import System.IO.Unsafe

data AliasInfo
  = AliasInfoUse T.Text
  | AliasInfoPrefix Hint T.Text T.Text
  deriving (Show)

activateAliasInfo :: Context -> Path Abs File -> IO ()
activateAliasInfo context path = do
  sourceAliasMap <- readIORef sourceAliasMapRef
  case Map.lookup path sourceAliasMap of
    Nothing ->
      context & raiseCritical' $ "[activateAliasInfoOfCurrentFile] (compiler bug)"
    Just aliasInfoList ->
      mapM_ (activateAliasInfoOfCurrentFile' context) aliasInfoList

activateAliasInfoOfCurrentFile' :: Context -> AliasInfo -> IO ()
activateAliasInfoOfCurrentFile' context aliasInfo =
  case aliasInfo of
    AliasInfoUse locator ->
      activateGlobalLocator locator
    AliasInfoPrefix m from to ->
      handleDefinePrefix context m from to

updateSourceAliasMapRef :: Path Abs File -> [AliasInfo] -> IO ()
updateSourceAliasMapRef path aliasInfoList = do
  modifyIORef' sourceAliasMapRef $ Map.insert path aliasInfoList

{-# NOINLINE sourceAliasMapRef #-}
sourceAliasMapRef :: IORef (Map.HashMap (Path Abs File) [AliasInfo])
sourceAliasMapRef =
  unsafePerformIO (newIORef Map.empty)
