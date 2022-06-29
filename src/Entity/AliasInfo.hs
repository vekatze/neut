{-# LANGUAGE FlexibleInstances #-}

module Entity.AliasInfo
  ( AliasInfo (..),
    activateAliasInfo,
    updateSourceAliasMapRef,
  )
where

import Context.App
import qualified Context.Throw as Throw
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

activateAliasInfo :: Axis -> Path Abs File -> IO ()
activateAliasInfo axis path = do
  sourceAliasMap <- readIORef sourceAliasMapRef
  case Map.lookup path sourceAliasMap of
    Nothing ->
      axis & throw & Throw.raiseCritical' $ "[activateAliasInfoOfCurrentFile] (compiler bug)"
    Just aliasInfoList ->
      mapM_ (activateAliasInfoOfCurrentFile' axis) aliasInfoList

activateAliasInfoOfCurrentFile' :: Axis -> AliasInfo -> IO ()
activateAliasInfoOfCurrentFile' axis aliasInfo =
  case aliasInfo of
    AliasInfoUse locator ->
      activateGlobalLocator locator
    AliasInfoPrefix m from to ->
      handleDefinePrefix axis m from to

updateSourceAliasMapRef :: Path Abs File -> [AliasInfo] -> IO ()
updateSourceAliasMapRef path aliasInfoList = do
  modifyIORef' sourceAliasMapRef $ Map.insert path aliasInfoList

{-# NOINLINE sourceAliasMapRef #-}
sourceAliasMapRef :: IORef (Map.HashMap (Path Abs File) [AliasInfo])
sourceAliasMapRef =
  unsafePerformIO (newIORef Map.empty)
