{-# LANGUAGE FlexibleInstances #-}

module Entity.AliasInfo
  ( AliasInfo (..),
    activateAliasInfo,
  )
where

import qualified Data.HashMap.Lazy as Map
import Data.IORef
import qualified Data.Text as T
import Entity.Hint
import Entity.Log
import Entity.Namespace
import Path
import System.IO.Unsafe

data AliasInfo
  = AliasInfoUse T.Text
  | AliasInfoPrefix Hint T.Text T.Text
  deriving (Show)

activateAliasInfo :: Path Abs File -> IO ()
activateAliasInfo path = do
  sourceAliasMap <- readIORef sourceAliasMapRef
  case Map.lookup path sourceAliasMap of
    Nothing ->
      raiseCritical' "[activateAliasInfoOfCurrentFile] (compiler bug)"
    Just aliasInfoList ->
      mapM_ activateAliasInfoOfCurrentFile' aliasInfoList

activateAliasInfoOfCurrentFile' :: AliasInfo -> IO ()
activateAliasInfoOfCurrentFile' aliasInfo =
  case aliasInfo of
    AliasInfoUse locator ->
      activateGlobalLocator locator
    AliasInfoPrefix m from to ->
      handleDefinePrefix m from to

{-# NOINLINE sourceAliasMapRef #-}
sourceAliasMapRef :: IORef (Map.HashMap (Path Abs File) [AliasInfo])
sourceAliasMapRef =
  unsafePerformIO (newIORef Map.empty)
