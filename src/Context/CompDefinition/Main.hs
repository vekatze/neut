module Context.CompDefinition.Main (new) where

import qualified Context.CompDefinition as CompDefinition
import Control.Monad
import qualified Data.HashMap.Strict as Map
import Data.IORef
import Prelude hiding (lookup)

new :: CompDefinition.Config -> IO CompDefinition.Context
new _ = do
  defMapRef <- newIORef Map.empty
  return $
    CompDefinition.Context
      { CompDefinition.insert =
          insert defMapRef,
        CompDefinition.union =
          union defMapRef,
        CompDefinition.lookup =
          lookup defMapRef
      }

insert ::
  IORef CompDefinition.DefMap ->
  CompDefinition.DefKey ->
  CompDefinition.DefValue ->
  IO ()
insert defMapRef k v = do
  modifyIORef' defMapRef $ Map.insert k v

union ::
  IORef CompDefinition.DefMap ->
  CompDefinition.DefMap ->
  IO ()
union defMapRef defMap = do
  modifyIORef' defMapRef $ Map.union defMap

lookup ::
  IORef CompDefinition.DefMap ->
  CompDefinition.DefKey ->
  IO (Maybe CompDefinition.DefValue)
lookup defMapRef k = do
  defMap <- readIORef defMapRef
  return $ Map.lookup k defMap
