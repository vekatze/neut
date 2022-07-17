module Context.Type.Main (new) where

import qualified Context.Throw as Throw
import qualified Context.Type as Type
import Control.Monad
import qualified Data.HashMap.Strict as Map
import Data.IORef
import qualified Entity.DefiniteDescription as DD
import qualified Entity.Hint as H
import Entity.WeakTerm
import Prelude hiding (lookup)

new :: Type.Config -> IO Type.Context
new cfg = do
  _termTypeEnvRef <- newIORef Map.empty
  return
    Type.Context
      { Type.insert = insert _termTypeEnvRef,
        Type.lookup = lookup (Type.throwCtx cfg) _termTypeEnvRef
      }

insert ::
  IORef (Map.HashMap DD.DefiniteDescription WeakTerm) ->
  DD.DefiniteDescription ->
  WeakTerm ->
  IO ()
insert typeEnvRef name e =
  modifyIORef' typeEnvRef $ Map.insert name e

lookup ::
  Throw.Context ->
  IORef (Map.HashMap DD.DefiniteDescription WeakTerm) ->
  H.Hint ->
  DD.DefiniteDescription ->
  IO WeakTerm
lookup throwCtx typeEnvRef m name = do
  termTypeEnv <- readIORef typeEnvRef
  case Map.lookup name termTypeEnv of
    Nothing ->
      Throw.raiseCritical throwCtx m $
        "`" <> DD.reify name <> "` is not found in the term type environment."
    Just t ->
      return t
