module Move.Scene.Parse.Discern.Handle
  ( Handle (..),
    new,
    extend,
    extend',
    extendWithoutInsert,
    extendByNominalEnv,
  )
where

import Control.Monad.Reader (asks)
import Data.IORef
import Data.IntMap qualified as IntMap
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Rule.Hint
import Rule.Ident
import Rule.Ident.Reify qualified as Ident
import Rule.Layer
import Rule.Module
import Rule.NominalEnv
import Rule.VarDefKind

data Handle = Handle
  { nameEnv :: NominalEnv,
    currentModule :: Module,
    currentLayer :: Layer,
    unusedVariableMapRef :: IORef (IntMap.IntMap (Hint, Ident, VarDefKind))
  }

new :: Module -> App Handle
new currentModule = do
  let nameEnv = empty
  unusedVariableMapRef <- asks App.unusedVariableMap
  let currentLayer = 0
  return $ Handle {..}

extend :: Handle -> Hint -> Ident -> Layer -> VarDefKind -> IO Handle
extend h m newVar l k = do
  insertUnusedVariable h m newVar k
  return $ h {nameEnv = (Ident.toText newVar, (m, newVar, l)) : nameEnv h}

extend' :: Handle -> Hint -> Ident -> VarDefKind -> IO Handle
extend' h m newVar k = do
  extend h m newVar (currentLayer h) k

extendWithoutInsert :: Handle -> Hint -> Ident -> Handle
extendWithoutInsert h m newVar = do
  h {nameEnv = (Ident.toText newVar, (m, newVar, currentLayer h)) : nameEnv h}

extendByNominalEnv :: Handle -> VarDefKind -> NominalEnv -> IO Handle
extendByNominalEnv h k newNominalEnv = do
  case newNominalEnv of
    [] ->
      return h
    (_, (m, x, l)) : rest -> do
      h' <- extend h m x l k
      extendByNominalEnv h' k rest

insertUnusedVariable :: Handle -> Hint -> Ident -> VarDefKind -> IO ()
insertUnusedVariable h m x k =
  modifyIORef' (unusedVariableMapRef h) $ IntMap.insert (Ident.toInt x) (m, x, k)
