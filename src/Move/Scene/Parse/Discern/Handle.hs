module Move.Scene.Parse.Discern.Handle
  ( Handle (..),
    new,
    extend,
    extend',
    extendWithoutInsert,
    extendByNominalEnv,
    lookupOD,
  )
where

import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.IntMap qualified as IntMap
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.Env (getMainModule)
import Move.Language.Utility.Gensym qualified as Gensym
import Rule.DefiniteDescription qualified as DD
import Rule.Hint
import Rule.Ident
import Rule.Ident.Reify qualified as Ident
import Rule.Layer
import Rule.LocationTree qualified as LT
import Rule.Module
import Rule.NominalEnv
import Rule.OptimizableData
import Rule.VarDefKind

data Handle = Handle
  { mainModule :: MainModule,
    gensymHandle :: Gensym.Handle,
    nameEnv :: NominalEnv,
    currentModule :: Module,
    currentLayer :: Layer,
    unusedVariableMapRef :: IORef (IntMap.IntMap (Hint, Ident, VarDefKind)),
    optDataMapRef :: IORef (Map.HashMap DD.DefiniteDescription OptimizableData),
    tagMapRef :: IORef LT.LocationTree
  }

new :: Module -> App Handle
new currentModule = do
  mainModule <- getMainModule
  gensymHandle <- Gensym.new
  let nameEnv = empty
  unusedVariableMapRef <- asks App.unusedVariableMap
  optDataMapRef <- asks App.optDataMap
  tagMapRef <- asks App.tagMap
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

lookupOD :: Handle -> DD.DefiniteDescription -> IO (Maybe OptimizableData)
lookupOD h dd = do
  optDataMap <- readIORef (optDataMapRef h)
  return $ Map.lookup dd optDataMap
