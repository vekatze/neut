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
import Move.Context.Alias qualified as Alias
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.Env (getMainModule)
import Move.Context.Global qualified as Global
import Move.Context.Locator qualified as Locator
import Move.Language.Utility.Gensym qualified as Gensym
import Rule.DefiniteDescription qualified as DD
import Rule.Hint
import Rule.Ident
import Rule.Ident.Reify qualified as Ident
import Rule.Layer
import Rule.LocalLocator qualified as LL
import Rule.LocationTree qualified as LT
import Rule.Module
import Rule.NominalEnv
import Rule.OptimizableData
import Rule.VarDefKind

data Handle = Handle
  { mainModule :: MainModule,
    gensymHandle :: Gensym.Handle,
    locatorHandle :: Locator.Handle,
    globalHandle :: Global.Handle,
    aliasHandle :: Alias.Handle,
    nameEnv :: NominalEnv,
    currentLayer :: Layer,
    unusedVariableMapRef :: IORef (IntMap.IntMap (Hint, Ident, VarDefKind)),
    unusedLocalLocatorMapRef :: IORef (Map.HashMap LL.LocalLocator Hint),
    optDataMapRef :: IORef (Map.HashMap DD.DefiniteDescription OptimizableData),
    tagMapRef :: IORef LT.LocationTree
  }

new :: App Handle
new = do
  mainModule <- getMainModule
  gensymHandle <- Gensym.new
  locatorHandle <- Locator.new
  globalHandle <- Global.new
  aliasHandle <- Alias.new
  let nameEnv = empty
  unusedVariableMapRef <- asks App.unusedVariableMap
  unusedLocalLocatorMapRef <- asks App.unusedLocalLocatorMap
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
