module Move.Scene.Parse.Discern.Handle
  ( Handle (..),
    new,
    extend,
    extend',
    extendWithoutInsert,
    extendByNominalEnv,
    deleteUnusedVariable,
    deleteUnusedStaticFile,
    getBuildMode,
  )
where

import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.IntMap qualified as IntMap
import Data.Set qualified as S
import Data.Text qualified as T
import Move.Context.Alias qualified as Alias
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.Env (getMainModule)
import Move.Context.Global qualified as Global
import Move.Context.KeyArg qualified as KeyArg
import Move.Context.Locator qualified as Locator
import Move.Context.OptimizableData qualified as OptimizableData
import Move.Context.PreDecl qualified as PreDecl
import Move.Context.SymLoc qualified as SymLoc
import Move.Context.Tag qualified as Tag
import Move.Context.TopCandidate qualified as TopCandidate
import Move.Context.UnusedLocalLocator qualified as UnusedLocalLocator
import Move.Language.Utility.Gensym qualified as Gensym
import Rule.BuildMode qualified as BM
import Rule.Hint
import Rule.Ident
import Rule.Ident.Reify qualified as Ident
import Rule.Layer
import Rule.Module
import Rule.NominalEnv
import Rule.VarDefKind

data Handle = Handle
  { mainModule :: MainModule,
    gensymHandle :: Gensym.Handle,
    locatorHandle :: Locator.Handle,
    globalHandle :: Global.Handle,
    aliasHandle :: Alias.Handle,
    tagHandle :: Tag.Handle,
    keyArgHandle :: KeyArg.Handle,
    symLocHandle :: SymLoc.Handle,
    topCandidateHandle :: TopCandidate.Handle,
    preDeclHandle :: PreDecl.Handle,
    optDataHandle :: OptimizableData.Handle,
    nameEnv :: NominalEnv,
    currentLayer :: Layer,
    unusedVariableMapRef :: IORef (IntMap.IntMap (Hint, Ident, VarDefKind)),
    unusedLocalLocatorHandle :: UnusedLocalLocator.Handle,
    usedVariableSetRef :: IORef (S.Set Int),
    unusedStaticFileMapRef :: IORef (Map.HashMap T.Text Hint),
    buildModeRef :: IORef BM.BuildMode
  }

new :: App Handle
new = do
  mainModule <- getMainModule
  gensymHandle <- Gensym.new
  locatorHandle <- Locator.new
  globalHandle <- Global.new
  aliasHandle <- Alias.new
  keyArgHandle <- KeyArg.new
  symLocHandle <- SymLoc.new
  topCandidateHandle <- TopCandidate.new
  tagHandle <- Tag.new
  preDeclHandle <- PreDecl.new
  optDataHandle <- OptimizableData.new
  let nameEnv = empty
  unusedVariableMapRef <- asks App.unusedVariableMap
  unusedLocalLocatorHandle <- UnusedLocalLocator.new
  usedVariableSetRef <- asks App.usedVariableSet
  unusedStaticFileMapRef <- asks App.unusedStaticFileMap
  buildModeRef <- asks App.buildMode
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

deleteUnusedVariable :: Handle -> Ident -> IO ()
deleteUnusedVariable h x =
  modifyIORef' (usedVariableSetRef h) $ S.insert (Ident.toInt x)

deleteUnusedStaticFile :: Handle -> T.Text -> IO ()
deleteUnusedStaticFile h ll =
  modifyIORef' (unusedStaticFileMapRef h) $ Map.delete ll

getBuildMode :: Handle -> IO BM.BuildMode
getBuildMode h = do
  readIORef (buildModeRef h)
