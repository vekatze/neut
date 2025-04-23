module Move.Scene.Parse.Discern.Handle
  ( Handle (..),
    new,
    extend,
    extend',
    extendWithoutInsert,
    extendByNominalEnv,
    lookupOD,
    deleteUnusedVariable,
    insertSymLoc,
    insertTopCandidate,
    insertExternalName,
    lookupExternalName,
    deleteUnusedStaticFile,
    getBuildMode,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.IntMap qualified as IntMap
import Data.Set qualified as S
import Data.Text qualified as T
import Move.Context.Alias qualified as Alias
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.EIO (EIO, raiseError)
import Move.Context.Env (getMainModule)
import Move.Context.Global qualified as Global
import Move.Context.KeyArg qualified as KeyArg
import Move.Context.Locator qualified as Locator
import Move.Language.Utility.Gensym qualified as Gensym
import Rule.BuildMode qualified as BM
import Rule.DefiniteDescription qualified as DD
import Rule.ExternalName qualified as EN
import Rule.Hint
import Rule.Ident
import Rule.Ident.Reify qualified as Ident
import Rule.Layer
import Rule.LocalLocator qualified as LL
import Rule.LocalVarTree qualified as LVT
import Rule.LocationTree qualified as LT
import Rule.Module
import Rule.NominalEnv
import Rule.OptimizableData
import Rule.TopCandidate
import Rule.VarDefKind

data Handle = Handle
  { mainModule :: MainModule,
    gensymHandle :: Gensym.Handle,
    locatorHandle :: Locator.Handle,
    globalHandle :: Global.Handle,
    aliasHandle :: Alias.Handle,
    keyArgHandle :: KeyArg.Handle,
    nameEnv :: NominalEnv,
    currentLayer :: Layer,
    unusedVariableMapRef :: IORef (IntMap.IntMap (Hint, Ident, VarDefKind)),
    unusedLocalLocatorMapRef :: IORef (Map.HashMap LL.LocalLocator Hint),
    usedVariableSetRef :: IORef (S.Set Int),
    topCandidateEnvRef :: IORef [TopCandidate],
    optDataMapRef :: IORef (Map.HashMap DD.DefiniteDescription OptimizableData),
    localVarMapRef :: IORef LVT.LocalVarTree,
    preDeclEnvRef :: IORef (Map.HashMap EN.ExternalName Hint),
    unusedStaticFileMapRef :: IORef (Map.HashMap T.Text Hint),
    buildModeRef :: IORef BM.BuildMode,
    tagMapRef :: IORef LT.LocationTree
  }

new :: App Handle
new = do
  mainModule <- getMainModule
  gensymHandle <- Gensym.new
  locatorHandle <- Locator.new
  globalHandle <- Global.new
  aliasHandle <- Alias.new
  keyArgHandle <- KeyArg.new
  let nameEnv = empty
  unusedVariableMapRef <- asks App.unusedVariableMap
  unusedLocalLocatorMapRef <- asks App.unusedLocalLocatorMap
  usedVariableSetRef <- asks App.usedVariableSet
  optDataMapRef <- asks App.optDataMap
  localVarMapRef <- asks App.localVarMap
  topCandidateEnvRef <- asks App.topCandidateEnv
  preDeclEnvRef <- asks App.preDeclEnv
  unusedStaticFileMapRef <- asks App.unusedStaticFileMap
  tagMapRef <- asks App.tagMap
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

lookupOD :: Handle -> DD.DefiniteDescription -> IO (Maybe OptimizableData)
lookupOD h dd = do
  optDataMap <- readIORef (optDataMapRef h)
  return $ Map.lookup dd optDataMap

insertSymLoc :: Handle -> Ident -> Loc -> Loc -> IO ()
insertSymLoc h x startLoc endLoc = do
  unless (isHole x) $ do
    modifyIORef' (localVarMapRef h) $ LVT.insert startLoc endLoc x

insertTopCandidate :: Handle -> TopCandidate -> IO ()
insertTopCandidate h cand = do
  modifyIORef' (topCandidateEnvRef h) $ (:) cand

insertExternalName :: Handle -> EN.ExternalName -> Hint -> IO ()
insertExternalName h k m =
  modifyIORef' (preDeclEnvRef h) $ Map.insert k m

lookupExternalName :: Handle -> Hint -> EN.ExternalName -> EIO Hint
lookupExternalName h m name = do
  preDeclEnv <- liftIO $ readIORef (preDeclEnvRef h)
  case Map.lookup name preDeclEnv of
    Just typeInfo ->
      return typeInfo
    Nothing -> do
      raiseError m $ "Undeclared function: " <> EN.reify name

deleteUnusedStaticFile :: Handle -> T.Text -> IO ()
deleteUnusedStaticFile h ll =
  modifyIORef' (unusedStaticFileMapRef h) $ Map.delete ll

getBuildMode :: Handle -> IO BM.BuildMode
getBuildMode h = do
  readIORef (buildModeRef h)
