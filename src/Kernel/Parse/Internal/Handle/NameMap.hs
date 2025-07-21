module Kernel.Parse.Internal.Handle.NameMap
  ( Handle,
    new,
    insert,
    registerGeist,
    reportMissingDefinitions,
    activateTopLevelNames,
    lookup,
    getGlobalNames,
    getGlobalNames',
  )
where

import App.App (App)
import App.Error
import App.Run (raiseCritical, raiseError)
import Control.Monad
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.GlobalName (getIsConstLike)
import Kernel.Common.GlobalName qualified as GN
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.Handle.Local.Tag qualified as Tag
import Kernel.Common.ReadableDD
import Kernel.Common.TopNameMap
import Kernel.Parse.Internal.Handle.Unused qualified as Unused
import Language.Common.ArgNum qualified as AN
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.IsConstLike
import Language.Common.PrimOp.FromText qualified as PrimOp
import Language.Common.PrimType.FromText qualified as PT
import Language.Common.StmtKind qualified as SK
import Language.RawTerm.RawStmt
import Language.RawTerm.RawTerm qualified as RT
import Language.Term.Stmt
import Logger.Hint
import Logger.Hint qualified as Hint
import Logger.Log (Log, newLog)
import Logger.LogLevel (LogLevel (Error))
import Prelude hiding (lookup)

data Handle = Handle
  { envHandle :: Env.Handle,
    platformHandle :: Platform.Handle,
    tagHandle :: Tag.Handle,
    unusedHandle :: Unused.Handle,
    nameMapRef :: IORef TopNameMap,
    geistMapRef :: IORef (Map.HashMap DD.DefiniteDescription (Hint, IsConstLike))
  }

new :: Global.Handle -> Unused.Handle -> Tag.Handle -> IO Handle
new (Global.Handle {..}) unusedHandle tagHandle = do
  nameMapRef <- newIORef Map.empty
  geistMapRef <- newIORef Map.empty
  return $ Handle {..}

insert :: Handle -> [(DD.DefiniteDescription, (Hint, GN.GlobalName))] -> App ()
insert h nameArrowList = do
  forM_ nameArrowList $ \(dd, (m, gn)) -> do
    ensureDefFreshness h m dd (getIsConstLike gn)
    liftIO $ insertToNameMap h dd m gn

registerGeist :: Handle -> RT.RawGeist DD.DefiniteDescription -> App ()
registerGeist h RT.RawGeist {..} = do
  let expArgs' = RT.extractArgs expArgs
  let impArgs' = RT.extractImpArgs impArgs
  let argNum = AN.fromInt $ length $ impArgs' ++ expArgs'
  let name' = fst name
  ensureGeistFreshness h loc name'
  ensureDefFreshness h loc name' isConstLike
  liftIO $ insertToGeistMap h name' loc isConstLike
  liftIO $ insertToNameMap h name' loc $ GN.TopLevelFunc argNum isConstLike

lookup :: Handle -> Hint.Hint -> DD.DefiniteDescription -> App (Maybe (Hint, GN.GlobalName))
lookup h m name = do
  nameMap <- liftIO $ readIORef (nameMapRef h)
  let dataSize = Platform.getDataSize (platformHandle h)
  case Map.lookup name nameMap of
    Just kind -> do
      liftIO $ Unused.deleteGlobalLocator (unusedHandle h) $ DD.globalLocator name
      return $ Just kind
    Nothing
      | Just primType <- PT.fromDefiniteDescription dataSize name ->
          return $ Just (m, GN.PrimType primType)
      | Just primOp <- PrimOp.fromDefiniteDescription dataSize name ->
          return $ Just (m, GN.PrimOp primOp)
      | otherwise -> do
          return Nothing

ensureDefFreshness :: Handle -> Hint.Hint -> DD.DefiniteDescription -> Bool -> App ()
ensureDefFreshness h m name isConstLike = do
  gmap <- liftIO $ readIORef (geistMapRef h)
  topNameMap <- liftIO $ readIORef (nameMapRef h)
  case (Map.lookup name gmap, Map.member name topNameMap) of
    (Just _, False) -> do
      let mainModule = Env.getMainModule (envHandle h)
      let name' = readableDD mainModule name
      raiseCritical m $ "`" <> name' <> "` is defined nominally but not registered in the top name map"
    (Just (mGeist, isConstLike'), True) -> do
      let mainModule = Env.getMainModule (envHandle h)
      let name' = readableDD mainModule name
      case (isConstLike', isConstLike) of
        (True, False) -> do
          raiseError m $ "`" <> name' <> "` is declared as a constant, but defined as a non-constant"
        (False, True) -> do
          raiseError m $ "`" <> name' <> "` is declared as a non-constant, but defined as a constant"
        _ -> do
          liftIO $ removeFromGeistMap h name
          liftIO $ removeFromDefNameMap h name
          liftIO $ Tag.insertGlobalVar (tagHandle h) mGeist name isConstLike m
    (Nothing, True) -> do
      let mainModule = Env.getMainModule (envHandle h)
      let name' = readableDD mainModule name
      raiseError m $ "`" <> name' <> "` is already defined"
    (Nothing, False) ->
      return ()

ensureGeistFreshness :: Handle -> Hint.Hint -> DD.DefiniteDescription -> App ()
ensureGeistFreshness h m name = do
  geistMap <- liftIO $ readIORef (geistMapRef h)
  when (Map.member name geistMap) $ do
    let mainModule = Env.getMainModule (envHandle h)
    let name' = readableDD mainModule name
    raiseError m $ "`" <> name' <> "` is already defined"

reportMissingDefinitions :: Handle -> App ()
reportMissingDefinitions h = do
  geistMap <- liftIO $ readIORef (geistMapRef h)
  let geistNameToHint = Map.toList geistMap
  let errorList = map (uncurry geistToRemark) geistNameToHint
  if null errorList
    then return ()
    else throwError $ MakeError errorList

insertToNameMap :: Handle -> DD.DefiniteDescription -> Hint -> GN.GlobalName -> IO ()
insertToNameMap h dd m gn = do
  modifyIORef' (nameMapRef h) $ Map.insert dd (m, gn)

insertToGeistMap :: Handle -> DD.DefiniteDescription -> Hint -> IsConstLike -> IO ()
insertToGeistMap h dd m isConstLike = do
  modifyIORef' (geistMapRef h) $ Map.insert dd (m, isConstLike)

removeFromGeistMap :: Handle -> DD.DefiniteDescription -> IO ()
removeFromGeistMap h dd = do
  modifyIORef' (geistMapRef h) $ Map.delete dd

removeFromDefNameMap :: Handle -> DD.DefiniteDescription -> IO ()
removeFromDefNameMap h dd = do
  modifyIORef' (nameMapRef h) $ Map.delete dd

activateTopLevelNames :: Handle -> TopNameMap -> IO ()
activateTopLevelNames h namesInSource = do
  forM_ (Map.toList namesInSource) $ \(dd, (mDef, gn)) ->
    insertToNameMap h dd mDef gn

getGlobalNames :: [PostRawStmt] -> [(DD.DefiniteDescription, (Hint, GN.GlobalName))]
getGlobalNames stmtList = do
  concatMap _getGlobalNames stmtList

_getGlobalNames :: PostRawStmt -> [(DD.DefiniteDescription, (Hint, GN.GlobalName))]
_getGlobalNames stmt = do
  case stmt of
    PostRawStmtDefine _ stmtKind (RT.RawDef {geist}) -> do
      let name = fst $ RT.name geist
      let impArgs = RT.extractImpArgs $ RT.impArgs geist
      let expArgs = RT.extractArgs $ RT.expArgs geist
      let isConstLike = RT.isConstLike geist
      let m = RT.loc geist
      let allArgNum = AN.fromInt $ length $ impArgs ++ expArgs
      case stmtKind of
        SK.Normal _ -> do
          [(name, (m, GN.TopLevelFunc allArgNum isConstLike))]
        SK.Main {} ->
          [(name, (m, GN.TopLevelFunc allArgNum isConstLike))]
        SK.Data dataName dataArgs consInfoList -> do
          let dataArgNum = AN.fromInt $ length dataArgs
          let consNameArrowList = map (toConsNameArrow dataArgNum) consInfoList
          (dataName, (m, GN.Data dataArgNum consNameArrowList isConstLike)) : consNameArrowList
        SK.DataIntro {} ->
          []
    PostRawStmtVariadic kind m name -> do
      [(name, (m, GN.Rule kind))]
    PostRawStmtNominal {} -> do
      []
    PostRawStmtDefineResource _ m (name, _) _ _ _ _ -> do
      [(name, (m, GN.TopLevelFunc AN.zero True))]
    PostRawStmtForeign {} ->
      []

getGlobalNames' :: [Stmt] -> [(DD.DefiniteDescription, (Hint, GN.GlobalName))]
getGlobalNames' stmtList = do
  concatMap _getGlobalNames' stmtList

_getGlobalNames' :: Stmt -> [(DD.DefiniteDescription, (Hint, GN.GlobalName))]
_getGlobalNames' stmt = do
  case stmt of
    StmtDefine isConstLike stmtKind (SavedHint m) name impArgs expArgs _ _ -> do
      let impBinders = map fst impArgs
      let allArgNum = AN.fromInt $ length $ impBinders ++ expArgs
      case stmtKind of
        SK.Normal _ -> do
          [(name, (m, GN.TopLevelFunc allArgNum isConstLike))]
        SK.Main {} ->
          [(name, (m, GN.TopLevelFunc allArgNum isConstLike))]
        SK.Data dataName dataArgs consInfoList -> do
          let dataArgNum = AN.fromInt $ length dataArgs
          let consNameArrowList = map (toConsNameArrow dataArgNum) consInfoList
          (dataName, (m, GN.Data dataArgNum consNameArrowList isConstLike)) : consNameArrowList
        SK.DataIntro {} ->
          []
    StmtVariadic kind (SavedHint m) name -> do
      [(name, (m, GN.Rule kind))]
    StmtForeign {} ->
      []

toConsNameArrow ::
  AN.ArgNum ->
  (SavedHint, DD.DefiniteDescription, IsConstLike, [a], D.Discriminant) ->
  (DD.DefiniteDescription, (Hint, GN.GlobalName))
toConsNameArrow dataArgNum (SavedHint m, consDD, isConstLikeCons, consArgs, discriminant) = do
  let consArgNum = AN.fromInt $ length consArgs
  (consDD, (m, GN.DataIntro dataArgNum consArgNum discriminant isConstLikeCons))

geistToRemark :: DD.DefiniteDescription -> (Hint, a) -> Log
geistToRemark dd (m, _) =
  newLog m Error $ "This nominal definition of `" <> DD.localLocator dd <> "` lacks a real definition"
