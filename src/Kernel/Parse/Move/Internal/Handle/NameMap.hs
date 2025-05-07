module Kernel.Parse.Move.Internal.Handle.NameMap
  ( Handle,
    new,
    registerStmtDefine,
    registerGeist,
    reportMissingDefinitions,
    activateTopLevelNames,
    lookup',
    lookup,
  )
where

import Control.Monad
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Error.Rule.EIO (EIO)
import Error.Rule.Error
import Kernel.Move.Context.Env qualified as Env
import Kernel.Move.Context.KeyArg qualified as KeyArg
import Kernel.Move.Context.Locator qualified as Locator
import Kernel.Move.Context.OptimizableData qualified as OptimizableData
import Kernel.Move.Context.Platform qualified as Platform
import Kernel.Move.Context.Tag qualified as Tag
import Kernel.Move.Scene.Init.Base qualified as Base
import Kernel.Parse.Move.Internal.Handle.Unused qualified as Unused
import Kernel.Rule.GlobalName
import Kernel.Rule.GlobalName qualified as GN
import Kernel.Rule.OptimizableData qualified as OD
import Kernel.Rule.TopNameMap
import Language.Common.Move.Raise (raiseCritical, raiseError)
import Language.Common.Rule.ArgNum qualified as AN
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.Discriminant qualified as D
import Language.Common.Rule.IsConstLike
import Language.Common.Rule.PrimOp.FromText qualified as PrimOp
import Language.Common.Rule.PrimType.FromText qualified as PT
import Language.Common.Rule.StmtKind qualified as SK
import Language.RawTerm.Rule.Key
import Language.RawTerm.Rule.RawTerm qualified as RT
import Logger.Rule.Hint
import Logger.Rule.Hint qualified as Hint
import Logger.Rule.Log (Log, newLog)
import Logger.Rule.LogLevel (LogLevel (Error))
import Prelude hiding (lookup)

data Handle = Handle
  { envHandle :: Env.Handle,
    platformHandle :: Platform.Handle,
    locatorHandle :: Locator.Handle,
    keyArgHandle :: KeyArg.Handle,
    optDataHandle :: OptimizableData.Handle,
    tagHandle :: Tag.Handle,
    unusedHandle :: Unused.Handle,
    nameMapRef :: IORef TopNameMap,
    geistMapRef :: IORef (Map.HashMap DD.DefiniteDescription (Hint, IsConstLike))
  }

new :: Base.Handle -> Locator.Handle -> Unused.Handle -> Tag.Handle -> IO Handle
new (Base.Handle {..}) locatorHandle unusedHandle tagHandle = do
  nameMapRef <- newIORef Map.empty
  geistMapRef <- newIORef Map.empty
  return $ Handle {..}

registerStmtDefine ::
  Handle ->
  IsConstLike ->
  Hint ->
  SK.BaseStmtKind DD.DefiniteDescription x t ->
  DD.DefiniteDescription ->
  AN.ArgNum ->
  [Key] ->
  EIO ()
registerStmtDefine h isConstLike m stmtKind name allArgNum expArgNames = do
  case stmtKind of
    SK.Normal _ ->
      registerTopLevelFunc h isConstLike m name allArgNum
    SK.Main {} ->
      registerTopLevelFunc h isConstLike m name allArgNum
    SK.Data dataName dataArgs consInfoList -> do
      registerData h isConstLike m dataName dataArgs consInfoList
      liftIO $ registerAsUnaryIfNecessary h dataName consInfoList
      liftIO $ registerAsEnumIfNecessary h dataName dataArgs consInfoList
    SK.DataIntro {} ->
      return ()
  KeyArg.insert (keyArgHandle h) m name isConstLike allArgNum expArgNames

registerAsEnumIfNecessary ::
  Handle ->
  DD.DefiniteDescription ->
  [a] ->
  [(SavedHint, DD.DefiniteDescription, IsConstLike, [a], D.Discriminant)] ->
  IO ()
registerAsEnumIfNecessary h dataName dataArgs consInfoList =
  when (hasNoArgs dataArgs consInfoList) $ do
    OptimizableData.insert (optDataHandle h) dataName OD.Enum
    forM_ consInfoList $ \(_, consName, _, _, _) -> do
      OptimizableData.insert (optDataHandle h) consName OD.Enum

hasNoArgs :: [a] -> [(c, DD.DefiniteDescription, b, [a], D.Discriminant)] -> Bool
hasNoArgs dataArgs consInfoList =
  null dataArgs && all (null . (\(_, _, _, consArgs, _) -> consArgs)) consInfoList

registerAsUnaryIfNecessary ::
  Handle ->
  DD.DefiniteDescription ->
  [(b, DD.DefiniteDescription, IsConstLike, [a], D.Discriminant)] ->
  IO ()
registerAsUnaryIfNecessary h dataName consInfoList = do
  case (isUnary consInfoList, length consInfoList == 1) of
    (True, _) -> do
      OptimizableData.insert (optDataHandle h) dataName OD.Unary
      forM_ consInfoList $ \(_, consName, _, _, _) -> do
        OptimizableData.insert (optDataHandle h) consName OD.Unary
    _ ->
      return ()

isUnary :: [(b, DD.DefiniteDescription, IsConstLike, [a], D.Discriminant)] -> Bool
isUnary consInfoList =
  case consInfoList of
    [(_, _, _, [_], _)] ->
      True
    _ ->
      False

registerGeist :: Handle -> RT.TopGeist -> EIO ()
registerGeist h RT.RawGeist {..} = do
  let expArgs' = RT.extractArgs expArgs
  let impArgs' = RT.extractArgs impArgs
  let expArgNames = map (\(_, x, _, _, _) -> x) expArgs'
  let argNum = AN.fromInt $ length $ impArgs' ++ expArgs'
  nameLifter <- liftIO $ Locator.getNameLifter (locatorHandle h)
  let name' = nameLifter $ fst name
  ensureGeistFreshness h loc name'
  ensureDefFreshness h loc name'
  KeyArg.insert (keyArgHandle h) loc name' isConstLike argNum expArgNames
  liftIO $ insertToGeistMap h name' loc isConstLike
  liftIO $ insertToNameMap h name' loc $ GN.TopLevelFunc argNum isConstLike

registerTopLevelFunc :: Handle -> IsConstLike -> Hint -> DD.DefiniteDescription -> AN.ArgNum -> EIO ()
registerTopLevelFunc h isConstLike m topLevelName allArgNum = do
  registerTopLevelFunc' h m topLevelName $ GN.TopLevelFunc allArgNum isConstLike

registerTopLevelFunc' :: Handle -> Hint -> DD.DefiniteDescription -> GN.GlobalName -> EIO ()
registerTopLevelFunc' h m topLevelName gn = do
  ensureDefFreshness h m topLevelName
  liftIO $ insertToNameMap h topLevelName m gn

registerData ::
  Handle ->
  IsConstLike ->
  Hint ->
  DD.DefiniteDescription ->
  [a] ->
  [(SavedHint, DD.DefiniteDescription, IsConstLike, [a], D.Discriminant)] ->
  EIO ()
registerData h isConstLike m dataName dataArgs consInfoList = do
  ensureDefFreshness h m dataName
  let dataArgNum = AN.fromInt $ length dataArgs
  let consNameArrowList = map (toConsNameArrow dataArgNum) consInfoList
  liftIO $ insertToNameMap h dataName m $ GN.Data dataArgNum consNameArrowList isConstLike
  forM_ consNameArrowList $ \(consDD, (mCons, gn)) -> do
    ensureDefFreshness h mCons consDD
    liftIO $ insertToNameMap h consDD mCons gn

toConsNameArrow ::
  AN.ArgNum ->
  (SavedHint, DD.DefiniteDescription, IsConstLike, [a], D.Discriminant) ->
  (DD.DefiniteDescription, (Hint, GN.GlobalName))
toConsNameArrow dataArgNum (SavedHint m, consDD, isConstLikeCons, consArgs, discriminant) = do
  let consArgNum = AN.fromInt $ length consArgs
  (consDD, (m, GN.DataIntro dataArgNum consArgNum discriminant isConstLikeCons))

lookup :: Handle -> Hint.Hint -> DD.DefiniteDescription -> EIO (Maybe (Hint, GlobalName))
lookup h m name = do
  nameMap <- liftIO $ readIORef (nameMapRef h)
  let dataSize = Platform.getDataSize (platformHandle h)
  case Map.lookup name nameMap of
    Just kind -> do
      liftIO $ Unused.deleteGlobalLocator (unusedHandle h) $ DD.globalLocator name
      liftIO $ Unused.deletePreset (unusedHandle h) $ DD.moduleID name
      return $ Just kind
    Nothing
      | Just primType <- PT.fromDefiniteDescription dataSize name ->
          return $ Just (m, GN.PrimType primType)
      | Just primOp <- PrimOp.fromDefiniteDescription dataSize name ->
          return $ Just (m, GN.PrimOp primOp)
      | otherwise -> do
          return Nothing

lookup' :: Handle -> Hint.Hint -> DD.DefiniteDescription -> EIO (Hint, GlobalName)
lookup' h m name = do
  mgn <- lookup h m name
  case mgn of
    Just gn ->
      return gn
    Nothing -> do
      let mainModule = Env.getMainModule (envHandle h)
      let name' = Locator.getReadableDD mainModule name
      raiseError m $ "No such top-level name is defined: " <> name'

ensureDefFreshness :: Handle -> Hint.Hint -> DD.DefiniteDescription -> EIO ()
ensureDefFreshness h m name = do
  gmap <- liftIO $ readIORef (geistMapRef h)
  topNameMap <- liftIO $ readIORef (nameMapRef h)
  case (Map.lookup name gmap, Map.member name topNameMap) of
    (Just _, False) -> do
      let mainModule = Env.getMainModule (envHandle h)
      let name' = Locator.getReadableDD mainModule name
      raiseCritical m $ "`" <> name' <> "` is defined nominally but not registered in the top name map"
    (Just (mGeist, isConstLike), True) -> do
      liftIO $ removeFromGeistMap h name
      liftIO $ removeFromDefNameMap h name
      liftIO $ Tag.insertGlobalVar (tagHandle h) mGeist name isConstLike m
    (Nothing, True) -> do
      let mainModule = Env.getMainModule (envHandle h)
      let name' = Locator.getReadableDD mainModule name
      raiseError m $ "`" <> name' <> "` is already defined"
    (Nothing, False) ->
      return ()

ensureGeistFreshness :: Handle -> Hint.Hint -> DD.DefiniteDescription -> EIO ()
ensureGeistFreshness h m name = do
  geistMap <- liftIO $ readIORef (geistMapRef h)
  when (Map.member name geistMap) $ do
    let mainModule = Env.getMainModule (envHandle h)
    let name' = Locator.getReadableDD mainModule name
    raiseError m $ "`" <> name' <> "` is already defined"

reportMissingDefinitions :: Handle -> EIO ()
reportMissingDefinitions h = do
  geistMap <- liftIO $ readIORef (geistMapRef h)
  let geistNameToHint = Map.toList geistMap
  let errorList = map (uncurry geistToRemark) geistNameToHint
  if null errorList
    then return ()
    else throwError $ MakeError errorList

geistToRemark :: DD.DefiniteDescription -> (Hint, a) -> Log
geistToRemark dd (m, _) =
  newLog m Error $ "This nominal definition of `" <> DD.localLocator dd <> "` lacks a real definition"

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
