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
import Language.Common.NominalTag
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
import SyntaxTree.Series qualified as SE
import Prelude hiding (lookup)

data Handle = Handle
  { envHandle :: Env.Handle,
    platformHandle :: Platform.Handle,
    tagHandle :: Tag.Handle,
    unusedHandle :: Unused.Handle,
    nameMapRef :: IORef TopNameMap,
    geistMapRef :: IORef (Map.HashMap DD.DefiniteDescription (Hint, IsConstLike))
  }

type NameEntry =
  (DD.DefiniteDescription, TopNameInfo)

new :: Global.Handle -> Unused.Handle -> Tag.Handle -> IO Handle
new (Global.Handle {..}) unusedHandle tagHandle = do
  nameMapRef <- newIORef Map.empty
  geistMapRef <- newIORef Map.empty
  return $ Handle {..}

insert :: Handle -> [NameEntry] -> App ()
insert h nameArrowList = do
  forM_ nameArrowList $ \(dd, (m, mTag, gn)) -> do
    ensureDefFreshness h m dd mTag (getIsConstLike gn)
    liftIO $ insertToNameMap h dd m mTag gn

registerGeist :: Handle -> NominalTag -> RT.RawGeist DD.DefiniteDescription -> App ()
registerGeist h tag RT.RawGeist {..} = do
  let expArgs' = RT.extractArgs expArgs
  let impArgs' = RT.extractImpArgs impArgs
  let defaultArgs' = map fst $ SE.extract $ fst defaultArgs
  let argNum = AN.fromInt $ length impArgs' + length defaultArgs' + length expArgs'
  let name' = fst name
  ensureGeistFreshness h loc name'
  ensureDefFreshness h loc name' (Just tag) isConstLike
  liftIO $ insertToGeistMap h name' loc isConstLike
  liftIO $ insertToNameMap h name' loc (Just tag) $ do
    if isTermTag tag
      then GN.TopLevelFuncTerm argNum isConstLike (isMacroTag tag)
      else GN.TopLevelFuncType argNum isConstLike False

lookup :: Handle -> Hint.Hint -> DD.DefiniteDescription -> App (Maybe (Hint, GN.GlobalName))
lookup h m name = do
  nameMap <- liftIO $ readIORef (nameMapRef h)
  let dataSize = Platform.getDataSize (platformHandle h)
  case Map.lookup name nameMap of
    Just (mFound, _tag, gn) -> do
      liftIO $ Unused.deleteGlobalLocator (unusedHandle h) $ DD.globalLocator name
      return $ Just (mFound, gn)
    Nothing
      | Just primType <- PT.fromDefiniteDescription dataSize name ->
          return $ Just (m, GN.PrimType primType)
      | Just primOp <- PrimOp.fromDefiniteDescription dataSize name ->
          return $ Just (m, GN.PrimOp primOp)
      | otherwise -> do
          return Nothing

ensureDefFreshness :: Handle -> Hint.Hint -> DD.DefiniteDescription -> Maybe NominalTag -> Bool -> App ()
ensureDefFreshness h m name mTag isConstLike = do
  gmap <- liftIO $ readIORef (geistMapRef h)
  topNameMap <- liftIO $ readIORef (nameMapRef h)
  case (Map.lookup name gmap, Map.lookup name topNameMap) of
    (Just _, Nothing) -> do
      let mainModule = Env.getMainModule (envHandle h)
      let name' = readableDD mainModule name
      raiseCritical m $ "`" <> name' <> "` is defined nominally but not registered in the top name map"
    (Just (mGeist, isConstLike'), Just (_, mNominalTag, _)) -> do
      let mainModule = Env.getMainModule (envHandle h)
      let name' = readableDD mainModule name
      nominalTag <-
        case mNominalTag of
          Just tag ->
            return tag
          Nothing ->
            raiseError m $ "`" <> name' <> "` cannot be declared nominally"
      actualTag <-
        case mTag of
          Just tag ->
            return tag
          Nothing ->
            raiseError m $ "`" <> name' <> "` cannot be defined to satisfy a nominal declaration"
      when (nominalTag /= actualTag) $ do
        raiseError m $
          "`"
            <> name'
            <> "` is declared as `"
            <> nominalTagToText nominalTag
            <> "`, but defined as `"
            <> nominalTagToText actualTag
            <> "`"
      case (isConstLike', isConstLike) of
        (True, False) -> do
          raiseError m $ "`" <> name' <> "` is declared as a constant, but defined as a non-constant"
        (False, True) -> do
          raiseError m $ "`" <> name' <> "` is declared as a non-constant, but defined as a constant"
        _ -> do
          liftIO $ removeFromGeistMap h name
          liftIO $ removeFromDefNameMap h name
          liftIO $ Tag.insertGlobalVar (tagHandle h) mGeist name isConstLike m
    (Nothing, Just _) -> do
      let mainModule = Env.getMainModule (envHandle h)
      let name' = readableDD mainModule name
      raiseError m $ "`" <> name' <> "` is already defined"
    (Nothing, Nothing) ->
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

insertToNameMap :: Handle -> DD.DefiniteDescription -> Hint -> Maybe NominalTag -> GN.GlobalName -> IO ()
insertToNameMap h dd m mTag gn = do
  modifyIORef' (nameMapRef h) $ Map.insert dd (m, mTag, gn)

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
  forM_ (Map.toList namesInSource) $ \(dd, (mDef, mTag, gn)) ->
    insertToNameMap h dd mDef mTag gn

getGlobalNames :: [PostRawStmt] -> [NameEntry]
getGlobalNames stmtList = do
  concatMap _getGlobalNames stmtList

_getGlobalNames :: PostRawStmt -> [NameEntry]
_getGlobalNames stmt = do
  case stmt of
    PostRawStmtDefineTerm _ stmtKind (RT.RawDef {geist}) ->
      getGlobalNamesFromDefTerm stmtKind geist
    PostRawStmtDefineType _ stmtKind (RT.RawTypeDef {typeGeist}) ->
      getGlobalNamesFromDefType stmtKind typeGeist
    PostRawStmtVariadic kind m name -> do
      [(name, (m, Nothing, GN.Rule kind))]
    PostRawStmtNominal {} -> do
      []
    PostRawStmtDefineResource _ m (name, _) _ _ _ _ -> do
      [(name, (m, Just Alias, GN.TopLevelFuncType AN.zero True False))]
    PostRawStmtForeign {} ->
      []

getGlobalNamesFromDefTerm ::
  RawStmtKindTerm DD.DefiniteDescription ->
  RT.RawGeist DD.DefiniteDescription ->
  [NameEntry]
getGlobalNamesFromDefTerm stmtKind geist = do
  let name = fst $ RT.name geist
  let impArgs = RT.extractImpArgs $ RT.impArgs geist
  let defaultArgs = map fst $ SE.extract $ fst $ RT.defaultArgs geist
  let expArgs = RT.extractArgs $ RT.expArgs geist
  let isConstLike = RT.isConstLike geist
  let m = RT.loc geist
  let allArgNum = AN.fromInt $ length impArgs + length defaultArgs + length expArgs
  case stmtKindTermToNominalTag stmtKind of
    Just tag -> do
      let isMacro = stmtKindTermIsMacro stmtKind
      [(name, (m, Just tag, GN.TopLevelFuncTerm allArgNum isConstLike isMacro))]
    Nothing ->
      []

getGlobalNamesFromDefType ::
  RawStmtKindType DD.DefiniteDescription ->
  RT.RawGeist DD.DefiniteDescription ->
  [NameEntry]
getGlobalNamesFromDefType stmtKind geist = do
  let name = fst $ RT.name geist
  let impArgs = RT.extractImpArgs $ RT.impArgs geist
  let defaultArgs = map fst $ SE.extract $ fst $ RT.defaultArgs geist
  let expArgs = RT.extractArgs $ RT.expArgs geist
  let isConstLike = RT.isConstLike geist
  let m = RT.loc geist
  let allArgNum = AN.fromInt $ length impArgs + length defaultArgs + length expArgs
  let mTag = stmtKindTypeToNominalTag stmtKind
  case stmtKind of
    SK.Alias ->
      [(name, (m, mTag, GN.TopLevelFuncType allArgNum isConstLike False))]
    SK.AliasOpaque ->
      [(name, (m, mTag, GN.TopLevelFuncType allArgNum isConstLike False))]
    SK.Data dataName dataArgs consInfoList -> do
      let dataArgNum = AN.fromInt $ length dataArgs
      let consNameArrowList = map (toConsNameArrow dataArgNum) consInfoList
      (dataName, (m, mTag, GN.Data dataArgNum (map stripTag consNameArrowList) isConstLike)) : consNameArrowList

getGlobalNames' :: [Stmt] -> [NameEntry]
getGlobalNames' stmtList = do
  concatMap _getGlobalNames' stmtList

_getGlobalNames' :: Stmt -> [NameEntry]
_getGlobalNames' stmt = do
  case stmt of
    StmtDefine isConstLike stmtKind (SavedHint m) name impArgs defaultArgs expArgs _ _ -> do
      let defaultBinders = map fst defaultArgs
      let allArgNum = AN.fromInt $ length $ impArgs ++ defaultBinders ++ expArgs
      case stmtKindTermToNominalTag stmtKind of
        Just tag -> do
          let isMacro = stmtKindTermIsMacro stmtKind
          [(name, (m, Just tag, GN.TopLevelFuncTerm allArgNum isConstLike isMacro))]
        Nothing ->
          []
    StmtDefineType isConstLike stmtKind (SavedHint m) name impArgs defaultArgs expArgs _ _ -> do
      let defaultBinders = map fst defaultArgs
      let allArgNum = AN.fromInt $ length $ impArgs ++ defaultBinders ++ expArgs
      let mTag = stmtKindTypeToNominalTag stmtKind
      case stmtKind of
        SK.Alias ->
          [(name, (m, mTag, GN.TopLevelFuncType allArgNum isConstLike False))]
        SK.AliasOpaque ->
          [(name, (m, mTag, GN.TopLevelFuncType allArgNum isConstLike False))]
        SK.Data dataName dataArgs consInfoList -> do
          let dataArgNum = AN.fromInt $ length dataArgs
          let consNameArrowList = map (toConsNameArrow dataArgNum) consInfoList
          (dataName, (m, mTag, GN.Data dataArgNum (map stripTag consNameArrowList) isConstLike)) : consNameArrowList
    StmtVariadic kind (SavedHint m) name -> do
      [(name, (m, Nothing, GN.Rule kind))]
    StmtForeign {} ->
      []

toConsNameArrow ::
  AN.ArgNum ->
  (SavedHint, DD.DefiniteDescription, IsConstLike, [a], D.Discriminant) ->
  NameEntry
toConsNameArrow dataArgNum (SavedHint m, consDD, isConstLikeCons, consArgs, discriminant) = do
  let consArgNum = AN.fromInt $ length consArgs
  (consDD, (m, Nothing, GN.DataIntro dataArgNum consArgNum discriminant isConstLikeCons))

geistToRemark :: DD.DefiniteDescription -> (Hint, a) -> Log
geistToRemark dd (m, _) =
  newLog m Error $ "This nominal definition of `" <> DD.localLocator dd <> "` lacks a real definition"

stripTag :: NameEntry -> (DD.DefiniteDescription, (Hint, GN.GlobalName))
stripTag (dd, (m, _tag, gn)) =
  (dd, (m, gn))

stmtKindTermToNominalTag :: SK.BaseStmtKindTerm name binder t -> Maybe NominalTag
stmtKindTermToNominalTag stmtKind =
  case stmtKind of
    SK.Define ->
      Just Define
    SK.Inline ->
      Just Inline
    SK.Macro ->
      Just Macro
    SK.MacroInline ->
      Just MacroInline
    SK.Main _ ->
      Just Define
    SK.DataIntro {} ->
      Nothing

stmtKindTermIsMacro :: SK.BaseStmtKindTerm name binder t -> Bool
stmtKindTermIsMacro stmtKind =
  case stmtKind of
    SK.Macro ->
      True
    SK.MacroInline ->
      True
    _ ->
      False

stmtKindTypeToNominalTag :: SK.BaseStmtKindType name binder -> Maybe NominalTag
stmtKindTypeToNominalTag stmtKind =
  case stmtKind of
    SK.Alias ->
      Just Alias
    SK.AliasOpaque ->
      Just AliasOpaque
    SK.Data {} ->
      Just Data
