module Main.Move.Scene.Parse
  ( Handle,
    new,
    parse,
    parseCachedStmtList,
    getUnusedLocators,
  )
where

import BasicParser.Move.Parse (runParser)
import Control.Monad
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Error.Rule.EIO (EIO)
import Language.Common.Rule.ArgNum qualified as AN
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.Ident.Reify
import Language.Common.Rule.LocalLocator qualified as LL
import Language.RawTerm.Rule.RawStmt
import Language.Term.Rule.Stmt
import Language.WeakTerm.Rule.WeakStmt
import Logger.Rule.Hint
import Logger.Rule.Log qualified as L
import Logger.Rule.LogLevel qualified as L
import Main.Move.Context.Cache qualified as Cache
import Main.Move.Context.Locator qualified as Locator
import Main.Move.Context.Path qualified as Path
import Main.Move.Context.Tag qualified as Tag
import Main.Move.Scene.Init.Base qualified as Base
import Main.Move.Scene.Init.Local qualified as Local
import Main.Move.Scene.Parse.Discern qualified as Discern
import Main.Move.Scene.Parse.Discern.Handle qualified as Discern
import Main.Move.Scene.Parse.Handle.Alias qualified as Alias
import Main.Move.Scene.Parse.Handle.GlobalNameMap qualified as GlobalNameMap
import Main.Move.Scene.Parse.Handle.NameMap qualified as NameMap
import Main.Move.Scene.Parse.Handle.Unused qualified as Unused
import Main.Move.Scene.Parse.Import qualified as Import
import Main.Move.Scene.Parse.Program qualified as Parse
import Main.Move.Scene.Parse.RawTerm qualified as ParseRT
import Main.Rule.Cache qualified as Cache
import Main.Rule.Import
import Main.Rule.Source qualified as Source
import Main.Rule.Target
import Main.Rule.UnusedGlobalLocators (UnusedGlobalLocators)
import Main.Rule.UnusedLocalLocators (UnusedLocalLocators)
import Main.Rule.VarDefKind

data Handle = Handle
  { parseHandle :: ParseRT.Handle,
    aliasHandle :: Alias.Handle,
    locatorHandle :: Locator.Handle,
    discernHandle :: Discern.Handle,
    pathHandle :: Path.Handle,
    importHandle :: Import.Handle,
    nameMapHandle :: NameMap.Handle,
    globalNameMapHandle :: GlobalNameMap.Handle,
    unusedHandle :: Unused.Handle
  }

new ::
  Base.Handle ->
  Local.Handle ->
  IO Handle
new baseHandle localHandle = do
  let unusedHandle = Local.unusedHandle localHandle
  let parseHandle = ParseRT.new (Base.gensymHandle baseHandle)
  let pathHandle = Base.pathHandle baseHandle
  let importHandle = Import.new baseHandle localHandle
  let globalNameMapHandle = Base.globalNameMapHandle baseHandle
  let aliasHandle = Local.aliasHandle localHandle
  let locatorHandle = Local.locatorHandle localHandle
  let tagHandle = Local.tagHandle localHandle
  nameMapHandle <- NameMap.new baseHandle locatorHandle unusedHandle tagHandle
  let discernHandle = Discern.new baseHandle localHandle nameMapHandle
  return $ Handle {..}

parse ::
  Handle ->
  Target ->
  Source.Source ->
  Either Cache.Cache T.Text ->
  EIO (Either Cache.Cache [WeakStmt], [L.Log])
parse h t source cacheOrContent = do
  parseSource h t source cacheOrContent

parseSource ::
  Handle ->
  Target ->
  Source.Source ->
  Either Cache.Cache T.Text ->
  EIO (Either Cache.Cache [WeakStmt], [L.Log])
parseSource h t source cacheOrContent = do
  let filePath = Source.sourceFilePath source
  case cacheOrContent of
    Left cache -> do
      let stmtList = Cache.stmtList cache
      parseCachedStmtList h stmtList
      saveTopLevelNames h source $ getStmtName stmtList
      return (Left cache, Cache.remarkList cache)
    Right fileContent -> do
      prog <- runParser filePath fileContent True (Parse.parseProgram (parseHandle h))
      (prog', logs) <- interpret h source (snd prog)
      tmap <- liftIO $ Tag.get (Discern.tagHandle (discernHandle h))
      Cache.saveLocationCache (pathHandle h) t source $ Cache.LocationCache tmap
      return (Right prog', logs)

parseCachedStmtList :: Handle -> [Stmt] -> EIO ()
parseCachedStmtList h stmtList = do
  forM_ stmtList $ \stmt -> do
    case stmt of
      StmtDefine isConstLike stmtKind (SavedHint m) name impArgs expArgs _ _ -> do
        let expArgNames = map (\(_, x, _) -> toText x) expArgs
        let allArgNum = AN.fromInt $ length $ impArgs ++ expArgs
        NameMap.registerStmtDefine (nameMapHandle h) isConstLike m stmtKind name allArgNum expArgNames
      StmtForeign {} ->
        return ()

interpret :: Handle -> Source.Source -> RawProgram -> EIO ([WeakStmt], [L.Log])
interpret h currentSource (RawProgram m importList stmtList) = do
  Import.interpretImport (importHandle h) m currentSource importList >>= activateImport h m
  stmtList' <- Discern.discernStmtList (discernHandle h) (Source.sourceModule currentSource) $ map fst stmtList
  NameMap.reportMissingDefinitions (nameMapHandle h)
  saveTopLevelNames h currentSource $ getWeakStmtName stmtList'
  logs1 <- liftIO $ registerUnusedVariableRemarks h
  logs2 <- liftIO $ registerUnusedGlobalLocatorRemarks h
  logs3 <- liftIO $ registerUnusedLocalLocatorRemarks h
  logs4 <- liftIO $ registerUnusedPresetRemarks h
  logs5 <- liftIO $ registerUnusedStaticFileRemarks h
  return (stmtList', logs1 ++ logs2 ++ logs3 ++ logs4 ++ logs5)

saveTopLevelNames :: Handle -> Source.Source -> [(Hint, DD.DefiniteDescription)] -> EIO ()
saveTopLevelNames h source topNameList = do
  globalNameList <- mapM (uncurry $ NameMap.lookup' (nameMapHandle h)) topNameList
  let nameMap = Map.fromList $ zip (map snd topNameList) globalNameList
  liftIO $ GlobalNameMap.insert (globalNameMapHandle h) (Source.sourceFilePath source) nameMap

getUnusedLocators :: Handle -> IO (UnusedGlobalLocators, UnusedLocalLocators)
getUnusedLocators h = do
  unusedGlobalLocators <- Unused.getGlobalLocator (unusedHandle h)
  unusedLocalLocators <- Unused.getLocalLocator (unusedHandle h)
  return (unusedGlobalLocators, unusedLocalLocators)

registerUnusedVariableRemarks :: Handle -> IO [L.Log]
registerUnusedVariableRemarks h = do
  unusedVars <- Unused.getVariable (unusedHandle h)
  return $ flip map unusedVars $ \(mx, x, k) ->
    case k of
      Normal ->
        L.newLog mx L.Warning $
          "Defined but not used: `" <> toText x <> "`"
      Borrowed ->
        L.newLog mx L.Warning $
          "Borrowed but not used: `" <> toText x <> "`"
      Relayed ->
        L.newLog mx L.Warning $
          "Relayed but not used: `" <> toText x <> "`"

registerUnusedGlobalLocatorRemarks :: Handle -> IO [L.Log]
registerUnusedGlobalLocatorRemarks h = do
  unusedGlobalLocatorMap <- Unused.getGlobalLocator (unusedHandle h)
  let unusedGlobalLocators = concatMap snd unusedGlobalLocatorMap
  return $ flip map unusedGlobalLocators $ \(m, locatorText) ->
    L.newLog m L.Warning $
      "Imported but not used: `" <> locatorText <> "`"

registerUnusedLocalLocatorRemarks :: Handle -> IO [L.Log]
registerUnusedLocalLocatorRemarks h = do
  unusedLocalLocatorMap <- Unused.getLocalLocator (unusedHandle h)
  return $ flip map unusedLocalLocatorMap $ \(ll, m) ->
    L.newLog m L.Warning $
      "Imported but not used: `" <> LL.reify ll <> "`"

registerUnusedPresetRemarks :: Handle -> IO [L.Log]
registerUnusedPresetRemarks h = do
  unusedPresets <- Unused.getPreset (unusedHandle h)
  return $ flip map (Map.toList unusedPresets) $ \(presetName, m) ->
    L.newLog m L.Warning $
      "Imported but not used: `" <> presetName <> "`"

registerUnusedStaticFileRemarks :: Handle -> IO [L.Log]
registerUnusedStaticFileRemarks h = do
  unusedStaticFiles <- Unused.getStaticFile (unusedHandle h)
  return $ flip map unusedStaticFiles $ \(k, m) ->
    L.newLog m L.Warning $
      "Imported but not used: `" <> k <> "`"

activateImport :: Handle -> Hint -> [ImportItem] -> EIO ()
activateImport h m sourceInfoList = do
  forM_ sourceInfoList $ \importItem -> do
    case importItem of
      ImportItem source aliasInfoList -> do
        let path = Source.sourceFilePath source
        namesInSource <- GlobalNameMap.lookup (globalNameMapHandle h) m path
        liftIO $ NameMap.activateTopLevelNames (nameMapHandle h) namesInSource
        forM_ aliasInfoList $ \aliasInfo ->
          Alias.activateAliasInfo (aliasHandle h) source namesInSource aliasInfo
      StaticKey pathList -> do
        forM_ pathList $ \(key, (mKey, path)) -> do
          Locator.activateStaticFile (locatorHandle h) mKey key path
