module Scene.Parse
  ( parse,
    parseCachedStmtList,
  )
where

import Context.App
import Context.Global qualified as Global
import Context.Locator qualified as Locator
import Context.Throw qualified as Throw
import Context.UnusedImport qualified as UnusedImport
import Context.UnusedLocalLocator qualified as UnusedLocalLocator
import Context.UnusedPreset qualified as UnusedPreset
import Context.UnusedVariable qualified as UnusedVariable
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.ArgNum qualified as AN
import Entity.Cache qualified as Cache
import Entity.DefiniteDescription qualified as DD
import Entity.Foreign qualified as F
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.Ident.Reify
import Entity.Opacity qualified as O
import Entity.RawProgram
import Entity.Source qualified as Source
import Entity.Stmt
import Entity.StmtKind qualified as SK
import Path
import Scene.Parse.Core qualified as P
import Scene.Parse.Discern qualified as Discern
import Scene.Parse.Foreign
import Scene.Parse.Import
import Scene.Parse.Program qualified as Parse

parse :: Source.Source -> Either Cache.Cache T.Text -> App (Either Cache.Cache ([F.Foreign], [WeakStmt]))
parse source cacheOrContent = do
  result <- parseSource source cacheOrContent
  mMainDD <- Locator.getMainDefiniteDescription source
  case mMainDD of
    Just mainDD -> do
      ensureMain (newSourceHint $ Source.sourceFilePath source) mainDD
      return result
    Nothing ->
      return result

parseSource :: Source.Source -> Either Cache.Cache T.Text -> App (Either Cache.Cache ([F.Foreign], [WeakStmt]))
parseSource source cacheOrContent = do
  let path = Source.sourceFilePath source
  case cacheOrContent of
    Left cache -> do
      let stmtList = Cache.stmtList cache
      parseCachedStmtList stmtList
      saveTopLevelNames path $ map getStmtName stmtList
      return $ Left cache
    Right content -> do
      prog <- snd <$> P.parseFile True Parse.parseProgram path content
      Right <$> interpret source prog

saveTopLevelNames :: Path Abs File -> [(Hint, DD.DefiniteDescription)] -> App ()
saveTopLevelNames path topNameList = do
  globalNameList <- mapM (uncurry Global.lookup') topNameList
  let nameMap = Map.fromList $ zip (map snd topNameList) globalNameList
  Global.saveCurrentNameSet path nameMap

parseCachedStmtList :: [Stmt] -> App ()
parseCachedStmtList stmtList = do
  forM_ stmtList $ \stmt -> do
    case stmt of
      StmtDefine isConstLike stmtKind (SavedHint m) name impArgs expArgs _ _ -> do
        let expArgNames = map (\(_, x, _) -> toText x) expArgs
        let allArgNum = AN.fromInt $ length $ impArgs ++ expArgs
        Global.registerStmtDefine isConstLike m stmtKind name allArgNum expArgNames
      StmtDefineConst (SavedHint m) dd _ _ ->
        Global.registerStmtDefine True m (SK.Normal O.Clear) dd AN.zero []

ensureMain :: Hint -> DD.DefiniteDescription -> App ()
ensureMain m mainFunctionName = do
  mMain <- Global.lookup m mainFunctionName
  case mMain of
    Just (_, GN.TopLevelFunc _ _) ->
      return ()
    _ ->
      Throw.raiseError m "`main` is missing"

interpret :: Source.Source -> RawProgram -> App ([F.Foreign], [WeakStmt])
interpret currentSource (RawProgram m importOrNone foreignOrNone stmtList) = do
  interpretImport currentSource importOrNone >>= activateImport m
  foreign' <- interpretForeign foreignOrNone
  activateForeign foreign'
  stmtList' <- Discern.discernStmtList $ map fst stmtList
  Global.reportMissingDefinitions
  saveTopLevelNames (Source.sourceFilePath currentSource) $ getWeakStmtName stmtList'
  UnusedVariable.registerRemarks
  UnusedImport.registerRemarks
  UnusedLocalLocator.registerRemarks
  UnusedPreset.registerRemarks
  return (foreign', stmtList')

getWeakStmtName :: [WeakStmt] -> [(Hint, DD.DefiniteDescription)]
getWeakStmtName =
  concatMap getWeakStmtName'

getWeakStmtName' :: WeakStmt -> [(Hint, DD.DefiniteDescription)]
getWeakStmtName' stmt =
  case stmt of
    WeakStmtDefine _ _ m name _ _ _ _ ->
      [(m, name)]
    WeakStmtDefineConst m name _ _ ->
      [(m, name)]
    WeakStmtDeclare {} ->
      []

getStmtName :: Stmt -> (Hint, DD.DefiniteDescription)
getStmtName stmt =
  case stmt of
    StmtDefine _ _ (SavedHint m) name _ _ _ _ ->
      (m, name)
    StmtDefineConst (SavedHint m) name _ _ ->
      (m, name)
