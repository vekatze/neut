module Scene.Parse
  ( parse,
    parseCachedStmtList,
  )
where

import Context.Alias qualified as Alias
import Context.App
import Context.Decl qualified as Decl
import Context.Global qualified as Global
import Context.Locator qualified as Locator
import Context.Throw qualified as Throw
import Context.UnusedImport qualified as UnusedImport
import Context.UnusedLocalLocator qualified as UnusedLocalLocator
import Context.UnusedPreset qualified as UnusedPreset
import Context.UnusedVariable qualified as UnusedVariable
import Control.Monad
import Control.Monad.Trans
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.ArgNum qualified as AN
import Entity.C
import Entity.Cache qualified as Cache
import Entity.DeclarationName qualified as DN
import Entity.DefiniteDescription qualified as DD
import Entity.Foreign qualified as F
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.Ident.Reify
import Entity.Opacity qualified as O
import Entity.Source qualified as Source
import Entity.Stmt
import Entity.StmtKind qualified as SK
import Path
import Scene.Parse.Core qualified as P
import Scene.Parse.Discern qualified as Discern
import Scene.Parse.Import
import Scene.Parse.Stmt qualified as Parse
import Text.Megaparsec hiding (parse)

parse :: Source.Source -> Either Cache.Cache T.Text -> App (Either Cache.Cache ([WeakStmt], [F.Foreign]))
parse source cacheOrContent = do
  result <- parseSource source cacheOrContent
  mMainDD <- Locator.getMainDefiniteDescription source
  case mMainDD of
    Just mainDD -> do
      ensureMain (newSourceHint $ Source.sourceFilePath source) mainDD
      return result
    Nothing ->
      return result

parseSource :: Source.Source -> Either Cache.Cache T.Text -> App (Either Cache.Cache ([WeakStmt], [F.Foreign]))
parseSource source cacheOrContent = do
  let path = Source.sourceFilePath source
  case cacheOrContent of
    Left cache -> do
      let stmtList = Cache.stmtList cache
      parseCachedStmtList stmtList
      saveTopLevelNames path $ map getStmtName stmtList
      return $ Left cache
    Right content -> do
      (_, (defList, declList)) <- P.parseFile True (program source) path content
      stmtList <- Discern.discernStmtList $ map fst defList
      Global.reportMissingDefinitions
      saveTopLevelNames path $ getWeakStmtName stmtList
      UnusedVariable.registerRemarks
      UnusedImport.registerRemarks
      UnusedLocalLocator.registerRemarks
      UnusedPreset.registerRemarks
      return $ Right (stmtList, declList)

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

program :: Source.Source -> P.Parser ([(RawStmt, C)], [F.Foreign])
program currentSource = do
  m <- P.getCurrentHint
  importBlockOrNone <- Parse.parseImport
  declListOrNone <- Parse.parseForeign
  case importBlockOrNone of
    Nothing ->
      return ()
    Just (importBlock, _) -> do
      sourceInfoList <- lift $ interpretImportBlock currentSource importBlock
      forM_ sourceInfoList $ \(source, aliasInfoList) -> do
        let path = Source.sourceFilePath source
        namesInSource <- lift $ Global.lookupSourceNameMap m path
        lift $ Global.activateTopLevelNames namesInSource
        forM_ aliasInfoList $ \aliasInfo ->
          lift $ Alias.activateAliasInfo namesInSource aliasInfo
  declList' <-
    case declListOrNone of
      Nothing ->
        return []
      Just (declList, _) -> do
        let declList' = interpretForeign declList
        forM_ declList' $ \(F.Foreign name domList cod) -> do
          lift $ Decl.insDeclEnv' (DN.Ext name) domList cod
        return declList'
  defList <- concat <$> many parseStmt <* eof
  return (defList, declList')

parseRawProgram :: P.Parser RawProgram
parseRawProgram = do
  importBlockOrNone <- Parse.parseImport
  foreignOrNone <- Parse.parseForeign
  defList <- concat <$> many parseStmt
  return $ RawProgram importBlockOrNone foreignOrNone defList

parseStmt :: P.Parser [(RawStmt, C)]
parseStmt = do
  choice
    [ return <$> Parse.parseDefine,
      return <$> Parse.parseData,
      return <$> Parse.parseInline,
      return <$> Parse.parseConstant,
      return <$> Parse.parseDeclare,
      return <$> Parse.parseResource
    ]

interpretForeign :: RawForeign -> [F.Foreign]
interpretForeign rf =
  case rf of
    RawForeign _ (_, xs) ->
      map (interpretForeignItem . snd) xs

interpretForeignItem :: RawForeignItem -> F.Foreign
interpretForeignItem (RawForeignItem name _ lts _ (cod, _)) =
  F.Foreign name (map fst $ distillArgList lts) cod

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
