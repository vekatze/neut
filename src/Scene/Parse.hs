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
import Data.Maybe
import Data.Text qualified as T
import Entity.ArgNum qualified as AN
import Entity.BaseName qualified as BN
import Entity.C
import Entity.Cache qualified as Cache
import Entity.DeclarationName qualified as DN
import Entity.DefiniteDescription qualified as DD
import Entity.ExternalName qualified as EN
import Entity.Foreign qualified as F
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.Ident.Reify
import Entity.IsConstLike
import Entity.Name
import Entity.Opacity qualified as O
import Entity.RawBinder
import Entity.RawDecl qualified as RD
import Entity.RawTerm qualified as RT
import Entity.Source qualified as Source
import Entity.Stmt
import Entity.StmtKind qualified as SK
import Path
import Scene.Parse.Core qualified as P
import Scene.Parse.Discern qualified as Discern
import Scene.Parse.Import qualified as Parse
import Scene.Parse.RawTerm
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
      (defList, declList) <- P.run (program source) path content
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
  importBlockOrNone <- Parse.parseImportBlock
  declList <- parseForeignList
  case importBlockOrNone of
    Nothing ->
      return ()
    Just importBlock -> do
      sourceInfoList <- lift $ Parse.interpretImportBlock currentSource importBlock
      forM_ sourceInfoList $ \(source, aliasInfoList) -> do
        let path = Source.sourceFilePath source
        namesInSource <- lift $ Global.lookupSourceNameMap m path
        lift $ Global.activateTopLevelNames namesInSource
        forM_ aliasInfoList $ \aliasInfo ->
          lift $ Alias.activateAliasInfo namesInSource aliasInfo
  let declList' = concatMap interpretForeign $ maybeToList declList
  forM_ declList' $ \(F.Foreign name domList cod) -> do
    lift $ Decl.insDeclEnv' (DN.Ext name) domList cod
  defList <- concat <$> many parseStmt <* eof
  return (defList, declList')

parseStmt :: P.Parser [(RawStmt, C)]
parseStmt = do
  choice
    [ return <$> parseDefine O.Opaque,
      return <$> parseDefineData,
      return <$> parseDefine O.Clear,
      return <$> parseConstant,
      return <$> parseDeclare,
      return <$> parseDefineResource
    ]

interpretForeign :: RawForeign -> [F.Foreign]
interpretForeign rf =
  case rf of
    RawForeign _ (_, (xs, _)) ->
      map (interpretForeignItem . snd) xs

interpretForeignItem :: RawForeignItem -> F.Foreign
interpretForeignItem (RawForeignItem name _ lts _ (cod, _)) =
  F.Foreign name (map fst $ distillArgList lts) cod

parseForeignList :: P.Parser (Maybe RawForeign)
parseForeignList = do
  optional $ do
    c1 <- P.keyword' "foreign"
    val <- P.betweenBrace' (P.manyList' parseForeign)
    return $ RawForeign c1 val

parseForeign :: P.Parser RawForeignItem
parseForeign = do
  (declName, c1) <- P.symbol'
  lts <- P.argList'' lowType
  c2 <- P.delimiter' ":"
  (cod, c) <- lowType
  return $ RawForeignItem (EN.ExternalName declName) c1 lts c2 (cod, c)

parseDeclare :: P.Parser (RawStmt, C)
parseDeclare = do
  c1 <- P.keyword' "declare"
  m <- P.getCurrentHint
  (c2, (decls, c3)) <- P.betweenBrace' $ P.manyList $ parseDeclareItem Locator.attachCurrentLocator
  return (RawStmtDeclare c1 m c2 decls, c3)

parseDefine :: O.Opacity -> P.Parser (RawStmt, C)
parseDefine opacity = do
  c1 <-
    case opacity of
      O.Opaque ->
        P.keyword' "define"
      O.Clear ->
        P.keyword' "inline"
  m <- P.getCurrentHint
  (((_, (name, c)), impArgs, expArgs, codType), e) <- parseTopDefInfo
  name' <- lift $ Locator.attachCurrentLocator name
  lift $ defineFunction c1 (SK.Normal opacity) m (name', c) impArgs expArgs codType e

defineFunction ::
  C ->
  SK.RawStmtKind ->
  Hint ->
  (DD.DefiniteDescription, C) ->
  RD.ImpArgs ->
  RD.ExpArgs ->
  (C, (RT.RawTerm, C)) ->
  (C, ((RT.RawTerm, C), C)) ->
  App (RawStmt, C)
defineFunction c1 stmtKind m name impArgs expArgs codType (c2, (e, c)) = do
  return (RawStmtDefine c1 False stmtKind m name impArgs expArgs codType (c2, e), c)

parseConstant :: P.Parser (RawStmt, C)
parseConstant = do
  c1 <- P.keyword' "constant"
  m <- P.getCurrentHint
  (constName, c2) <- P.baseName'
  constName' <- lift $ Locator.attachCurrentLocator constName
  t <- parseDefInfoCod m
  (c3, (v, c4)) <- P.betweenBrace' rawExpr
  return (RawStmtDefineConst c1 m (constName', c2) t (c3, v), c4)

parseDefineData :: P.Parser (RawStmt, C)
parseDefineData = do
  c1 <- P.keyword' "data"
  m <- P.getCurrentHint
  (dataName, c2) <- P.baseName'
  dataName' <- lift $ Locator.attachCurrentLocator dataName
  dataArgsOrNone <- parseDataArgs
  (c3, (consInfoList, c)) <- P.betweenBrace' $ P.manyList parseDefineDataClause
  return (RawStmtDefineData c1 m (dataName', c2) dataArgsOrNone c3 consInfoList, c)

parseDataArgs :: P.Parser (Maybe RD.ExpArgs)
parseDataArgs = do
  choice
    [ do
        args <- try (P.argSeqOrList preBinder)
        return $ Just args,
      return Nothing
    ]

parseDefineDataClause :: P.Parser (Hint, BN.BaseName, IsConstLike, RD.ExpArgs)
parseDefineDataClause = do
  m <- P.getCurrentHint
  consName <- P.baseName
  unless (isConsName (BN.reify consName)) $ do
    lift $ Throw.raiseError m "the name of a constructor must be capitalized"
  consArgsOrNone <- parseConsArgs
  let consArgs = fromMaybe (Nothing, ([], [])) consArgsOrNone
  let isConstLike = isNothing consArgsOrNone
  return (m, consName, isConstLike, consArgs)

parseConsArgs :: P.Parser (Maybe RD.ExpArgs)
parseConsArgs = do
  choice
    [ do
        args <- P.argSeqOrList parseDefineDataClauseArg
        return $ Just args,
      return Nothing
    ]

parseDefineDataClauseArg :: P.Parser (RawBinder (RT.RawTerm, C))
parseDefineDataClauseArg = do
  choice
    [ try preAscription,
      typeWithoutIdent
    ]

parseDefineResource :: P.Parser (RawStmt, C)
parseDefineResource = do
  c1 <- P.keyword' "resource"
  m <- P.getCurrentHint
  (name, c2) <- P.baseName'
  name' <- lift $ Locator.attachCurrentLocator name
  (c3, ((discarder, copier), c4)) <- P.betweenBrace' $ do
    cDiscarder <- P.delimiter' "-"
    discarder <- rawExpr
    cCopier <- P.delimiter' "-"
    copier <- rawExpr
    return ((cDiscarder, discarder), (cCopier, copier))
  return (RawStmtDefineResource c1 m (name', c2) c3 discarder copier, c4)

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
