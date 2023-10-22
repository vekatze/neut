module Scene.Parse
  ( parse,
    parseCachedStmtList,
  )
where

import Context.Alias qualified as Alias
import Context.App
import Context.Cache qualified as Cache
import Context.Decl qualified as Decl
import Context.Env qualified as Env
import Context.Global qualified as Global
import Context.Locator qualified as Locator
import Context.NameDependence qualified as NameDependence
import Context.Throw qualified as Throw
import Context.UnusedVariable qualified as UnusedVariable
import Context.Via qualified as Via
import Control.Comonad.Cofree hiding (section)
import Control.Monad
import Control.Monad.Trans
import Data.HashMap.Strict qualified as Map
import Data.Maybe
import Data.Text qualified as T
import Entity.ArgNum qualified as AN
import Entity.Attr.Data qualified as AttrD
import Entity.Attr.DataIntro qualified as AttrDI
import Entity.BaseName qualified as BN
import Entity.Cache qualified as Cache
import Entity.Decl qualified as DE
import Entity.DeclarationName qualified as DN
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.ExternalName qualified as EN
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.Ident.Reify
import Entity.IsConstLike
import Entity.Name
import Entity.Opacity qualified as O
import Entity.RawBinder
import Entity.RawIdent
import Entity.RawTerm qualified as RT
import Entity.Source qualified as Source
import Entity.Stmt
import Entity.StmtKind qualified as SK
import Entity.ViaMap qualified as VM
import Path
import Scene.Parse.Core qualified as P
import Scene.Parse.Discern qualified as Discern
import Scene.Parse.Import qualified as Parse
import Scene.Parse.RawTerm
import Text.Megaparsec hiding (parse)

parse :: App (Either Cache.Cache ([WeakStmt], [DE.Decl]))
parse = do
  source <- Env.getCurrentSource
  result <- parseSource source
  mMainDD <- Locator.getMainDefiniteDescription source
  case mMainDD of
    Just mainDD -> do
      let m = Entity.Hint.new 1 1 $ toFilePath $ Source.sourceFilePath source
      ensureMain m mainDD
      return result
    Nothing ->
      return result

parseSource :: Source.Source -> App (Either Cache.Cache ([WeakStmt], [DE.Decl]))
parseSource source = do
  mCache <- Cache.loadCache source
  let path = Source.sourceFilePath source
  case mCache of
    Just cache -> do
      let stmtList = Cache.stmtList cache
      parseCachedStmtList stmtList
      saveTopLevelNames path $ map getStmtName stmtList
      NameDependence.add path $ Map.fromList $ Cache.nameDependence cache
      Via.union path $ VM.decode $ Cache.viaInfo cache
      return $ Left cache
    Nothing -> do
      (defList, declList) <- P.run (program source) $ Source.sourceFilePath source
      registerTopLevelNames defList
      stmtList <- Discern.discernStmtList defList
      saveTopLevelNames path $ map getWeakStmtName stmtList
      UnusedVariable.registerRemarks
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
      StmtDefine isConstLike stmtKind m name impArgNum args _ _ -> do
        let explicitArgs = drop (AN.reify impArgNum) args
        let argNames = map (\(_, x, _) -> toText x) explicitArgs
        Global.registerStmtDefine isConstLike m stmtKind name impArgNum argNames
      StmtDefineResource m name _ _ ->
        Global.registerStmtDefineResource m name

ensureMain :: Hint -> DD.DefiniteDescription -> App ()
ensureMain m mainFunctionName = do
  mMain <- Global.lookup m mainFunctionName
  case mMain of
    Just (_, GN.TopLevelFunc _ _) ->
      return ()
    _ ->
      Throw.raiseError m "`main` is missing"

program :: Source.Source -> P.Parser ([RawStmt], [DE.Decl])
program currentSource = do
  m <- P.getCurrentHint
  sourceInfoList <- Parse.parseImportBlock currentSource
  declList <- parseDeclareList
  forM_ sourceInfoList $ \(source, aliasInfoList) -> do
    let path = Source.sourceFilePath source
    namesInSource <- lift $ Global.lookupSourceNameMap m path
    lift $ Global.activateTopLevelNames namesInSource
    forM_ aliasInfoList $ \aliasInfo ->
      lift $ Alias.activateAliasInfo namesInSource aliasInfo
    lift $ NameDependence.get path >>= Global.activateTopLevelNames
    lift $ Via.get path >>= Via.addToActiveViaMap
  forM_ declList $ \(DE.Decl name domList cod) -> do
    lift $ Decl.insDeclEnv' (DN.Ext name) domList cod
  defList <- concat <$> many parseStmt <* eof
  return (defList, declList)

parseStmt :: P.Parser [RawStmt]
parseStmt = do
  choice
    [ parseDefineData,
      return <$> parseAliasOpaque,
      return <$> parseAliasTransparent,
      return <$> parseDefineResource,
      return <$> parseDefine O.Transparent,
      return <$> parseDefine O.Opaque
    ]

parseDeclareList :: P.Parser [DE.Decl]
parseDeclareList = do
  choice
    [ do
        P.keyword "declare"
        P.betweenBrace (P.manyList parseDeclare),
      return []
    ]

parseDeclare :: P.Parser DE.Decl
parseDeclare = do
  declName <- EN.ExternalName <$> P.symbol
  lts <- P.betweenParen $ P.commaList lowType
  cod <- P.delimiter ":" >> lowType
  return $ DE.Decl declName lts cod

parseDefine :: O.Opacity -> P.Parser RawStmt
parseDefine opacity = do
  try $
    case opacity of
      O.Opaque ->
        P.keyword "define"
      O.Transparent ->
        P.keyword "inline"
  m <- P.getCurrentHint
  ((_, name), expArgs, codType, e) <- parseTopDefInfo
  name' <- lift $ Locator.attachCurrentLocator name
  lift $ defineFunction (SK.Normal opacity) m name' (AN.fromInt 0) expArgs codType e

defineFunction ::
  SK.RawStmtKind ->
  Hint ->
  DD.DefiniteDescription ->
  AN.ArgNum ->
  [RawBinder RT.RawTerm] ->
  RT.RawTerm ->
  RT.RawTerm ->
  App RawStmt
defineFunction stmtKind m name impArgNum binder codType e = do
  return $ RawStmtDefine False stmtKind m name impArgNum binder codType e

parseDefineData :: P.Parser [RawStmt]
parseDefineData = do
  m <- P.getCurrentHint
  try $ P.keyword "data"
  a <- P.baseName >>= lift . Locator.attachCurrentLocator
  dataArgsOrNone <- parseDataArgs
  consInfoList <- P.betweenBrace $ P.manyList parseDefineDataClause
  lift $ defineData m a dataArgsOrNone consInfoList

parseDataArgs :: P.Parser (Maybe [RawBinder RT.RawTerm])
parseDataArgs = do
  choice
    [ Just <$> try (P.argSeqOrList preBinder),
      return Nothing
    ]

defineData ::
  Hint ->
  DD.DefiniteDescription ->
  Maybe [RawBinder RT.RawTerm] ->
  [(Hint, BN.BaseName, IsConstLike, [(RawBinder RT.RawTerm, Maybe Name)])] ->
  App [RawStmt]
defineData m dataName dataArgsOrNone consInfoList = do
  let dataArgs = fromMaybe [] dataArgsOrNone
  consInfoList' <- mapM modifyConstructorName consInfoList
  let consInfoList'' = modifyConsInfo D.zero consInfoList'
  let stmtKind = SK.Data dataName dataArgs consInfoList''
  let consNameList = map (\(_, consName, _, _, _) -> consName) consInfoList''
  let isConstLike = isNothing dataArgsOrNone
  let dataType = constructDataType m dataName isConstLike consNameList dataArgs
  let formRule = RawStmtDefine isConstLike stmtKind m dataName (AN.fromInt 0) dataArgs (m :< RT.Tau) dataType
  introRuleList <- parseDefineDataConstructor dataType dataName dataArgs consInfoList' D.zero
  return $ formRule : introRuleList

modifyConsInfo ::
  D.Discriminant ->
  [(Hint, DD.DefiniteDescription, b, [(RawBinder RT.RawTerm, Maybe Name)])] ->
  [(Hint, DD.DefiniteDescription, b, [RawBinder RT.RawTerm], D.Discriminant)]
modifyConsInfo d consInfoList =
  case consInfoList of
    [] ->
      []
    (m, consName, isConstLike, consArgs) : rest ->
      (m, consName, isConstLike, map fst consArgs, d) : modifyConsInfo (D.increment d) rest

modifyConstructorName ::
  (Hint, BN.BaseName, IsConstLike, [(RawBinder RT.RawTerm, Maybe Name)]) ->
  App (Hint, DD.DefiniteDescription, IsConstLike, [(RawBinder RT.RawTerm, Maybe Name)])
modifyConstructorName (mb, consName, isConstLike, yts) = do
  consName' <- Locator.attachCurrentLocator consName
  return (mb, consName', isConstLike, yts)

parseDefineDataConstructor ::
  RT.RawTerm ->
  DD.DefiniteDescription ->
  [RawBinder RT.RawTerm] ->
  [(Hint, DD.DefiniteDescription, IsConstLike, [(RawBinder RT.RawTerm, Maybe Name)])] ->
  D.Discriminant ->
  App [RawStmt]
parseDefineDataConstructor dataType dataName dataArgs consInfoList discriminant = do
  case consInfoList of
    [] ->
      return []
    (m, consName, isConstLike, consArgs) : rest -> do
      let dataArgs' = map identPlusToVar dataArgs
      let consArgs' = map adjustConsArg consArgs
      let consNameList = map (\(_, c, _, _) -> c) consInfoList
      let args = dataArgs ++ map fst consArgs
      let introRule =
            RawStmtDefine
              isConstLike
              (SK.DataIntro consName dataArgs (map fst consArgs) discriminant)
              m
              consName
              (AN.fromInt $ length dataArgs)
              args
              dataType
              $ m :< RT.DataIntro (AttrDI.Attr {..}) consName dataArgs' (map fst consArgs')
      let viaRule = RawStmtVia m consName (map snd consArgs')
      introRuleList <- parseDefineDataConstructor dataType dataName dataArgs rest (D.increment discriminant)
      return $ introRule : viaRule : introRuleList

constructDataType ::
  Hint ->
  DD.DefiniteDescription ->
  IsConstLike ->
  [DD.DefiniteDescription] ->
  [RawBinder RT.RawTerm] ->
  RT.RawTerm
constructDataType m dataName isConstLike consNameList dataArgs = do
  m :< RT.Data (AttrD.Attr {..}) dataName (map identPlusToVar dataArgs)

parseDefineDataClause :: P.Parser (Hint, BN.BaseName, IsConstLike, [(RawBinder RT.RawTerm, Maybe Name)])
parseDefineDataClause = do
  m <- P.getCurrentHint
  consName <- P.baseNameCapitalized
  consArgsOrNone <- parseConsArgs
  let consArgs = fromMaybe [] consArgsOrNone
  let isConstLike = isNothing consArgsOrNone
  return (m, consName, isConstLike, consArgs)

parseConsArgs :: P.Parser (Maybe [(RawBinder RT.RawTerm, Maybe Name)])
parseConsArgs = do
  choice
    [ Just <$> try (P.argSeqOrList parseDefineDataClauseArg),
      return Nothing
    ]

parseDefineDataClauseArg :: P.Parser (RawBinder RT.RawTerm, Maybe Name)
parseDefineDataClauseArg = do
  consArg <-
    choice
      [ try preAscription,
        typeWithoutIdent
      ]
  choice
    [ do
        P.keyword "via"
        (_, name) <- parseName
        return (consArg, Just name),
      return (consArg, Nothing)
    ]

parseAliasTransparent :: P.Parser RawStmt
parseAliasTransparent = do
  parseType "alias" O.Transparent

parseAliasOpaque :: P.Parser RawStmt
parseAliasOpaque = do
  parseType "alias-opaque" O.Opaque

parseType :: T.Text -> O.Opacity -> P.Parser RawStmt
parseType keywordText opacity = do
  m <- P.getCurrentHint
  try $ P.keyword keywordText
  aliasName <- P.baseName
  aliasName' <- lift $ Locator.attachCurrentLocator aliasName
  P.betweenBrace $ do
    t <- rawExpr
    let stmtKind = SK.Normal opacity
    return $ RawStmtDefine True stmtKind m aliasName' AN.zero [] (m :< RT.Tau) t

parseDefineResource :: P.Parser RawStmt
parseDefineResource = do
  try $ P.keyword "resource"
  m <- P.getCurrentHint
  name <- P.baseName
  name' <- lift $ Locator.attachCurrentLocator name
  P.betweenBrace $ do
    discarder <- P.delimiter "-" >> rawExpr
    copier <- P.delimiter "-" >> rawExpr
    return $ RawStmtDefineResource m name' discarder copier

identPlusToVar :: RawBinder RT.RawTerm -> RT.RawTerm
identPlusToVar (m, x, _) =
  m :< RT.Var (Var x)

adjustConsArg :: (RawBinder RT.RawTerm, Maybe Name) -> (RT.RawTerm, (RawIdent, Maybe Name))
adjustConsArg ((m, x, _), mName) =
  (m :< RT.Var (Var x), (x, mName))

registerTopLevelNames :: [RawStmt] -> App ()
registerTopLevelNames stmtList =
  case stmtList of
    [] ->
      return ()
    RawStmtDefine isConstLike stmtKind m functionName impArgNum xts _ _ : rest -> do
      let explicitArgs = drop (AN.reify impArgNum) xts
      let argNames = map (\(_, x, _) -> x) explicitArgs
      Global.registerStmtDefine isConstLike m stmtKind functionName impArgNum argNames
      registerTopLevelNames rest
    RawStmtDefineResource m name _ _ : rest -> do
      Global.registerStmtDefineResource m name
      registerTopLevelNames rest
    RawStmtVia {} : rest ->
      registerTopLevelNames rest

getWeakStmtName :: WeakStmt -> (Hint, DD.DefiniteDescription)
getWeakStmtName stmt =
  case stmt of
    WeakStmtDefine _ _ m name _ _ _ _ ->
      (m, name)
    WeakStmtDefineResource m name _ _ ->
      (m, name)

getStmtName :: Stmt -> (Hint, DD.DefiniteDescription)
getStmtName stmt =
  case stmt of
    StmtDefine _ _ m name _ _ _ _ ->
      (m, name)
    StmtDefineResource m name _ _ ->
      (m, name)
