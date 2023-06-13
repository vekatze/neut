module Scene.Parse
  ( parse,
    parseCachedStmtList,
  )
where

import Context.Alias qualified as Alias
import Context.App
import Context.Cache qualified as Cache
import Context.Env qualified as Env
import Context.Global qualified as Global
import Context.Locator qualified as Locator
import Context.Throw qualified as Throw
import Context.UnusedVariable qualified as UnusedVariable
import Control.Comonad.Cofree hiding (section)
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import Data.Text qualified as T
import Entity.ArgNum qualified as AN
import Entity.BaseName qualified as BN
import Entity.Cache qualified as Cache
import Entity.Decl qualified as DE
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.ExternalName qualified as EN
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.Ident.Reify
import Entity.IsConstLike
import Entity.Name
import Entity.NameArrow qualified as NA
import Entity.Opacity qualified as O
import Entity.RawBinder
import Entity.RawTerm qualified as RT
import Entity.Source qualified as Source
import Entity.Stmt
import Entity.StmtKind qualified as SK
import Path
import Scene.Parse.Core qualified as P
import Scene.Parse.Discern qualified as Discern
import Scene.Parse.Export qualified as Parse
import Scene.Parse.Import qualified as Parse
import Scene.Parse.RawTerm
import Text.Megaparsec hiding (parse)

parse :: App (Either Cache.Cache ([WeakStmt], [NA.NameArrow]))
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

parseSource :: Source.Source -> App (Either Cache.Cache ([WeakStmt], [NA.NameArrow]))
parseSource source = do
  mCache <- Cache.loadCache source
  let path = Source.sourceFilePath source
  case mCache of
    Just cache -> do
      let stmtList = Cache.stmtList cache
      parseCachedStmtList stmtList
      Global.saveCurrentNameSet path $ Cache.nameArrowList cache
      return $ Left cache
    Nothing -> do
      (defList, nameArrowList, declList) <- P.run (program source) $ Source.sourceFilePath source
      registerTopLevelNames defList
      stmtList <- Discern.discernStmtList defList
      nameArrowList' <- concat <$> mapM Discern.discernNameArrow nameArrowList
      Global.saveCurrentNameSet path nameArrowList'
      UnusedVariable.registerRemarks
      return $ Right (stmtList, nameArrowList')

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

program :: Source.Source -> P.Parser ([RawStmt], [NA.RawNameArrow], [DE.Decl])
program currentSource = do
  m <- P.getCurrentHint
  sourceInfoList <- Parse.parseImportBlock currentSource
  nameArrowList <- Parse.parseExportBlock
  declList <- parseDeclareList
  forM_ sourceInfoList $ \(source, aliasInfo) -> do
    lift $ Global.activateTopLevelNamesInSource m source
    lift $ Alias.activateAliasInfo aliasInfo
  defList <- concat <$> many parseStmt <* eof
  return (defList, nameArrowList, declList)

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
  ((_, name), impArgs, expArgs, codType, e) <- parseTopDefInfo
  name' <- lift $ Locator.attachCurrentLocator name
  lift $ defineFunction (SK.Normal opacity) m name' (AN.fromInt $ length impArgs) (impArgs ++ expArgs) codType e

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
  [(Hint, BN.BaseName, IsConstLike, [RawBinder RT.RawTerm])] ->
  App [RawStmt]
defineData m dataName dataArgsOrNone consInfoList = do
  let dataArgs = fromMaybe [] dataArgsOrNone
  consInfoList' <- mapM modifyConstructorName consInfoList
  let consInfoList'' = modifyConsInfo D.zero consInfoList'
  let stmtKind = SK.Data dataName dataArgs consInfoList''
  let consNameList = map (\(_, consName, _, _, _) -> consName) consInfoList''
  let dataType = constructDataType m dataName consNameList dataArgs
  let isConstLike = isNothing dataArgsOrNone
  let formRule = RawStmtDefine isConstLike stmtKind m dataName (AN.fromInt 0) dataArgs (m :< RT.Tau) dataType
  introRuleList <- parseDefineDataConstructor dataType dataName dataArgs consInfoList' D.zero
  return $ formRule : introRuleList

modifyConsInfo ::
  D.Discriminant ->
  [(Hint, DD.DefiniteDescription, b, [RawBinder RT.RawTerm])] ->
  [(Hint, DD.DefiniteDescription, b, [RawBinder RT.RawTerm], D.Discriminant)]
modifyConsInfo d consInfoList =
  case consInfoList of
    [] ->
      []
    (m, consName, isConstLike, consArgs) : rest ->
      (m, consName, isConstLike, consArgs, d) : modifyConsInfo (D.increment d) rest

modifyConstructorName ::
  (Hint, BN.BaseName, IsConstLike, [RawBinder RT.RawTerm]) ->
  App (Hint, DD.DefiniteDescription, IsConstLike, [RawBinder RT.RawTerm])
modifyConstructorName (mb, consName, isConstLike, yts) = do
  consName' <- Locator.attachCurrentLocator consName
  return (mb, consName', isConstLike, yts)

parseDefineDataConstructor ::
  RT.RawTerm ->
  DD.DefiniteDescription ->
  [RawBinder RT.RawTerm] ->
  [(Hint, DD.DefiniteDescription, IsConstLike, [RawBinder RT.RawTerm])] ->
  D.Discriminant ->
  App [RawStmt]
parseDefineDataConstructor dataType dataName dataArgs consInfoList discriminant = do
  case consInfoList of
    [] ->
      return []
    (m, consName, isConstLike, consArgs) : rest -> do
      let dataArgs' = map identPlusToVar dataArgs
      let consArgs' = map identPlusToVar consArgs
      let consNameList = map (\(_, c, _, _) -> c) consInfoList
      let args = dataArgs ++ consArgs
      let introRule =
            RawStmtDefine
              isConstLike
              (SK.DataIntro consName dataArgs consArgs discriminant)
              m
              consName
              (AN.fromInt $ length dataArgs)
              args
              dataType
              $ m :< RT.DataIntro dataName consName consNameList discriminant dataArgs' consArgs'
      introRuleList <- parseDefineDataConstructor dataType dataName dataArgs rest (D.increment discriminant)
      return $ introRule : introRuleList

constructDataType ::
  Hint ->
  DD.DefiniteDescription ->
  [DD.DefiniteDescription] ->
  [RawBinder RT.RawTerm] ->
  RT.RawTerm
constructDataType m dataName consNameList dataArgs = do
  m :< RT.Data dataName consNameList (map identPlusToVar dataArgs)

parseDefineDataClause :: P.Parser (Hint, BN.BaseName, IsConstLike, [RawBinder RT.RawTerm])
parseDefineDataClause = do
  m <- P.getCurrentHint
  consName <- P.baseNameCapitalized
  consArgsOrNone <- parseConsArgs
  let consArgs = fromMaybe [] consArgsOrNone
  let isConstLike = isNothing consArgsOrNone
  return (m, consName, isConstLike, consArgs)

parseConsArgs :: P.Parser (Maybe [RawBinder RT.RawTerm])
parseConsArgs = do
  choice
    [ Just <$> try (P.argSeqOrList parseDefineDataClauseArg),
      return Nothing
    ]

parseDefineDataClauseArg :: P.Parser (RawBinder RT.RawTerm)
parseDefineDataClauseArg = do
  choice
    [ try preAscription,
      typeWithoutIdent
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
