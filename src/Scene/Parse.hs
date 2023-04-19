module Scene.Parse
  ( parse,
    parseCachedStmtList,
  )
where

import Context.Alias qualified as Alias
import Context.App
import Context.Cache qualified as Cache
import Context.CodataDefinition qualified as CodataDefinition
import Context.Env qualified as Env
import Context.Gensym qualified as Gensym
import Context.Global qualified as Global
import Context.Locator qualified as Locator
import Context.Throw (liftEither)
import Context.Throw qualified as Throw
import Context.UnusedVariable qualified as UnusedVariable
import Control.Comonad.Cofree hiding (section)
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import Data.Text qualified as T
import Data.Vector qualified as V
import Entity.ArgNum qualified as AN
import Entity.Arity qualified as A
import Entity.BaseName qualified as BN
import Entity.Binder
import Entity.Cache qualified as Cache
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.Ident qualified as Ident
import Entity.Ident.Reflect qualified as Ident
import Entity.Ident.Reify qualified as Ident
import Entity.IsConstLike
import Entity.Opacity qualified as O
import Entity.RawPattern qualified as RP
import Entity.RawTerm qualified as RT
import Entity.Source qualified as Source
import Entity.Stmt
import Entity.Term.Weaken qualified as TM
import Path
import Scene.Parse.Core qualified as P
import Scene.Parse.Discern qualified as Discern
import Scene.Parse.Export qualified as Parse
import Scene.Parse.Import qualified as Parse
import Scene.Parse.RawTerm
import Text.Megaparsec hiding (parse)

--
-- core functions
--

parse :: App (Either Cache.Cache [WeakStmt])
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

saveCurrentNameSet :: Hint -> Path Abs File -> [DD.DefiniteDescription] -> App ()
saveCurrentNameSet m currentPath nameList = do
  nameList' <- forM nameList $ \name -> do
    globalName <- Global.lookupStrict m name
    return (name, globalName)
  Global.insertToSourceNameMap currentPath nameList'

parseSource :: Source.Source -> App (Either Cache.Cache [WeakStmt])
parseSource source = do
  hasCacheSet <- Env.getHasCacheSet
  mCache <- Cache.loadCache source hasCacheSet
  let path = Source.sourceFilePath source
  let m = Entity.Hint.new 1 1 $ toFilePath $ Source.sourceFilePath source
  case mCache of
    Just cache -> do
      let stmtList = Cache.stmtList cache
      parseCachedStmtList stmtList
      saveCurrentNameSet m path $ map (getNameFromWeakStmt . TM.weakenStmt) stmtList
      return $ Left cache
    Nothing -> do
      defList <- P.run (program source) $ Source.sourceFilePath source
      registerTopLevelNames defList
      stmtList <- Discern.discernStmtList defList
      saveCurrentNameSet m path $ map getNameFromWeakStmt stmtList
      UnusedVariable.registerRemarks
      return $ Right stmtList

parseCachedStmtList :: [Stmt] -> App ()
parseCachedStmtList stmtList = do
  forM_ stmtList $ \stmt -> do
    case stmt of
      StmtDefine isConstLike stmtKind m name impArgNum args _ _ -> do
        Global.registerStmtDefine isConstLike m stmtKind name impArgNum $ AN.fromInt (length args)
      StmtDefineResource m name _ _ ->
        Global.registerStmtDefineResource m name
      StmtExport m alias dd gn ->
        Global.registerStmtExport m alias dd gn

ensureMain :: Hint -> DD.DefiniteDescription -> App ()
ensureMain m mainFunctionName = do
  mMain <- Global.lookup m mainFunctionName
  case mMain of
    Just (GN.TopLevelFunc _ _) ->
      return ()
    _ ->
      Throw.raiseError m "`main` is missing"

program :: Source.Source -> P.Parser [RawStmt]
program currentSource = do
  m <- P.getCurrentHint
  sourceInfoList <- Parse.parseImportBlock currentSource
  forM_ sourceInfoList $ \(source, aliasInfo) -> do
    lift $ Global.activateTopLevelNamesInSource m source
    lift $ Alias.activateAliasInfo aliasInfo
  defList <- concat <$> many parseStmt
  exportList <- choice [parseExport, return []] <* eof
  return $ defList ++ exportList

parseStmt :: P.Parser [RawStmt]
parseStmt = do
  choice
    [ parseDefineVariant,
      parseDefineStruct,
      return <$> parseAliasOpaque,
      return <$> parseAliasTransparent,
      return <$> parseDefineResource,
      return <$> parseDefine O.Transparent,
      return <$> parseDefine O.Opaque
    ]

--
-- parser for statements
--

-- define name (x1 : A1) ... (xn : An) : A = e
parseDefine :: O.Opacity -> P.Parser RawStmt
parseDefine opacity = do
  try $
    case opacity of
      O.Opaque ->
        P.keyword "define"
      O.Transparent ->
        P.keyword "define-inline"
  m <- P.getCurrentHint
  ((_, name), impArgs, expArgs, codType, e) <- parseTopDefInfo
  name' <- lift $ Locator.attachCurrentLocator name
  lift $ defineFunction (Normal opacity) m name' (AN.fromInt $ length impArgs) (impArgs ++ expArgs) codType e

defineFunction ::
  StmtKindF RT.RawTerm ->
  Hint ->
  DD.DefiniteDescription ->
  AN.ArgNum ->
  [BinderF RT.RawTerm] ->
  RT.RawTerm ->
  RT.RawTerm ->
  App RawStmt
defineFunction stmtKind m name impArgNum binder codType e = do
  return $ RawStmtDefine False stmtKind m name impArgNum binder codType e

parseDefineVariant :: P.Parser [RawStmt]
parseDefineVariant = do
  m <- P.getCurrentHint
  try $ P.keyword "variant"
  a <- P.baseName >>= lift . Locator.attachCurrentLocator
  dataArgsOrNone <- parseDataArgs
  consInfoList <- P.betweenBrace $ P.manyList parseDefineVariantClause
  lift $ defineData m a dataArgsOrNone consInfoList

parseDataArgs :: P.Parser (Maybe [BinderF RT.RawTerm])
parseDataArgs = do
  choice
    [ Just <$> P.argList preBinder,
      return Nothing
    ]

defineData ::
  Hint ->
  DD.DefiniteDescription ->
  Maybe [BinderF RT.RawTerm] ->
  [(Hint, BN.BaseName, IsConstLike, [BinderF RT.RawTerm])] ->
  App [RawStmt]
defineData m dataName dataArgsOrNone consInfoList = do
  let dataArgs = fromMaybe [] dataArgsOrNone
  consInfoList' <- mapM modifyConstructorName consInfoList
  let consInfoList'' = modifyConsInfo D.zero consInfoList'
  let stmtKind = Data dataName dataArgs consInfoList''
  let consNameList = map (\(consName, _, _, _) -> consName) consInfoList''
  let dataType = constructDataType m dataName consNameList dataArgs
  let isConstLike = isNothing dataArgsOrNone
  let formRule = RawStmtDefine isConstLike stmtKind m dataName (AN.fromInt 0) dataArgs (m :< RT.Tau) dataType
  -- let formRule = RawStmtDefine stmtKind m dataName dataArgs (m :< RT.Tau) dataType
  introRuleList <- parseDefineVariantConstructor dataType dataName dataArgs consInfoList' D.zero
  return $ formRule : introRuleList

modifyConsInfo ::
  D.Discriminant ->
  [(a, DD.DefiniteDescription, b, [BinderF RT.RawTerm])] ->
  [(DD.DefiniteDescription, b, [BinderF RT.RawTerm], D.Discriminant)]
modifyConsInfo d consInfoList =
  case consInfoList of
    [] ->
      []
    (_, consName, isConstLike, consArgs) : rest ->
      (consName, isConstLike, consArgs, d) : modifyConsInfo (D.increment d) rest

modifyConstructorName ::
  (Hint, BN.BaseName, IsConstLike, [BinderF RT.RawTerm]) ->
  App (Hint, DD.DefiniteDescription, IsConstLike, [BinderF RT.RawTerm])
modifyConstructorName (mb, consName, isConstLike, yts) = do
  consName' <- Locator.attachCurrentLocator consName
  return (mb, consName', isConstLike, yts)

parseDefineVariantConstructor ::
  RT.RawTerm ->
  DD.DefiniteDescription ->
  [BinderF RT.RawTerm] ->
  [(Hint, DD.DefiniteDescription, IsConstLike, [BinderF RT.RawTerm])] ->
  D.Discriminant ->
  App [RawStmt]
parseDefineVariantConstructor dataType dataName dataArgs consInfoList discriminant = do
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
              (DataIntro consName dataArgs consArgs discriminant)
              m
              consName
              (AN.fromInt $ length dataArgs)
              args
              dataType
              $ m :< RT.DataIntro dataName consName consNameList discriminant dataArgs' consArgs'
      introRuleList <- parseDefineVariantConstructor dataType dataName dataArgs rest (D.increment discriminant)
      return $ introRule : introRuleList

constructDataType ::
  Hint ->
  DD.DefiniteDescription ->
  [DD.DefiniteDescription] ->
  [BinderF RT.RawTerm] ->
  RT.RawTerm
constructDataType m dataName consNameList dataArgs = do
  m :< RT.Data dataName consNameList (map identPlusToVar dataArgs)

parseDefineVariantClause :: P.Parser (Hint, BN.BaseName, IsConstLike, [BinderF RT.RawTerm])
parseDefineVariantClause = do
  m <- P.getCurrentHint
  consName <- P.baseNameCapitalized
  consArgsOrNone <- parseConsArgs
  let consArgs = fromMaybe [] consArgsOrNone
  let isConstLike = isNothing consArgsOrNone
  return (m, consName, isConstLike, consArgs)

parseConsArgs :: P.Parser (Maybe [BinderF RT.RawTerm])
parseConsArgs = do
  choice
    [ Just <$> P.argList parseDefineVariantClauseArg,
      return Nothing
    ]

parseDefineVariantClauseArg :: P.Parser (BinderF RT.RawTerm)
parseDefineVariantClauseArg = do
  choice
    [ try preAscription,
      typeWithoutIdent
    ]

parseDefineStruct :: P.Parser [RawStmt]
parseDefineStruct = do
  m <- P.getCurrentHint
  try $ P.keyword "struct"
  dataName <- P.baseName >>= lift . Locator.attachCurrentLocator
  dataArgsOrNone <- parseDataArgs
  P.keyword "by"
  consName <- P.baseNameCapitalized
  elemInfoList <- P.betweenBrace $ P.manyList preAscription
  formRule <- lift $ defineData m dataName dataArgsOrNone [(m, consName, False, elemInfoList)]
  let dataArgs = fromMaybe [] dataArgsOrNone
  consName' <- lift $ Locator.attachCurrentLocator consName
  elimRuleList <- mapM (lift . parseDefineStructElim dataName dataArgs consName' elemInfoList) elemInfoList
  -- register codata info for `new-with-end`
  let numOfDataArgs = A.fromInt $ length dataArgs
  let numOfFields = A.fromInt $ length elemInfoList
  let (_, consInfoList, _) = unzip3 elemInfoList
  consInfoList' <- lift $ mapM (liftEither . BN.reflect m . Ident.toText) consInfoList
  consNameList <- lift $ mapM Locator.attachCurrentLocator consInfoList'
  lift $ CodataDefinition.insert dataName (consName', numOfDataArgs, numOfFields) consNameList
  -- ... then return
  return $ formRule ++ elimRuleList

-- noetic projection
parseDefineStructElim ::
  DD.DefiniteDescription ->
  [BinderF RT.RawTerm] ->
  DD.DefiniteDescription ->
  [BinderF RT.RawTerm] ->
  BinderF RT.RawTerm ->
  App RawStmt
parseDefineStructElim dataName dataArgs consName elemInfoList (m, elemName, elemType) = do
  let structType = m :< RT.Noema (constructDataType m dataName [consName] dataArgs)
  structVarText <- Gensym.newText
  let projArgs = dataArgs ++ [(m, Ident.fromText structVarText, structType)]
  elemName' <- Throw.liftEither $ BN.reflect m $ Ident.toText elemName
  projectionName <- Locator.attachCurrentLocator elemName'
  let argList = flip map elemInfoList $ \(mx, x, _) -> (mx, RP.Var (Ident.attachHolePrefix x))
  defineFunction
    (Normal O.Opaque)
    m
    projectionName -- e.g. some-lib.foo::my-struct.element-x
    (AN.fromInt $ length dataArgs)
    projArgs
    (m :< RT.Noema elemType)
    $ m
      :< RT.DataElim
        True
        [preVar m structVarText]
        ( RP.new
            [ ( V.fromList [(m, RP.Cons (RP.DefiniteDescription consName) argList)],
                preVar' m (Ident.attachHolePrefix elemName)
              )
            ]
        )

parseExport :: P.Parser [RawStmt]
parseExport = do
  exportInfo <- Parse.parseExportBlock
  forM exportInfo $ \(m, alias, varOrDD) -> do
    return $ RawStmtExport m alias varOrDD

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
    let stmtKind = Normal opacity
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

identPlusToVar :: BinderF RT.RawTerm -> RT.RawTerm
identPlusToVar (m, x, _) =
  m :< RT.Var x

registerTopLevelNames :: [RawStmt] -> App ()
registerTopLevelNames stmtList =
  case stmtList of
    [] ->
      return ()
    RawStmtDefine isConstLike stmtKind m functionName impArgNum xts _ _ : rest -> do
      Global.registerStmtDefine isConstLike m stmtKind functionName impArgNum $ AN.fromInt (length xts)
      registerTopLevelNames rest
    RawStmtDefineResource m name _ _ : rest -> do
      Global.registerStmtDefineResource m name
      registerTopLevelNames rest
    RawStmtExport {} : rest -> do
      registerTopLevelNames rest
