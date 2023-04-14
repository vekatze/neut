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
import Context.Throw qualified as Throw
import Control.Comonad.Cofree hiding (section)
import Control.Monad
import Control.Monad.Trans
import Data.HashMap.Strict qualified as Map
import Data.Maybe
import Data.Text qualified as T
import Data.Vector qualified as V
import Entity.ArgNum qualified as AN
import Entity.Arity qualified as A
import Entity.BaseName qualified as BN
import Entity.Binder
import Entity.Const
import Entity.DefiniteDescription qualified as DD
import Entity.DefiniteLocator qualified as DL
import Entity.Discriminant qualified as D
import Entity.GlobalLocator qualified as GL
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.Ident.Reflect qualified as Ident
import Entity.Ident.Reify qualified as Ident
import Entity.LocalLocator qualified as LL
import Entity.Opacity qualified as O
import Entity.RawPattern qualified as RP
import Entity.RawTerm qualified as RT
import Entity.Section qualified as Section
import Entity.Source qualified as Source
import Entity.Stmt
import Entity.StrictGlobalLocator qualified as SGL
import Path
import Scene.Parse.Core qualified as P
import Scene.Parse.Discern qualified as Discern
import Scene.Parse.Import qualified as Parse
import Scene.Parse.RawTerm
import Text.Megaparsec hiding (parse)

--
-- core functions
--

parse :: App (Either [Stmt] [WeakStmt])
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

parseSource :: Source.Source -> App (Either [Stmt] [WeakStmt])
parseSource source = do
  hasCacheSet <- Env.getHasCacheSet
  mCache <- Cache.loadCache source hasCacheSet
  case mCache of
    Just cache -> do
      let stmtList = cacheStmtList cache
      parseCachedStmtList stmtList
      return $ Left stmtList
    Nothing -> do
      sourceAliasMap <- Env.getSourceAliasMap
      case Map.lookup (Source.sourceFilePath source) sourceAliasMap of
        Nothing ->
          Throw.raiseCritical' "[activateAliasInfoOfCurrentFile] (compiler bug)"
        Just aliasInfoList ->
          Alias.activateAliasInfo aliasInfoList
      defList <- P.run program $ Source.sourceFilePath source
      registerTopLevelNames defList
      Right <$> Discern.discernStmtList defList

parseCachedStmtList :: [Stmt] -> App ()
parseCachedStmtList stmtList = do
  forM_ stmtList $ \stmt -> do
    case stmt of
      StmtDefine isConstLike stmtKind m name impArgNum args _ _ -> do
        Global.registerStmtDefine isConstLike m stmtKind name impArgNum $ AN.fromInt (length args)
      StmtDefineResource m name _ _ ->
        Global.registerStmtDefineResource m name

ensureMain :: Hint -> DD.DefiniteDescription -> App ()
ensureMain m mainFunctionName = do
  mMain <- Global.lookup mainFunctionName
  case mMain of
    Just (GN.TopLevelFunc _ _) ->
      return ()
    _ ->
      Throw.raiseError m "`main` is missing"

program :: P.Parser [RawStmt]
program = do
  Parse.skipImportSequence
  parseStmtUseSequence
  concat <$> many parseStmt <* eof

parseStmtUseSequence :: P.Parser ()
parseStmtUseSequence = do
  let p1 = P.useBlock (P.manyList parseLocator)
  let p2 = return []
  locatorList <- choice [p1, p2]
  forM_ locatorList $ \loc -> do
    case loc of
      Left partialLocator ->
        lift $ Locator.activateDefiniteLocator partialLocator
      Right globalLocator ->
        lift $ Locator.activateGlobalLocator globalLocator

parseLocator :: P.Parser (Either DL.DefiniteLocator SGL.StrictGlobalLocator)
parseLocator = do
  choice
    [ Left <$> try parseDefiniteLocator,
      Right <$> parseGlobalLocator
    ]

parseStmt :: P.Parser [RawStmt]
parseStmt = do
  choice
    [ parseDefineVariant,
      parseDefineStruct,
      return <$> parseAlias,
      return <$> parseDefineResource,
      return <$> parseDefine O.Transparent,
      return <$> parseDefine O.Opaque,
      return <$> parseSection
    ]

parseDefiniteLocator :: P.Parser DL.DefiniteLocator
parseDefiniteLocator = do
  m <- P.getCurrentHint
  globalLocator <- P.symbol >>= lift . (Throw.liftEither . GL.reflect m >=> Alias.resolveAlias m)
  P.delimiter definiteSep
  localLocator <- P.symbol
  baseNameList <- lift $ Throw.liftEither $ BN.bySplit m localLocator
  return $ DL.new globalLocator $ map Section.Section baseNameList

parseGlobalLocator :: P.Parser SGL.StrictGlobalLocator
parseGlobalLocator = do
  m <- P.getCurrentHint
  gl <- P.symbol >>= lift . Throw.liftEither . GL.reflect m
  lift $ Alias.resolveAlias m gl

--
-- parser for statements
--

parseSection :: P.Parser RawStmt
parseSection = do
  try $ P.keyword "section"
  section <- Section.Section <$> P.baseName
  Locator.withLiftedSection section $ do
    stmtList <- concat <$> P.betweenBrace (many parseStmt)
    return $ RawStmtSection section stmtList

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
  [(Hint, T.Text, IsConstLike, [BinderF RT.RawTerm])] ->
  App [RawStmt]
defineData m dataName dataArgsOrNone consInfoList = do
  let dataArgs = fromMaybe [] dataArgsOrNone
  consInfoList' <- mapM (modifyConstructorName m dataName) consInfoList
  let consInfoList'' = modifyConsInfo D.zero consInfoList'
  let stmtKind = Data dataName dataArgs consInfoList''
  let dataType = constructDataType m dataName dataArgs
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
  Hint ->
  DD.DefiniteDescription ->
  (Hint, T.Text, IsConstLike, [BinderF RT.RawTerm]) ->
  App (Hint, DD.DefiniteDescription, IsConstLike, [BinderF RT.RawTerm])
modifyConstructorName m dataDD (mb, consName, isConstLike, yts) = do
  consName' <- Throw.liftEither $ DD.extend m dataDD consName
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
              $ m :< RT.DataIntro dataName consName discriminant dataArgs' consArgs'
      introRuleList <- parseDefineVariantConstructor dataType dataName dataArgs rest (D.increment discriminant)
      return $ introRule : introRuleList

constructDataType ::
  Hint ->
  DD.DefiniteDescription ->
  [BinderF RT.RawTerm] ->
  RT.RawTerm
constructDataType m dataName dataArgs = do
  m :< RT.Data dataName (map identPlusToVar dataArgs)

parseDefineVariantClause :: P.Parser (Hint, T.Text, IsConstLike, [BinderF RT.RawTerm])
parseDefineVariantClause = do
  m <- P.getCurrentHint
  consName <- P.symbolCapitalized
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
  m <- P.getCurrentHint
  choice
    [ try preAscription,
      weakTermToWeakIdent m rawTerm
    ]

parseDefineStruct :: P.Parser [RawStmt]
parseDefineStruct = do
  m <- P.getCurrentHint
  try $ P.keyword "struct"
  dataName <- P.baseName >>= lift . Locator.attachCurrentLocator
  dataArgsOrNone <- parseDataArgs
  elemInfoList <- P.betweenBrace $ P.manyList preAscription
  formRule <- lift $ defineData m dataName dataArgsOrNone [(m, "New", False, elemInfoList)]
  let dataArgs = fromMaybe [] dataArgsOrNone
  elimRuleList <- mapM (lift . parseDefineStructElim dataName dataArgs elemInfoList) elemInfoList
  -- register codata info for `new-with-end`
  dataNewName <- lift $ Throw.liftEither $ DD.extend m dataName "New"
  let numOfDataArgs = A.fromInt $ length dataArgs
  let numOfFields = A.fromInt $ length elemInfoList
  let (_, consInfoList, _) = unzip3 elemInfoList
  consNameList <- mapM (lift . Throw.liftEither . DD.extend m dataName . Ident.toText) consInfoList
  lift $ CodataDefinition.insert dataName (dataNewName, numOfDataArgs, numOfFields) consNameList
  -- ... then return
  return $ formRule ++ elimRuleList

-- noetic projection
parseDefineStructElim ::
  DD.DefiniteDescription ->
  [BinderF RT.RawTerm] ->
  [BinderF RT.RawTerm] ->
  BinderF RT.RawTerm ->
  App RawStmt
parseDefineStructElim dataName dataArgs elemInfoList (m, elemName, elemType) = do
  let structType = m :< RT.Noema (constructDataType m dataName dataArgs)
  structVarText <- Gensym.newText
  let projArgs = dataArgs ++ [(m, Ident.fromText structVarText, structType)]
  projectionName <- Throw.liftEither $ DD.extend m dataName $ Ident.toText elemName
  let newDD = DD.extendLL dataName $ LL.new [] BN.new
  let argList = flip map elemInfoList $ \(mx, x, _) -> (mx, RP.Var x)
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
            [ ( V.fromList [(m, RP.Cons (RP.DefiniteDescription newDD) argList)],
                preVar m (Ident.toText elemName)
              )
            ]
        )

parseAlias :: P.Parser RawStmt
parseAlias = do
  m <- P.getCurrentHint
  try $ P.keyword "alias"
  aliasName <- P.baseName
  aliasName' <- lift $ Locator.attachCurrentLocator aliasName
  P.betweenBrace $ do
    t <- rawExpr
    let stmtKind = Normal O.Transparent
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

weakTermToWeakIdent :: Hint -> P.Parser RT.RawTerm -> P.Parser (BinderF RT.RawTerm)
weakTermToWeakIdent m f = do
  a <- f
  h <- lift $ Gensym.newTextualIdentFromText "_"
  return (m, h, a)

registerTopLevelNames :: [RawStmt] -> App ()
registerTopLevelNames stmtList =
  case stmtList of
    [] ->
      return ()
    RawStmtDefine isConstLike stmtKind m functionName impArgNum xts _ _ : rest -> do
      Global.registerStmtDefine isConstLike m stmtKind functionName impArgNum $ AN.fromInt (length xts)
      registerTopLevelNames rest
    RawStmtSection section innerStmtList : rest -> do
      Locator.withSection section $ do
        registerTopLevelNames innerStmtList
        registerTopLevelNames rest
    RawStmtDefineResource m name _ _ : rest -> do
      Global.registerStmtDefineResource m name
      registerTopLevelNames rest
