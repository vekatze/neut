module Scene.Parse
  ( parse,
    parseCachedStmtList,
  )
where

import Context.Alias qualified as Alias
import Context.App
import Context.Cache qualified as Cache
import Context.CodataDefinition qualified as CodataDefinition
import Context.Enum qualified as Enum
import Context.Env qualified as Env
import Context.Gensym qualified as Gensym
import Context.Global qualified as Global
import Context.Locator qualified as Locator
import Context.Throw qualified as Throw
import Control.Comonad.Cofree hiding (section)
import Control.Monad
import Control.Monad.Trans
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
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
      return $ Right defList

parseCachedStmtList :: [Stmt] -> App ()
parseCachedStmtList stmtList = do
  forM_ stmtList $ \stmt -> do
    case stmt of
      StmtDefine stmtKind m name args _ _ ->
        case stmtKind of
          Normal _ ->
            Global.registerTopLevelFunc m name $ A.fromInt (length args)
          Data dataName dataArgs consInfoList -> do
            Global.registerData m dataName dataArgs consInfoList
            registerAsEnumIfNecessary dataName dataArgs consInfoList
          DataIntro {} ->
            return ()
      StmtDefineResource m name _ _ ->
        Global.registerResource m name

ensureMain :: Hint -> DD.DefiniteDescription -> App ()
ensureMain m mainFunctionName = do
  mMain <- Global.lookup mainFunctionName
  case mMain of
    Just (GN.TopLevelFunc _) ->
      return ()
    _ ->
      Throw.raiseError m "`main` is missing"

program :: P.Parser [WeakStmt]
program = do
  Parse.skipImportSequence
  parseStmtUseSequence
  (many parseStmt >>= lift . Discern.discernStmtList . concat) <* eof

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
    [ parseDefineData,
      parseDefineCodata,
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
  P.keyword "do"
  Locator.withLiftedSection section $ do
    stmtList <- concat <$> many parseStmt
    P.keyword "end"
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
  ((_, name), expArgs, codType, e) <- parseTopDefInfo
  name' <- lift $ Locator.attachCurrentLocator name
  lift $ defineFunction (Normal opacity) m name' expArgs codType e

defineFunction ::
  StmtKindF RT.RawTerm ->
  Hint ->
  DD.DefiniteDescription ->
  [BinderF RT.RawTerm] ->
  RT.RawTerm ->
  RT.RawTerm ->
  App RawStmt
defineFunction stmtKind m name binder codType e = do
  Global.registerTopLevelFunc m name (A.fromInt (length binder))
  return $ RawStmtDefine stmtKind m name binder codType e

parseDefineData :: P.Parser [RawStmt]
parseDefineData = do
  m <- P.getCurrentHint
  try $ P.keyword "define-data"
  a <- P.baseName >>= lift . Locator.attachCurrentLocator
  dataArgs <- P.argList preAscription
  consInfoList <- P.withBlock $ P.manyList parseDefineDataClause
  lift $ defineData m a dataArgs consInfoList

defineData ::
  Hint ->
  DD.DefiniteDescription ->
  [BinderF RT.RawTerm] ->
  [(Hint, T.Text, [BinderF RT.RawTerm])] ->
  App [RawStmt]
defineData m dataName dataArgs consInfoList = do
  consInfoList' <- mapM (modifyConstructorName m dataName) consInfoList
  let consInfoList'' = modifyConsInfo D.zero consInfoList'
  Global.registerData m dataName dataArgs consInfoList''
  let stmtKind = Data dataName dataArgs consInfoList''
  let dataType = constructDataType m dataName dataArgs
  let formRule = RawStmtDefine stmtKind m dataName dataArgs (m :< RT.Tau) dataType
  introRuleList <- parseDefineDataConstructor dataType dataName dataArgs consInfoList' D.zero
  registerAsEnumIfNecessary dataName dataArgs consInfoList''
  return $ formRule : introRuleList

registerAsEnumIfNecessary ::
  DD.DefiniteDescription ->
  [BinderF a] ->
  [(DD.DefiniteDescription, [BinderF a], D.Discriminant)] ->
  App ()
registerAsEnumIfNecessary dataName dataArgs consInfoList =
  when (hasNoArgs dataArgs consInfoList) $ do
    Enum.insert dataName
    mapM_ (Enum.insert . (\(consName, _, _) -> consName)) consInfoList

modifyConsInfo ::
  D.Discriminant ->
  [(a, DD.DefiniteDescription, [BinderF RT.RawTerm])] ->
  [(DD.DefiniteDescription, [BinderF RT.RawTerm], D.Discriminant)]
modifyConsInfo d consInfoList =
  case consInfoList of
    [] ->
      []
    (_, consName, consArgs) : rest ->
      (consName, consArgs, d) : modifyConsInfo (D.increment d) rest

hasNoArgs :: [BinderF a] -> [(DD.DefiniteDescription, [BinderF a], D.Discriminant)] -> Bool
hasNoArgs dataArgs consInfoList =
  null dataArgs && null (concatMap (\(_, consArgs, _) -> consArgs) consInfoList)

modifyConstructorName ::
  Hint ->
  DD.DefiniteDescription ->
  (Hint, T.Text, [BinderF RT.RawTerm]) ->
  App (Hint, DD.DefiniteDescription, [BinderF RT.RawTerm])
modifyConstructorName m dataDD (mb, consName, yts) = do
  consName' <- Throw.liftEither $ DD.extend m dataDD consName
  return (mb, consName', yts)

parseDefineDataConstructor ::
  RT.RawTerm ->
  DD.DefiniteDescription ->
  [BinderF RT.RawTerm] ->
  [(Hint, DD.DefiniteDescription, [BinderF RT.RawTerm])] ->
  D.Discriminant ->
  App [RawStmt]
parseDefineDataConstructor dataType dataName dataArgs consInfoList discriminant = do
  case consInfoList of
    [] ->
      return []
    (m, consName, consArgs) : rest -> do
      let dataArgs' = map identPlusToVar dataArgs
      let consArgs' = map identPlusToVar consArgs
      let args = dataArgs ++ consArgs
      let introRule =
            RawStmtDefine
              (DataIntro consName dataArgs consArgs discriminant)
              m
              consName
              args
              dataType
              $ m :< RT.DataIntro dataName consName discriminant dataArgs' consArgs'
      introRuleList <- parseDefineDataConstructor dataType dataName dataArgs rest (D.increment discriminant)
      return $ introRule : introRuleList

constructDataType ::
  Hint ->
  DD.DefiniteDescription ->
  [BinderF RT.RawTerm] ->
  RT.RawTerm
constructDataType m dataName dataArgs = do
  m :< RT.Data dataName (map identPlusToVar dataArgs)

parseDefineDataClause :: P.Parser (Hint, T.Text, [BinderF RT.RawTerm])
parseDefineDataClause = do
  m <- P.getCurrentHint
  b <- P.symbol
  yts <- P.argList parseDefineDataClauseArg
  return (m, b, yts)

parseDefineDataClauseArg :: P.Parser (BinderF RT.RawTerm)
parseDefineDataClauseArg = do
  m <- P.getCurrentHint
  choice
    [ try preAscription,
      weakTermToWeakIdent m rawTerm
    ]

parseDefineCodata :: P.Parser [RawStmt]
parseDefineCodata = do
  m <- P.getCurrentHint
  try $ P.keyword "define-codata"
  dataName <- P.baseName >>= lift . Locator.attachCurrentLocator
  dataArgs <- P.argList preAscription
  elemInfoList <- P.withBlock $ P.manyList preAscription
  formRule <- lift $ defineData m dataName dataArgs [(m, "new", elemInfoList)]
  elimRuleList <- mapM (lift . parseDefineCodataElim dataName dataArgs elemInfoList) elemInfoList
  -- register codata info for `new-with-end`
  dataNewName <- lift $ Throw.liftEither $ DD.extend m dataName "new"
  let numOfDataArgs = A.fromInt $ length dataArgs
  let numOfFields = A.fromInt $ length elemInfoList
  let (_, consInfoList, _) = unzip3 elemInfoList
  consNameList <- mapM (lift . Throw.liftEither . DD.extend m dataName . Ident.toText) consInfoList
  lift $ CodataDefinition.insert dataName (dataNewName, numOfDataArgs, numOfFields) consNameList
  -- ... then return
  return $ formRule ++ elimRuleList

-- noetic projection
parseDefineCodataElim ::
  DD.DefiniteDescription ->
  [BinderF RT.RawTerm] ->
  [BinderF RT.RawTerm] ->
  BinderF RT.RawTerm ->
  App RawStmt
parseDefineCodataElim dataName dataArgs elemInfoList (m, elemName, elemType) = do
  let codataType = m :< RT.Noema (constructDataType m dataName dataArgs)
  recordVarText <- Gensym.newText
  let projArgs = dataArgs ++ [(m, Ident.fromText recordVarText, codataType)]
  projectionName <- Throw.liftEither $ DD.extend m dataName $ Ident.toText elemName
  let newDD = DD.extendLL dataName $ LL.new [] BN.new
  let argList = flip map elemInfoList $ \(mx, x, _) -> (mx, RP.Var x)
  defineFunction
    (Normal O.Opaque)
    m
    projectionName -- e.g. some-lib.foo::my-record.element-x
    projArgs
    (m :< RT.Noema elemType)
    $ m
      :< RT.DataElim
        True
        [preVar m recordVarText]
        ( RP.new
            [ ( V.fromList [(m, RP.Cons (RP.DefiniteDescription newDD) argList)],
                preVar m (Ident.toText elemName)
              )
            ]
        )

parseDefineResource :: P.Parser RawStmt
parseDefineResource = do
  try $ P.keyword "define-resource"
  m <- P.getCurrentHint
  name <- P.baseName
  name' <- lift $ Locator.attachCurrentLocator name
  P.withBlock $ do
    discarder <- P.delimiter "-" >> rawTerm
    copier <- P.delimiter "-" >> rawTerm
    lift $ Global.registerResource m name'
    return $ RawStmtDefineResource m name' discarder copier

identPlusToVar :: BinderF RT.RawTerm -> RT.RawTerm
identPlusToVar (m, x, _) =
  m :< RT.Var x

weakTermToWeakIdent :: Hint -> P.Parser RT.RawTerm -> P.Parser (BinderF RT.RawTerm)
weakTermToWeakIdent m f = do
  a <- f
  h <- lift $ Gensym.newTextualIdentFromText "_"
  return (m, h, a)
