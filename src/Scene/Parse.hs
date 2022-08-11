module Scene.Parse
  ( parse,
  )
where

import qualified Context.Alias as Alias
import qualified Context.Env as Env
import qualified Context.Gensym as Gensym
import qualified Context.Global as Global
import qualified Context.Locator as Locator
import qualified Context.Throw as Throw
import Control.Comonad.Cofree hiding (section)
import Control.Monad
import Control.Monad.Trans
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import Entity.AliasInfo
import qualified Entity.Arity as A
import qualified Entity.BaseName as BN
import Entity.Binder
import Entity.Const
import qualified Entity.DefiniteDescription as DD
import qualified Entity.DefiniteLocator as DL
import qualified Entity.Discriminant as D
import Entity.EnumInfo
import qualified Entity.GlobalLocator as GL
import qualified Entity.GlobalName as GN
import Entity.Hint
import qualified Entity.Ident.Reflect as Ident
import qualified Entity.Ident.Reify as Ident
import qualified Entity.ImpArgNum as I
import Entity.LamKind
import qualified Entity.LocalLocator as LL
import Entity.Opacity
import qualified Entity.PreTerm as PT
import qualified Entity.Section as Section
import qualified Entity.Source as Source
import Entity.Stmt
import qualified Entity.StrictGlobalLocator as SGL
import Path
import qualified Scene.Parse.Core as P
import qualified Scene.Parse.Discern as Discern
import qualified Scene.Parse.Enum as Enum
import qualified Scene.Parse.Import as Parse
import Scene.Parse.PreTerm
import Text.Megaparsec hiding (parse)

class
  ( Alias.Context m,
    Gensym.Context m,
    Global.Context m,
    Locator.Context m,
    Throw.Context m,
    Env.Context m,
    Enum.Context m,
    Discern.Context m,
    Parse.Context m,
    P.Context m
  ) =>
  Context m
  where
  loadCache :: Source.Source -> PathSet -> m (Maybe Cache)

--
-- core functions
--

parse :: Context m => Source.Source -> m (Either [Stmt] ([WeakStmt], [EnumInfo]))
parse source = do
  result <- parseSource source
  mMainDD <- Locator.getMainDefiniteDescription source
  case mMainDD of
    Just mainDD -> do
      let m = Entity.Hint.new 1 1 $ toFilePath $ Source.sourceFilePath source
      ensureMain m mainDD
      return result
    Nothing ->
      return result

parseSource :: Context m => Source.Source -> m (Either [Stmt] ([WeakStmt], [EnumInfo]))
parseSource source = do
  hasCacheSet <- Env.getHasCacheSet
  mCache <- loadCache source hasCacheSet
  case mCache of
    Just cache -> do
      let hint = Entity.Hint.new 1 1 $ toFilePath $ Source.sourceFilePath source
      forM_ (cacheEnumInfo cache) $ \enumInfo ->
        uncurry (Global.registerEnum hint) (fromEnumInfo enumInfo)
      let stmtList = cacheStmtList cache
      forM_ stmtList $ \stmt -> do
        case stmt of
          StmtDefine _ _ name _ args _ _ ->
            Global.registerTopLevelFunc hint name $ A.fromInt (length args)
          StmtDefineResource _ name _ _ ->
            Global.registerResource hint name
      return $ Left stmtList
    Nothing -> do
      sourceAliasMap <- Env.getSourceAliasMap
      case Map.lookup (Source.sourceFilePath source) sourceAliasMap of
        Nothing ->
          Throw.raiseCritical' "[activateAliasInfoOfCurrentFile] (compiler bug)"
        Just aliasInfoList ->
          activateAliasInfo aliasInfoList
      (defList, enumInfoList) <- P.run program $ Source.sourceFilePath source
      return $ Right (defList, enumInfoList)

ensureMain :: Context m => Hint -> DD.DefiniteDescription -> m ()
ensureMain m mainFunctionName = do
  mMain <- Global.lookup mainFunctionName
  case mMain of
    Just (GN.TopLevelFunc _) ->
      return ()
    _ ->
      Throw.raiseError m "`main` is missing"

program :: Context m => P.Parser m ([WeakStmt], [EnumInfo])
program = do
  Parse.skipImportSequence
  program' <* eof

program' :: Context m => P.Parser m ([WeakStmt], [EnumInfo])
program' =
  choice
    [ do
        enumInfo <- Enum.parseDefineEnum
        (defList, enumInfoList) <- program'
        return (defList, enumInfo : enumInfoList),
      do
        parseStmtUse
        program',
      do
        stmtList <- many parseStmt >>= lift . Discern.discernStmtList . concat
        -- stmtList <- many (parseStmt) >>= liftm . Discern.discernStmtList (Discern.specialize) . concat
        return (stmtList, [])
    ]

parseStmtUse :: Context m => P.Parser m ()
parseStmtUse = do
  try $ P.keyword "use"
  loc <- parseLocator
  case loc of
    Left partialLocator ->
      lift $ Locator.activateDefiniteLocator partialLocator
    Right globalLocator ->
      lift $ Locator.activateGlobalLocator globalLocator

parseLocator :: Context m => P.Parser m (Either DL.DefiniteLocator SGL.StrictGlobalLocator)
parseLocator = do
  choice
    [ Left <$> try parseDefiniteLocator,
      Right <$> parseGlobalLocator
    ]

parseStmt :: Context m => P.Parser m [PreStmt]
parseStmt = do
  choice
    [ parseDefineData,
      parseDefineCodata,
      return <$> parseDefineResource,
      return <$> parseDefine OpacityTransparent,
      return <$> parseDefine OpacityOpaque,
      return <$> parseSection
    ]

parseDefiniteLocator :: Context m => P.Parser m DL.DefiniteLocator
parseDefiniteLocator = do
  m <- lift P.getCurrentHint
  globalLocator <- P.symbol >>= lift . (GL.reflect m >=> Alias.resolveAlias m)
  P.delimiter definiteSep
  localLocator <- P.symbol
  baseNameList <- lift $ BN.bySplit m localLocator
  return $ DL.new globalLocator $ map Section.Section baseNameList

parseGlobalLocator :: Context m => P.Parser m SGL.StrictGlobalLocator
parseGlobalLocator = do
  m <- lift P.getCurrentHint
  gl <- P.symbol >>= lift . GL.reflect m
  lift $ Alias.resolveAlias m gl

--
-- parser for statements
--

parseSection :: Context m => P.Parser m PreStmt
parseSection = do
  try $ P.keyword "section"
  section <- Section.Section <$> P.baseName
  Locator.withLiftedSection section $ do
    stmtList <- concat <$> many parseStmt
    P.keyword "end"
    return $ PreStmtSection section stmtList

-- define name (x1 : A1) ... (xn : An) : A = e
parseDefine :: Context m => Opacity -> P.Parser m PreStmt
parseDefine opacity = do
  try $
    case opacity of
      OpacityOpaque ->
        P.keyword "define"
      OpacityTransparent ->
        P.keyword "define-inline"
  m <- lift P.getCurrentHint
  ((_, name), impArgs, expArgs, codType, e) <- parseTopDefInfo
  name' <- lift $ Locator.attachCurrentLocator name
  lift $ defineFunction opacity m name' (I.fromInt $ length impArgs) (impArgs ++ expArgs) codType e

defineFunction ::
  Context m =>
  Opacity ->
  Hint ->
  DD.DefiniteDescription ->
  I.ImpArgNum ->
  [BinderF PT.PreTerm] ->
  PT.PreTerm ->
  PT.PreTerm ->
  m PreStmt
defineFunction opacity m name impArgNum binder codType e = do
  Global.registerTopLevelFunc m name (A.fromInt (length binder))
  return $ PreStmtDefine opacity m name impArgNum binder codType e

parseDefineData :: Context m => P.Parser m [PreStmt]
parseDefineData = do
  m <- lift P.getCurrentHint
  try $ P.keyword "define-data"
  a <- P.baseName >>= lift . Locator.attachCurrentLocator
  dataArgs <- P.argList preAscription
  consInfoList <- P.asBlock $ P.manyList parseDefineDataClause
  lift $ defineData m a dataArgs consInfoList

defineData ::
  Context m =>
  Hint ->
  DD.DefiniteDescription ->
  [BinderF PT.PreTerm] ->
  [(Hint, T.Text, [BinderF PT.PreTerm])] ->
  m [PreStmt]
defineData m dataName dataArgs consInfoList = do
  consInfoList' <- mapM (modifyConstructorName m dataName) consInfoList
  setAsData m dataName (A.fromInt (length dataArgs)) consInfoList'
  let consType = m :< PT.Pi [] (m :< PT.Tau)
  let formRule = PreStmtDefine OpacityOpaque m dataName (I.fromInt 0) dataArgs (m :< PT.Tau) consType
  introRuleList <- parseDefineDataConstructor dataName dataArgs consInfoList' D.zero
  return $ formRule : introRuleList

modifyConstructorName ::
  Throw.Context m =>
  Hint ->
  DD.DefiniteDescription ->
  (Hint, T.Text, [BinderF PT.PreTerm]) ->
  m (Hint, DD.DefiniteDescription, [BinderF PT.PreTerm])
modifyConstructorName m dataDD (mb, consName, yts) = do
  consName' <- DD.extend m dataDD consName
  return (mb, consName', yts)

parseDefineDataConstructor ::
  Context m =>
  DD.DefiniteDescription ->
  [BinderF PT.PreTerm] ->
  [(Hint, DD.DefiniteDescription, [BinderF PT.PreTerm])] ->
  D.Discriminant ->
  m [PreStmt]
parseDefineDataConstructor dataName dataArgs consInfoList discriminant = do
  case consInfoList of
    [] ->
      return []
    (m, consName, consArgs) : rest -> do
      let consArgs' = map identPlusToVar consArgs
      let dataType = constructDataType m dataName dataArgs
      introRule <-
        defineFunction
          OpacityTransparent
          m
          consName
          (I.fromInt $ length dataArgs)
          (dataArgs ++ consArgs)
          dataType
          $ m
            :< PT.PiIntro
              (LamKindCons dataName consName discriminant dataType)
              [ (m, Ident.fromText (DD.reify consName), m :< PT.Pi consArgs (m :< PT.Tau))
              ]
              (m :< PT.PiElim (preVar m (DD.reify consName)) consArgs')
      introRuleList <- parseDefineDataConstructor dataName dataArgs rest (D.increment discriminant)
      return $ introRule : introRuleList

constructDataType :: Hint -> DD.DefiniteDescription -> [BinderF PT.PreTerm] -> PT.PreTerm
constructDataType m dataName dataArgs =
  m :< PT.PiElim (m :< PT.VarGlobalStrict dataName) (map identPlusToVar dataArgs)

parseDefineDataClause :: Context m => P.Parser m (Hint, T.Text, [BinderF PT.PreTerm])
parseDefineDataClause = do
  m <- lift P.getCurrentHint
  b <- P.symbol
  yts <- P.argList parseDefineDataClauseArg
  return (m, b, yts)

parseDefineDataClauseArg :: Context m => P.Parser m (BinderF PT.PreTerm)
parseDefineDataClauseArg = do
  m <- lift P.getCurrentHint
  choice
    [ try preAscription,
      weakTermToWeakIdent m preTerm
    ]

parseDefineCodata :: Context m => P.Parser m [PreStmt]
parseDefineCodata = do
  m <- lift P.getCurrentHint
  try $ P.keyword "define-codata"
  dataName <- P.baseName >>= lift . Locator.attachCurrentLocator
  dataArgs <- P.argList preAscription
  elemInfoList <- P.asBlock $ P.manyList preAscription
  formRule <- lift $ defineData m dataName dataArgs [(m, "new", elemInfoList)]
  elimRuleList <- mapM (lift . parseDefineCodataElim dataName dataArgs elemInfoList) elemInfoList
  return $ formRule ++ elimRuleList

parseDefineCodataElim ::
  Context m =>
  DD.DefiniteDescription ->
  [BinderF PT.PreTerm] ->
  [BinderF PT.PreTerm] ->
  BinderF PT.PreTerm ->
  m PreStmt
parseDefineCodataElim dataName dataArgs elemInfoList (m, elemName, elemType) = do
  let codataType = constructDataType m dataName dataArgs
  recordVarText <- Gensym.newText
  let projArgs = dataArgs ++ [(m, Ident.fromText recordVarText, codataType)]
  projectionName <- DD.extend m dataName $ Ident.toText elemName
  let newDD = DD.extendLL dataName $ LL.new [] BN.new
  defineFunction
    OpacityOpaque
    m
    projectionName -- e.g. some-lib.foo::my-record.element-x
    (I.fromInt $ length dataArgs)
    projArgs
    elemType
    $ m
      :< PT.Match
        Nothing
        (preVar m recordVarText, codataType)
        [((m, Right newDD, elemInfoList), preVar m (Ident.toText elemName))]

parseDefineResource :: Context m => P.Parser m PreStmt
parseDefineResource = do
  try $ P.keyword "define-resource"
  m <- lift P.getCurrentHint
  name <- P.baseName
  name' <- lift $ Locator.attachCurrentLocator name
  P.asBlock $ do
    discarder <- P.delimiter "-" >> preTerm
    copier <- P.delimiter "-" >> preTerm
    lift $ Global.registerResource m name'
    return $ PreStmtDefineResource m name' discarder copier

setAsData ::
  Global.Context m =>
  Hint ->
  DD.DefiniteDescription ->
  A.Arity ->
  [(Hint, DD.DefiniteDescription, [BinderF PT.PreTerm])] ->
  m ()
setAsData m dataName arity consInfoList = do
  let consNameList = map (\(_, consName, _) -> consName) consInfoList
  Global.registerData m dataName arity consNameList

identPlusToVar :: BinderF PT.PreTerm -> PT.PreTerm
identPlusToVar (m, x, _) =
  m :< PT.Var x

weakTermToWeakIdent :: Context m => Hint -> P.Parser m PT.PreTerm -> P.Parser m (BinderF PT.PreTerm)
weakTermToWeakIdent m f = do
  a <- f
  h <- lift $ Gensym.newTextualIdentFromText "_"
  return (m, h, a)
