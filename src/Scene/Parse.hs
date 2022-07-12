module Scene.Parse
  ( parseMain,
    parseOther,
  )
where

import qualified Context.Alias as Alias
import Context.App
import qualified Context.Gensym as Gensym
import qualified Context.Global as Global
import qualified Context.Locator as Locator
import qualified Context.Throw as Throw
import Control.Comonad.Cofree hiding (section)
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as Map
import Data.IORef
import qualified Data.Text as T
import Entity.AliasInfo
import qualified Entity.BaseName as BN
import Entity.Binder
import Entity.Const
import qualified Entity.DefiniteDescription as DD
import qualified Entity.DefiniteLocator as DL
import Entity.EnumInfo
import Entity.Global
import qualified Entity.GlobalLocator as GL
import qualified Entity.GlobalName as GN
import Entity.Hint
import qualified Entity.Ident.Reflect as Ident
import qualified Entity.Ident.Reify as Ident
import Entity.LamKind
import Entity.Opacity
import qualified Entity.PreTerm as PT
import qualified Entity.Section as Section
import Entity.Source
import Entity.Stmt
import Entity.Stmt.Discern
import qualified Entity.StrictGlobalLocator as SGL
import qualified Entity.WeakTerm.Discern as WT
import Path
import Scene.Parse.Core
import Scene.Parse.Enum
import Scene.Parse.Import
import Scene.Parse.PreTerm
import Text.Megaparsec

--
-- core functions
--

parseMain :: Context -> DD.DefiniteDescription -> Source -> IO (Either [Stmt] ([WeakStmt], [EnumInfo]))
parseMain ctx mainFunctionName source = do
  result <- parseSource ctx source
  let m = Entity.Hint.new 1 1 $ toFilePath $ sourceFilePath source
  ensureMain ctx m mainFunctionName
  return result

parseOther :: Context -> Source -> IO (Either [Stmt] ([WeakStmt], [EnumInfo]))
parseOther =
  parseSource

parseSource :: Context -> Source -> IO (Either [Stmt] ([WeakStmt], [EnumInfo]))
parseSource ctx source = do
  mCache <- loadCache source (hasCacheSet ctx)
  case mCache of
    Just cache -> do
      let hint = Entity.Hint.new 1 1 $ toFilePath $ sourceFilePath source
      forM_ (cacheEnumInfo cache) $ \enumInfo ->
        uncurry (Global.registerEnum (global ctx) hint) (fromEnumInfo enumInfo)
      let stmtList = cacheStmtList cache
      forM_ (map extractName stmtList) $ Global.registerTopLevelFunc (global ctx) hint
      return $ Left stmtList
    Nothing -> do
      case Map.lookup (sourceFilePath source) (sourceAliasMap ctx) of
        Nothing ->
          Throw.raiseCritical' (throw ctx) "[activateAliasInfoOfCurrentFile] (compiler bug)"
        Just aliasInfoList ->
          activateAliasInfo (alias ctx) aliasInfoList
      (defList, enumInfoList) <- run (throw ctx) (program ctx) $ sourceFilePath source
      return $ Right (defList, enumInfoList)

ensureMain :: Context -> Hint -> DD.DefiniteDescription -> IO ()
ensureMain ctx m mainFunctionName = do
  mMain <- Global.lookup (global ctx) mainFunctionName
  case mMain of
    Just GN.TopLevelFunc ->
      return ()
    _ ->
      Throw.raiseError (throw ctx) m "`main` is missing"

program :: Context -> Parser ([WeakStmt], [EnumInfo])
program ctx = do
  skipImportSequence
  program' ctx <* eof

program' :: Context -> Parser ([WeakStmt], [EnumInfo])
program' ctx =
  choice
    [ do
        enumInfo <- parseDefineEnum ctx
        (defList, enumInfoList) <- program' ctx
        return (defList, enumInfo : enumInfoList),
      do
        parseStmtUse ctx
        program' ctx,
      do
        stmtList <- many (parseStmt ctx) >>= liftIO . discernStmtList (WT.specialize ctx) . concat
        return (stmtList, [])
    ]

parseStmtUse :: Context -> Parser ()
parseStmtUse ctx = do
  try $ keyword "use"
  loc <- parseLocator (alias ctx)
  case loc of
    Left partialLocator ->
      liftIO $ Locator.activateDefiniteLocator (locator ctx) partialLocator
    Right globalLocator ->
      liftIO $ Locator.activateGlobalLocator (locator ctx) globalLocator

parseLocator :: Alias.Context -> Parser (Either DL.DefiniteLocator SGL.StrictGlobalLocator)
parseLocator ctx = do
  choice
    [ Left <$> try (parseDefiniteLocator ctx),
      Right <$> parseGlobalLocator ctx
    ]

parseStmt :: Context -> Parser [PreStmt]
parseStmt ctx = do
  choice
    [ parseDefineData ctx,
      parseDefineCodata ctx,
      return <$> parseDefineResource ctx,
      return <$> parseDefine ctx OpacityTransparent,
      return <$> parseDefine ctx OpacityOpaque,
      return <$> parseSection ctx
    ]

parseDefiniteLocator :: Alias.Context -> Parser DL.DefiniteLocator
parseDefiniteLocator ctx = do
  m <- currentHint
  gl <- symbol >>= liftIO . (GL.reflect >=> Alias.resolveAlias ctx m)
  delimiter definiteSep
  ll <- symbol
  let sectionStack = map Section.Section $ BN.bySplit ll
  return $ DL.new gl sectionStack

parseGlobalLocator :: Alias.Context -> Parser SGL.StrictGlobalLocator
parseGlobalLocator ctx = do
  m <- currentHint
  gl <- symbol >>= liftIO . GL.reflect
  liftIO $ Alias.resolveAlias ctx m gl

--
-- parser for statements
--

parseSection :: Context -> Parser PreStmt
parseSection ctx = do
  try $ keyword "section"
  section <- Section.Section <$> baseName
  Locator.withSection (locator ctx) section $ do
    stmtList <- concat <$> many (parseStmt ctx)
    keyword "end"
    return $ PreStmtSection section stmtList

-- define name (x1 : A1) ... (xn : An) : A = e
parseDefine :: Context -> Opacity -> Parser PreStmt
parseDefine ctx opacity = do
  try $
    case opacity of
      OpacityOpaque ->
        keyword "define"
      OpacityTransparent ->
        keyword "define-inline"
  m <- currentHint
  ((_, name), impArgs, expArgs, codType, e) <- parseTopDefInfo ctx
  name' <- liftIO $ Locator.attachCurrentLocator (locator ctx) name
  liftIO $ defineFunction ctx opacity m name' (length impArgs) (impArgs ++ expArgs) codType e

defineFunction ::
  Context ->
  Opacity ->
  Hint ->
  DD.DefiniteDescription ->
  Int ->
  [BinderF PT.PreTerm] ->
  PT.PreTerm ->
  PT.PreTerm ->
  IO PreStmt
defineFunction ctx opacity m name impArgNum binder codType e = do
  Global.registerTopLevelFunc (global ctx) m name
  return $ PreStmtDefine opacity m name impArgNum binder codType e

parseDefineData :: Context -> Parser [PreStmt]
parseDefineData ctx = do
  m <- currentHint
  try $ keyword "define-data"
  a <- baseName >>= liftIO . Locator.attachCurrentLocator (locator ctx)
  dataArgs <- argList $ preAscription ctx
  consInfoList <- asBlock $ manyList $ parseDefineDataClause ctx
  liftIO $ defineData ctx m a dataArgs consInfoList

defineData ::
  Context ->
  Hint ->
  DD.DefiniteDescription ->
  [BinderF PT.PreTerm] ->
  [(Hint, T.Text, [BinderF PT.PreTerm])] ->
  IO [PreStmt]
defineData ctx m dataName dataArgs consInfoList = do
  consInfoList' <- mapM (modifyConstructorName dataName) consInfoList
  setAsData dataName (length dataArgs) consInfoList'
  let consType = m :< PT.Pi [] (m :< PT.Tau)
  formRule <- defineFunction ctx OpacityOpaque m dataName 0 dataArgs (m :< PT.Tau) consType
  introRuleList <- mapM (parseDefineDataConstructor ctx dataName dataArgs) $ zip consInfoList' [0 ..]
  return $ formRule : introRuleList

modifyConstructorName ::
  DD.DefiniteDescription ->
  (Hint, T.Text, [BinderF PT.PreTerm]) ->
  IO (Hint, DD.DefiniteDescription, [BinderF PT.PreTerm])
modifyConstructorName dataDD (mb, consName, yts) = do
  return (mb, DD.extend dataDD consName, yts)

parseDefineDataConstructor ::
  Context ->
  DD.DefiniteDescription ->
  [BinderF PT.PreTerm] ->
  ((Hint, DD.DefiniteDescription, [BinderF PT.PreTerm]), Integer) ->
  IO PreStmt
parseDefineDataConstructor ctx dataName dataArgs ((m, consName, consArgs), consNumber) = do
  let dataConsArgs = dataArgs ++ consArgs
  let consArgs' = map identPlusToVar consArgs
  let dataType = constructDataType m dataName dataArgs
  defineFunction
    ctx
    OpacityTransparent
    m
    consName
    (length dataArgs)
    dataConsArgs
    dataType
    $ m
      :< PT.PiIntro
        (LamKindCons dataName consName consNumber dataType)
        [ (m, Ident.fromText (DD.reify consName), m :< PT.Pi consArgs (m :< PT.Tau))
        ]
        (m :< PT.PiElim (preVar m (DD.reify consName)) consArgs')

constructDataType :: Hint -> DD.DefiniteDescription -> [BinderF PT.PreTerm] -> PT.PreTerm
constructDataType m dataName dataArgs =
  m :< PT.PiElim (m :< PT.VarGlobalStrict dataName) (map identPlusToVar dataArgs)

parseDefineDataClause :: Context -> Parser (Hint, T.Text, [BinderF PT.PreTerm])
parseDefineDataClause ctx = do
  m <- currentHint
  b <- symbol
  yts <- argList $ parseDefineDataClauseArg ctx
  return (m, b, yts)

parseDefineDataClauseArg :: Context -> Parser (BinderF PT.PreTerm)
parseDefineDataClauseArg ctx = do
  m <- currentHint
  choice
    [ try (preAscription ctx),
      weakTermToWeakIdent (gensym ctx) m (preTerm ctx)
    ]

parseDefineCodata :: Context -> Parser [PreStmt]
parseDefineCodata ctx = do
  m <- currentHint
  try $ keyword "define-codata"
  dataName <- baseName >>= liftIO . Locator.attachCurrentLocator (locator ctx)
  dataArgs <- argList $ preAscription ctx
  elemInfoList <- asBlock $ manyList $ preAscription ctx
  formRule <- liftIO $ defineData ctx m dataName dataArgs [(m, "new", elemInfoList)]
  elimRuleList <- liftIO $ mapM (parseDefineCodataElim ctx dataName dataArgs elemInfoList) elemInfoList
  return $ formRule ++ elimRuleList

parseDefineCodataElim ::
  Context ->
  DD.DefiniteDescription ->
  [BinderF PT.PreTerm] ->
  [BinderF PT.PreTerm] ->
  BinderF PT.PreTerm ->
  IO PreStmt
parseDefineCodataElim ctx dataName dataArgs elemInfoList (m, elemName, elemType) = do
  let codataType = constructDataType m dataName dataArgs
  recordVarText <- Gensym.newText (gensym ctx)
  let projArgs = dataArgs ++ [(m, Ident.fromText recordVarText, codataType)]
  let projectionName = DD.extend dataName $ Ident.toText elemName
  let newDD = DD.extend dataName "new"
  defineFunction
    ctx
    OpacityOpaque
    m
    projectionName -- e.g. some-lib.foo::my-record.element-x
    (length dataArgs)
    projArgs
    elemType
    $ m
      :< PT.Match
        Nothing
        (preVar m recordVarText, codataType)
        [((m, Right newDD, elemInfoList), preVar m (Ident.toText elemName))]

parseDefineResource :: Context -> Parser PreStmt
parseDefineResource ctx = do
  try $ keyword "define-resource"
  m <- currentHint
  name <- baseName
  name' <- liftIO $ Locator.attachCurrentLocator (locator ctx) name
  asBlock $ do
    discarder <- delimiter "-" >> preTerm ctx
    copier <- delimiter "-" >> preTerm ctx
    liftIO $ Global.registerResource (global ctx) m name'
    return $ PreStmtDefineResource m name' discarder copier

setAsData :: DD.DefiniteDescription -> Int -> [(Hint, DD.DefiniteDescription, [BinderF PT.PreTerm])] -> IO ()
setAsData dataName dataArgNum consInfoList = do
  let consNameList = map (\(_, consName, _) -> consName) consInfoList
  modifyIORef' dataEnvRef $ Map.insert dataName consNameList
  forM_ consNameList $ \consName ->
    modifyIORef' constructorEnvRef $ Map.insert consName dataArgNum

identPlusToVar :: BinderF PT.PreTerm -> PT.PreTerm
identPlusToVar (m, x, _) =
  m :< PT.Var x

weakTermToWeakIdent :: Gensym.Context -> Hint -> Parser PT.PreTerm -> Parser (BinderF PT.PreTerm)
weakTermToWeakIdent ctx m f = do
  a <- f
  h <- liftIO $ Gensym.newTextualIdentFromText ctx "_"
  return (m, h, a)
