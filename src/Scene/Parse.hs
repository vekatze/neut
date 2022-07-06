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
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.HashMap.Lazy as Map
import Data.IORef
import qualified Data.Text as T
import Entity.AliasInfo
import Entity.Binder
import Entity.EnumInfo
import Entity.Global
import qualified Entity.GlobalName as GN
import Entity.Hint
import qualified Entity.Ident.Reflect as Ident
import qualified Entity.Ident.Reify as Ident
import Entity.LamKind
import Entity.Opacity
import Entity.Source
import Entity.Stmt
import Entity.Stmt.Discern
import Entity.WeakTerm
import qualified Entity.WeakTerm.Discern as WT
import Path
import Scene.Parse.Core
import Scene.Parse.Enum
import Scene.Parse.Import
import Scene.Parse.WeakTerm
import Text.Megaparsec

--
-- core functions
--

parseMain :: Context -> T.Text -> Source -> IO (Either [Stmt] ([QuasiStmt], [EnumInfo]))
parseMain ctx mainFunctionName source = do
  result <- parseSource ctx source
  let m = Entity.Hint.new 1 1 $ toFilePath $ sourceFilePath source
  ensureMain ctx m mainFunctionName
  return result

parseOther :: Context -> Source -> IO (Either [Stmt] ([QuasiStmt], [EnumInfo]))
parseOther =
  parseSource

parseSource :: Context -> Source -> IO (Either [Stmt] ([QuasiStmt], [EnumInfo]))
parseSource ctx source = do
  mCache <- loadCache source
  case mCache of
    Just cache -> do
      let hint = Entity.Hint.new 1 1 $ toFilePath $ sourceFilePath source
      forM_ (cacheEnumInfo cache) $ \enumInfo ->
        uncurry (Global.registerEnum (global ctx) hint) (fromEnumInfo enumInfo)
      let stmtList = cacheStmtList cache
      forM_ (map extractName stmtList) $ Global.registerTopLevelFunc (global ctx) hint
      return $ Left stmtList
    Nothing -> do
      sourceAliasMap <- readIORef sourceAliasMapRef
      case Map.lookup (sourceFilePath source) sourceAliasMap of
        Nothing ->
          Throw.raiseCritical' (throw ctx) "[activateAliasInfoOfCurrentFile] (compiler bug)"
        Just aliasInfoList ->
          activateAliasInfo (alias ctx) aliasInfoList
      (defList, enumInfoList) <- run (throw ctx) (program ctx) $ sourceFilePath source
      return $ Right (defList, enumInfoList)

ensureMain :: Context -> Hint -> T.Text -> IO ()
ensureMain ctx m mainFunctionName = do
  mMain <- Global.lookup (global ctx) mainFunctionName
  case mMain of
    Just GN.TopLevelFunc ->
      return ()
    _ ->
      Throw.raiseError (throw ctx) m "`main` is missing"

program :: Context -> Parser ([QuasiStmt], [EnumInfo])
program ctx = do
  skipImportSequence
  program' ctx <* eof

program' :: Context -> Parser ([QuasiStmt], [EnumInfo])
program' ctx =
  choice
    [ do
        enumInfo <- parseDefineEnum ctx
        (defList, enumInfoList) <- program' ctx
        return (defList, enumInfo : enumInfoList),
      do
        parseDefinePrefix ctx
        program' ctx,
      do
        parseStmtUse ctx
        program' ctx,
      do
        stmtList <- many (parseStmt ctx) >>= liftIO . discernStmtList (WT.specialize ctx) . concat
        return (stmtList, [])
    ]

parseDefinePrefix :: Context -> Parser ()
parseDefinePrefix ctx = do
  m <- currentHint
  try $ keyword "define-prefix"
  from <- snd <$> var
  delimiter "="
  to <- snd <$> var
  liftIO $ Alias.registerLocatorAlias (alias ctx) m from to

parseStmtUse :: Context -> Parser ()
parseStmtUse ctx = do
  try $ keyword "use"
  (_, name) <- parseDefiniteDescription
  liftIO $ Locator.activatePartialLocator (locator ctx) name

parseStmt :: Context -> Parser [WeakStmt]
parseStmt ctx = do
  choice
    [ parseDefineData ctx,
      parseDefineCodata ctx,
      return <$> parseDefineResource ctx,
      return <$> parseDefine ctx OpacityTransparent,
      return <$> parseDefine ctx OpacityOpaque,
      return <$> parseSection ctx
    ]

--
-- parser for statements
--

parseSection :: Context -> Parser WeakStmt
parseSection ctx = do
  try $ keyword "section"
  sectionName <- symbol
  -- liftIO $ modifyIORef' isPrivateStackRef $ (:) (sectionName == "private")
  liftIO $ Locator.pushToCurrentLocalLocator (locator ctx) sectionName
  stmtList <- concat <$> many (parseStmt ctx)
  m <- currentHint
  keyword "end"
  _ <- liftIO $ Locator.popFromCurrentLocalLocator (locator ctx) m
  -- liftIO $ modifyIORef' isPrivateStackRef tail
  return $ WeakStmtSection m sectionName stmtList

-- define name (x1 : A1) ... (xn : An) : A = e
parseDefine :: Context -> Opacity -> Parser WeakStmt
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
  T.Text ->
  Int ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  WeakTerm ->
  IO WeakStmt
defineFunction ctx opacity m name impArgNum binder codType e = do
  Global.registerTopLevelFunc (global ctx) m name
  return $ WeakStmtDefine opacity m name impArgNum binder codType e

parseDefineData :: Context -> Parser [WeakStmt]
parseDefineData ctx = do
  m <- currentHint
  try $ keyword "define-data"
  a <- var >>= liftIO . Locator.attachCurrentLocator (locator ctx) . snd
  dataArgs <- argList $ weakAscription ctx
  consInfoList <- asBlock $ manyList $ parseDefineDataClause ctx
  liftIO $ defineData ctx m a dataArgs consInfoList

defineData ::
  Context ->
  Hint ->
  T.Text ->
  [BinderF WeakTerm] ->
  [(Hint, T.Text, [BinderF WeakTerm])] ->
  IO [WeakStmt]
defineData ctx m dataName dataArgs consInfoList = do
  consInfoList' <- mapM (modifyConstructorName dataName) consInfoList
  setAsData dataName (length dataArgs) consInfoList'
  let consType = m :< WeakTermPi [] (m :< WeakTermTau)
  formRule <- defineFunction ctx OpacityOpaque m dataName 0 dataArgs (m :< WeakTermTau) consType
  introRuleList <- mapM (parseDefineDataConstructor ctx dataName dataArgs) $ zip consInfoList' [0 ..]
  return $ formRule : introRuleList

modifyConstructorName :: T.Text -> (Hint, T.Text, [BinderF WeakTerm]) -> IO (Hint, T.Text, [BinderF WeakTerm])
modifyConstructorName dataName (mb, b, yts) = do
  return (mb, dataName <> nsSep <> b, yts)

parseDefineDataConstructor ::
  Context ->
  T.Text ->
  [BinderF WeakTerm] ->
  ((Hint, T.Text, [BinderF WeakTerm]), Integer) ->
  IO WeakStmt
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
      :< WeakTermPiIntro
        (LamKindCons dataName consName consNumber dataType)
        [ (m, Ident.fromText consName, m :< WeakTermPi consArgs (m :< WeakTermTau))
        ]
        (m :< WeakTermPiElim (weakVar m consName) consArgs')

constructDataType :: Hint -> T.Text -> [BinderF WeakTerm] -> WeakTerm
constructDataType m dataName dataArgs =
  m :< WeakTermPiElim (m :< WeakTermVarGlobal dataName) (map identPlusToVar dataArgs)

parseDefineDataClause :: Context -> Parser (Hint, T.Text, [BinderF WeakTerm])
parseDefineDataClause ctx = do
  m <- currentHint
  b <- symbol
  yts <- argList $ parseDefineDataClauseArg ctx
  return (m, b, yts)

parseDefineDataClauseArg :: Context -> Parser (BinderF WeakTerm)
parseDefineDataClauseArg ctx = do
  m <- currentHint
  choice
    [ try (weakAscription ctx),
      weakTermToWeakIdent (gensym ctx) m (weakTerm ctx)
    ]

parseDefineCodata :: Context -> Parser [WeakStmt]
parseDefineCodata ctx = do
  m <- currentHint
  try $ keyword "define-codata"
  dataName <- var >>= liftIO . Locator.attachCurrentLocator (locator ctx) . snd
  dataArgs <- argList $ weakAscription ctx
  elemInfoList <- asBlock $ manyList $ weakAscription ctx
  formRule <- liftIO $ defineData ctx m dataName dataArgs [(m, "new", elemInfoList)]
  elimRuleList <- liftIO $ mapM (parseDefineCodataElim ctx dataName dataArgs elemInfoList) elemInfoList
  return $ formRule ++ elimRuleList

parseDefineCodataElim :: Context -> T.Text -> [BinderF WeakTerm] -> [BinderF WeakTerm] -> BinderF WeakTerm -> IO WeakStmt
parseDefineCodataElim ctx dataName dataArgs elemInfoList (m, elemName, elemType) = do
  let codataType = constructDataType m dataName dataArgs
  recordVarText <- Gensym.newText (gensym ctx)
  let projArgs = dataArgs ++ [(m, Ident.fromText recordVarText, codataType)]
  let elemName' = dataName <> nsSep <> Ident.toText elemName
  defineFunction
    ctx
    OpacityOpaque
    m
    elemName'
    (length dataArgs)
    projArgs
    elemType
    $ m
      :< WeakTermMatch
        Nothing
        (weakVar m recordVarText, codataType)
        [((m, dataName <> nsSep <> "new", elemInfoList), weakVar m (Ident.toText elemName))]

parseDefineResource :: Context -> Parser WeakStmt
parseDefineResource ctx = do
  try $ keyword "define-resource"
  m <- currentHint
  name <- snd <$> var
  asBlock $ do
    discarder <- delimiter "-" >> weakTerm ctx
    copier <- delimiter "-" >> weakTerm ctx
    liftIO $ Global.registerTopLevelFunc (global ctx) m name
    return $ WeakStmtDefineResource m name discarder copier

setAsData :: T.Text -> Int -> [(Hint, T.Text, [BinderF WeakTerm])] -> IO ()
setAsData dataName dataArgNum consInfoList = do
  let consNameList = map (\(_, consName, _) -> consName) consInfoList
  modifyIORef' dataEnvRef $ Map.insert dataName consNameList
  forM_ consNameList $ \consName ->
    modifyIORef' constructorEnvRef $ Map.insert consName dataArgNum

identPlusToVar :: BinderF WeakTerm -> WeakTerm
identPlusToVar (m, x, _) =
  m :< WeakTermVar x

weakTermToWeakIdent :: Gensym.Context -> Hint -> Parser WeakTerm -> Parser (BinderF WeakTerm)
weakTermToWeakIdent ctx m f = do
  a <- f
  h <- liftIO $ Gensym.newTextualIdentFromText ctx "_"
  return (m, h, a)
