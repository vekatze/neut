module Scene.Parse
  ( parse,
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
import Entity.Source
import Entity.Stmt
import qualified Entity.StrictGlobalLocator as SGL
import Path
import Scene.Parse.Core
import qualified Scene.Parse.Discern as Discern
import Scene.Parse.Enum
import qualified Scene.Parse.Import as Parse
import Scene.Parse.PreTerm
import Text.Megaparsec hiding (parse)

--
-- core functions
--

parse :: Context -> Source -> IO (Either [Stmt] ([WeakStmt], [EnumInfo]))
parse ctx source = do
  result <- parseSource ctx source
  mMainDD <- Locator.getMainDefiniteDescription (locator ctx) source
  case mMainDD of
    Just mainDD -> do
      let m = Entity.Hint.new 1 1 $ toFilePath $ sourceFilePath source
      ensureMain ctx m mainDD
      return result
    Nothing ->
      return result

parseSource :: Context -> Source -> IO (Either [Stmt] ([WeakStmt], [EnumInfo]))
parseSource ctx source = do
  mCache <- loadCache source (hasCacheSet ctx)
  case mCache of
    Just cache -> do
      let hint = Entity.Hint.new 1 1 $ toFilePath $ sourceFilePath source
      forM_ (cacheEnumInfo cache) $ \enumInfo ->
        uncurry (Global.registerEnum (global ctx) hint) (fromEnumInfo enumInfo)
      let stmtList = cacheStmtList cache
      forM_ stmtList $ \stmt -> do
        case stmt of
          StmtDefine _ _ name _ args _ _ ->
            Global.registerTopLevelFunc (global ctx) hint name $ A.fromInt (length args)
          StmtDefineResource _ name _ _ ->
            Global.registerResource (global ctx) hint name
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
    Just (GN.TopLevelFunc _) ->
      return ()
    _ ->
      Throw.raiseError (throw ctx) m "`main` is missing"

program :: Context -> Parser ([WeakStmt], [EnumInfo])
program ctx = do
  Parse.skipImportSequence
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
        stmtList <- many (parseStmt ctx) >>= liftIO . Discern.discernStmtList (Discern.specialize ctx) . concat
        return (stmtList, [])
    ]

parseStmtUse :: Context -> Parser ()
parseStmtUse ctx = do
  try $ keyword "use"
  loc <- parseLocator ctx
  case loc of
    Left partialLocator ->
      liftIO $ Locator.activateDefiniteLocator (locator ctx) partialLocator
    Right globalLocator ->
      liftIO $ Locator.activateGlobalLocator (locator ctx) globalLocator

parseLocator :: Context -> Parser (Either DL.DefiniteLocator SGL.StrictGlobalLocator)
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

parseDefiniteLocator :: Context -> Parser DL.DefiniteLocator
parseDefiniteLocator ctx = do
  m <- currentHint
  globalLocator <- symbol >>= liftIO . (GL.reflect (throw ctx) m >=> Alias.resolveAlias (alias ctx) m)
  delimiter definiteSep
  localLocator <- symbol
  baseNameList <- liftIO $ BN.bySplit (throw ctx) m localLocator
  return $ DL.new globalLocator $ map Section.Section baseNameList

parseGlobalLocator :: Context -> Parser SGL.StrictGlobalLocator
parseGlobalLocator ctx = do
  m <- currentHint
  gl <- symbol >>= liftIO . GL.reflect (throw ctx) m
  liftIO $ Alias.resolveAlias (alias ctx) m gl

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
  liftIO $ defineFunction ctx opacity m name' (I.fromInt $ length impArgs) (impArgs ++ expArgs) codType e

defineFunction ::
  Context ->
  Opacity ->
  Hint ->
  DD.DefiniteDescription ->
  I.ImpArgNum ->
  [BinderF PT.PreTerm] ->
  PT.PreTerm ->
  PT.PreTerm ->
  IO PreStmt
defineFunction ctx opacity m name impArgNum binder codType e = do
  Global.registerTopLevelFunc (global ctx) m name (A.fromInt (length binder))
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
  consInfoList' <- mapM (modifyConstructorName (throw ctx) m dataName) consInfoList
  setAsData (global ctx) m dataName (A.fromInt (length dataArgs)) consInfoList'
  let consType = m :< PT.Pi [] (m :< PT.Tau)
  let formRule = PreStmtDefine OpacityOpaque m dataName (I.fromInt 0) dataArgs (m :< PT.Tau) consType
  introRuleList <- parseDefineDataConstructor ctx dataName dataArgs consInfoList' D.zero
  return $ formRule : introRuleList

modifyConstructorName ::
  Throw.Context ->
  Hint ->
  DD.DefiniteDescription ->
  (Hint, T.Text, [BinderF PT.PreTerm]) ->
  IO (Hint, DD.DefiniteDescription, [BinderF PT.PreTerm])
modifyConstructorName ctx m dataDD (mb, consName, yts) = do
  consName' <- DD.extend ctx m dataDD consName
  return (mb, consName', yts)

parseDefineDataConstructor ::
  Context ->
  DD.DefiniteDescription ->
  [BinderF PT.PreTerm] ->
  [(Hint, DD.DefiniteDescription, [BinderF PT.PreTerm])] ->
  D.Discriminant ->
  IO [PreStmt]
parseDefineDataConstructor ctx dataName dataArgs consInfoList discriminant = do
  case consInfoList of
    [] ->
      return []
    (m, consName, consArgs) : rest -> do
      let consArgs' = map identPlusToVar consArgs
      let dataType = constructDataType m dataName dataArgs
      introRule <-
        defineFunction
          ctx
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
      introRuleList <- parseDefineDataConstructor ctx dataName dataArgs rest (D.increment discriminant)
      return $ introRule : introRuleList

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
  projectionName <- DD.extend (throw ctx) m dataName $ Ident.toText elemName
  let newDD = DD.extendLL dataName $ LL.new [] BN.new
  defineFunction
    ctx
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

setAsData ::
  Global.Context ->
  Hint ->
  DD.DefiniteDescription ->
  A.Arity ->
  [(Hint, DD.DefiniteDescription, [BinderF PT.PreTerm])] ->
  IO ()
setAsData ctx m dataName arity consInfoList = do
  let consNameList = map (\(_, consName, _) -> consName) consInfoList
  Global.registerData ctx m dataName arity consNameList

identPlusToVar :: BinderF PT.PreTerm -> PT.PreTerm
identPlusToVar (m, x, _) =
  m :< PT.Var x

weakTermToWeakIdent :: Gensym.Context -> Hint -> Parser PT.PreTerm -> Parser (BinderF PT.PreTerm)
weakTermToWeakIdent ctx m f = do
  a <- f
  h <- liftIO $ Gensym.newTextualIdentFromText ctx "_"
  return (m, h, a)
