module Scene.Parse
  ( parseMain,
    parseOther,
  )
where

import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.HashMap.Lazy as Map
import Data.IORef
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.AliasInfo
import Entity.Binder
import Entity.EnumInfo
import qualified Entity.EnumInfo.Env as EnumInfo
import Entity.Global
import Entity.Hint
import qualified Entity.Ident.Reflect as Ident
import qualified Entity.Ident.Reify as Ident
import Entity.LamKind
import Entity.Log
import Entity.Namespace
import Entity.Opacity
import Entity.Source
import Entity.Stmt
import Entity.Stmt.Discern
import Entity.WeakTerm
import Scene.Parse.Core
import Scene.Parse.Enum
import Scene.Parse.Import
import Scene.Parse.WeakTerm
import System.IO.Unsafe
import Text.Megaparsec

--
-- core functions
--

parseMain :: T.Text -> Source -> IO (Either [Stmt] ([QuasiStmt], [EnumInfo]))
parseMain mainFunctionName source = do
  result <- parseSource source
  ensureMain mainFunctionName
  return result

parseOther :: Source -> IO (Either [Stmt] ([QuasiStmt], [EnumInfo]))
parseOther =
  parseSource

parseSource :: Source -> IO (Either [Stmt] ([QuasiStmt], [EnumInfo]))
parseSource source = do
  setCurrentFilePath $ sourceFilePath source
  mCache <- loadCache source
  initializeNamespace source
  setupSectionPrefix source
  case mCache of
    Just cache -> do
      forM_ (cacheEnumInfo cache) EnumInfo.register
      let stmtList = cacheStmtList cache
      let names = S.fromList $ map extractName stmtList
      modifyIORef' topNameSetRef $ S.union names
      return $ Left stmtList
    Nothing -> do
      getCurrentFilePath >>= activateAliasInfo
      (defList, enumInfoList) <- run program $ sourceFilePath source
      privateNameSet <- readIORef privateNameSetRef
      modifyIORef' topNameSetRef $ S.filter (`S.notMember` privateNameSet)
      return $ Right (defList, enumInfoList)

ensureMain :: T.Text -> IO ()
ensureMain mainFunctionName = do
  let m = error "undefined"
  topNameSet <- readIORef topNameSetRef
  currentGlobalLocator <- readIORef currentGlobalLocatorRef
  if S.member mainFunctionName topNameSet
    then return ()
    else raiseError m $ "`main` is missing in `" <> currentGlobalLocator <> "`"

program :: Parser ([QuasiStmt], [EnumInfo])
program = do
  skipImportSequence
  program' <* eof

program' :: Parser ([QuasiStmt], [EnumInfo])
program' =
  choice
    [ do
        enumInfo <- parseDefineEnum
        (defList, enumInfoList) <- program'
        return (defList, enumInfo : enumInfoList),
      do
        parseDefinePrefix
        program',
      do
        parseStmtUse
        program',
      do
        stmtList <- many parseStmt >>= liftIO . discernStmtList . concat
        return (stmtList, [])
    ]

parseDefinePrefix :: Parser ()
parseDefinePrefix = do
  m <- currentHint
  try $ keyword "define-prefix"
  from <- snd <$> var
  delimiter "="
  to <- snd <$> var
  liftIO $ handleDefinePrefix m from to

parseStmtUse :: Parser ()
parseStmtUse = do
  try $ keyword "use"
  (_, name) <- parseDefiniteDescription
  liftIO $ activateLocalLocator name

parseStmt :: Parser [WeakStmt]
parseStmt = do
  choice
    [ parseDefineData,
      parseDefineCodata,
      return <$> parseDefineResource,
      return <$> parseDefine OpacityTransparent,
      return <$> parseDefine OpacityOpaque,
      return <$> parseSection
    ]

--
-- parser for statements
--

parseSection :: Parser WeakStmt
parseSection = do
  try $ keyword "section"
  sectionName <- symbol
  liftIO $ modifyIORef' isPrivateStackRef $ (:) (sectionName == "private")
  liftIO $ pushToCurrentLocalLocator sectionName
  stmtList <- concat <$> many parseStmt
  m <- currentHint
  keyword "end"
  _ <- liftIO $ popFromCurrentLocalLocator m
  liftIO $ modifyIORef' isPrivateStackRef tail
  return $ WeakStmtSection m sectionName stmtList

-- define name (x1 : A1) ... (xn : An) : A = e
parseDefine :: Opacity -> Parser WeakStmt
parseDefine opacity = do
  m <- currentHint
  try $
    case opacity of
      OpacityOpaque ->
        keyword "define"
      OpacityTransparent ->
        keyword "define-inline"
  ((_, name), impArgs, expArgs, codType, e) <- parseTopDefInfo
  name' <- liftIO $ attachSectionPrefix name
  liftIO $ defineFunction opacity m name' (length impArgs) (impArgs ++ expArgs) codType e

defineFunction :: Opacity -> Hint -> T.Text -> Int -> [BinderF WeakTerm] -> WeakTerm -> WeakTerm -> IO WeakStmt
defineFunction opacity m name impArgNum binder codType e = do
  registerTopLevelName m name
  return $ WeakStmtDefine opacity m name impArgNum binder codType e

parseDefineData :: Parser [WeakStmt]
parseDefineData = do
  m <- currentHint
  try $ keyword "define-data"
  a <- var >>= liftIO . attachSectionPrefix . snd
  dataArgs <- argList weakAscription
  consInfoList <- asBlock $ manyList parseDefineDataClause
  liftIO $ defineData m a dataArgs consInfoList

defineData :: Hint -> T.Text -> [BinderF WeakTerm] -> [(Hint, T.Text, [BinderF WeakTerm])] -> IO [WeakStmt]
defineData m dataName dataArgs consInfoList = do
  consInfoList' <- mapM (modifyConstructorName dataName) consInfoList
  setAsData dataName (length dataArgs) consInfoList'
  let consType = m :< WeakTermPi [] (m :< WeakTermTau)
  formRule <- defineFunction OpacityOpaque m dataName 0 dataArgs (m :< WeakTermTau) consType
  introRuleList <- mapM (parseDefineDataConstructor dataName dataArgs) $ zip consInfoList' [0 ..]
  return $ formRule : introRuleList

modifyConstructorName :: T.Text -> (Hint, T.Text, [BinderF WeakTerm]) -> IO (Hint, T.Text, [BinderF WeakTerm])
modifyConstructorName dataName (mb, b, yts) = do
  return (mb, dataName <> nsSep <> b, yts)

parseDefineDataConstructor :: T.Text -> [BinderF WeakTerm] -> ((Hint, T.Text, [BinderF WeakTerm]), Integer) -> IO WeakStmt
parseDefineDataConstructor dataName dataArgs ((m, consName, consArgs), consNumber) = do
  let dataConsArgs = dataArgs ++ consArgs
  let consArgs' = map identPlusToVar consArgs
  let dataType = constructDataType m dataName dataArgs
  defineFunction
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

parseDefineDataClause :: Parser (Hint, T.Text, [BinderF WeakTerm])
parseDefineDataClause = do
  m <- currentHint
  b <- symbol
  yts <- argList parseDefineDataClauseArg
  return (m, b, yts)

parseDefineDataClauseArg :: Parser (BinderF WeakTerm)
parseDefineDataClauseArg = do
  m <- currentHint
  choice
    [ try weakAscription,
      weakTermToWeakIdent m weakTerm
    ]

parseDefineCodata :: Parser [WeakStmt]
parseDefineCodata = do
  m <- currentHint
  try $ keyword "define-codata"
  dataName <- var >>= liftIO . attachSectionPrefix . snd
  dataArgs <- argList weakAscription
  elemInfoList <- asBlock $ manyList weakAscription
  formRule <- liftIO $ defineData m dataName dataArgs [(m, "new", elemInfoList)]
  elimRuleList <- liftIO $ mapM (parseDefineCodataElim dataName dataArgs elemInfoList) elemInfoList
  return $ formRule ++ elimRuleList

parseDefineCodataElim :: T.Text -> [BinderF WeakTerm] -> [BinderF WeakTerm] -> BinderF WeakTerm -> IO WeakStmt
parseDefineCodataElim dataName dataArgs elemInfoList (m, elemName, elemType) = do
  let codataType = constructDataType m dataName dataArgs
  recordVarText <- newText
  let projArgs = dataArgs ++ [(m, Ident.fromText recordVarText, codataType)]
  let elemName' = dataName <> nsSep <> Ident.toText elemName
  defineFunction
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

parseDefineResource :: Parser WeakStmt
parseDefineResource = do
  m <- currentHint
  try $ keyword "define-resource"
  name <- snd <$> var
  asBlock $ do
    discarder <- delimiter "-" >> weakTerm
    copier <- delimiter "-" >> weakTerm
    liftIO $ registerTopLevelName m name
    liftIO $ modifyIORef' resourceTypeSetRef $ S.insert name
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

registerTopLevelName :: Hint -> T.Text -> IO ()
registerTopLevelName m x = do
  topNameSet <- readIORef topNameSetRef
  when (S.member x topNameSet) $
    raiseError m $ "the variable `" <> x <> "` is already defined at the top level"
  modifyIORef' topNameSetRef $ S.insert x
  isPrivate <- checkIfPrivate
  when isPrivate $
    modifyIORef' privateNameSetRef $ S.insert x

checkIfPrivate :: IO Bool
checkIfPrivate = do
  isPrivateStack <- readIORef isPrivateStackRef
  if null isPrivateStack
    then return False
    else return $ head isPrivateStack

{-# NOINLINE isPrivateStackRef #-}
isPrivateStackRef :: IORef [Bool]
isPrivateStackRef =
  unsafePerformIO (newIORef [])

{-# NOINLINE privateNameSetRef #-}
privateNameSetRef :: IORef (S.Set T.Text)
privateNameSetRef =
  unsafePerformIO (newIORef S.empty)

weakTermToWeakIdent :: Hint -> Parser WeakTerm -> Parser (BinderF WeakTerm)
weakTermToWeakIdent m f = do
  a <- f
  h <- liftIO $ newTextualIdentFromText "_"
  return (m, h, a)
