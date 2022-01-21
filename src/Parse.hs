module Parse
  ( parseMain,
    parseOther,
  )
where

import Control.Comonad.Cofree (Cofree (..))
import Control.Monad (forM_, when)
import Data.Basic (AliasInfo (..), BinderF, Hint, LamKindF (LamKindCons), Opacity (..), asIdent, asText)
import Data.Global
  ( constructorEnvRef,
    currentGlobalLocatorRef,
    dataEnvRef,
    getCurrentFilePath,
    globalLocatorListRef,
    impArgEnvRef,
    localLocatorListRef,
    moduleAliasMapRef,
    newText,
    nsSep,
    resourceTypeSetRef,
    sourceAliasMapRef,
    topNameSetRef,
  )
import qualified Data.HashMap.Lazy as Map
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Log (raiseCritical', raiseError)
import Data.Module (defaultModulePrefix, getChecksumAliasList)
import Data.Namespace
  ( activateGlobalLocator,
    activateLocalLocator,
    attachSectionPrefix,
    handleDefinePrefix,
  )
import qualified Data.Set as S
import Data.Source (Source (..), getDomain, getLocator)
import Data.Stmt
  ( EnumInfo,
    Stmt,
    WeakStmt (..),
    extractName,
    loadCache,
  )
import qualified Data.Text as T
import Data.WeakTerm
  ( WeakTerm,
    WeakTermF
      ( WeakTermMatch,
        WeakTermPi,
        WeakTermPiElim,
        WeakTermPiIntro,
        WeakTermTau,
        WeakTermVar,
        WeakTermVarGlobal
      ),
  )
import Parse.Core (currentHint, initializeParserForFile, isSymbolChar, lookAhead, parseArgList, parseAsBlock, parseByPredicate, parseDefiniteDescription, parseManyList, parseSymbol, parseToken, parseVarText, raiseParseError, skip, takeN, textRef, tryPlanList, weakTermToWeakIdent, weakVar)
import Parse.Discern (discernStmtList)
import Parse.Enum (insEnumEnv, parseDefineEnum)
import Parse.Import (skipImportSequence)
import Parse.WeakTerm
  ( parseTopDefInfo,
    weakAscription,
    weakTerm,
  )
import System.IO.Unsafe (unsafePerformIO)

--
-- core functions
--

parseMain :: T.Text -> Source -> IO (Either [Stmt] ([WeakStmt], [EnumInfo]))
parseMain mainFunctionName source = do
  result <- parseSource source
  ensureMain mainFunctionName
  return result

parseOther :: Source -> IO (Either [Stmt] ([WeakStmt], [EnumInfo]))
parseOther =
  parseSource

parseSource :: Source -> IO (Either [Stmt] ([WeakStmt], [EnumInfo]))
parseSource source = do
  mCache <- loadCache source
  modifyIORef' isPrivateRef $ const True
  initializeNamespace source
  setupSectionPrefix source
  case mCache of
    Just (stmtList, enumInfoList) -> do
      forM_ enumInfoList $ \(mEnum, name, itemList) ->
        insEnumEnv mEnum name itemList
      let names = S.fromList $ map extractName stmtList
      modifyIORef' topNameSetRef $ S.union names
      return $ Left stmtList
    Nothing -> do
      let path = sourceFilePath source
      initializeParserForFile path
      skip
      arrangeNamespace
      (defList, enumInfoList) <- parseHeader
      privateNameSet <- readIORef privateNameSetRef
      modifyIORef' topNameSetRef $ S.filter (`S.notMember` privateNameSet)
      return $ Right (defList, enumInfoList)

ensureMain :: T.Text -> IO ()
ensureMain mainFunctionName = do
  m <- currentHint
  topNameSet <- readIORef topNameSetRef
  currentGlobalLocator <- readIORef currentGlobalLocatorRef
  if S.member mainFunctionName topNameSet
    then return ()
    else raiseError m $ "`main` is missing in `" <> currentGlobalLocator <> "`"

setupSectionPrefix :: Source -> IO ()
setupSectionPrefix currentSource = do
  locator <- getLocator currentSource
  activateGlobalLocator locator
  writeIORef currentGlobalLocatorRef locator

parseHeaderBase :: IO ([WeakStmt], [EnumInfo]) -> IO ([WeakStmt], [EnumInfo])
parseHeaderBase action = do
  text <- readIORef textRef
  if T.null text
    then return ([], [])
    else action

parseHeader :: IO ([WeakStmt], [EnumInfo])
parseHeader = do
  parseHeaderBase $ do
    skipImportSequence
    parseHeader'

arrangeNamespace :: IO ()
arrangeNamespace = do
  sourceAliasMap <- readIORef sourceAliasMapRef
  currentFilePath <- getCurrentFilePath
  case Map.lookup currentFilePath sourceAliasMap of
    Nothing ->
      raiseCritical' "[arrangeNamespace] (compiler bug)"
    Just aliasInfoList ->
      mapM_ arrangeNamespace' aliasInfoList

arrangeNamespace' :: AliasInfo -> IO ()
arrangeNamespace' aliasInfo =
  case aliasInfo of
    AliasInfoUse locator ->
      activateGlobalLocator locator
    AliasInfoPrefix m from to ->
      handleDefinePrefix m from to

parseHeader' :: IO ([WeakStmt], [EnumInfo])
parseHeader' =
  parseHeaderBase $ do
    headSymbol <- lookAhead (parseByPredicate isSymbolChar)
    case headSymbol of
      Just "define-enum" -> do
        enumInfo <- parseDefineEnum
        (defList, enumInfoList) <- parseHeader'
        return (defList, enumInfo : enumInfoList)
      Just "define-prefix" -> do
        parseDefinePrefix
        parseHeader'
      Just "use" -> do
        parseStmtUse
        parseHeader'
      _ -> do
        stmtList <- parseStmtList >>= discernStmtList
        return (stmtList, [])

parseStmtList :: IO [WeakStmt]
parseStmtList = do
  text <- readIORef textRef
  if T.null text
    then return []
    else do
      headSymbol <- lookAhead (parseByPredicate isSymbolChar)
      case headSymbol of
        Just "define" -> do
          def <- parseDefine OpacityOpaque
          stmtList <- parseStmtList
          return $ def : stmtList
        Just "define-inline" -> do
          def <- parseDefine OpacityTransparent
          stmtList <- parseStmtList
          return $ def : stmtList
        Just "define-data" -> do
          stmtList1 <- parseDefineData
          stmtList2 <- parseStmtList
          return $ stmtList1 ++ stmtList2
        Just "define-codata" -> do
          stmtList1 <- parseDefineCodata
          stmtList2 <- parseStmtList
          return $ stmtList1 ++ stmtList2
        Just "define-resource" -> do
          def <- parseDefineResource
          stmtList <- parseStmtList
          return $ def : stmtList
        Just "public" -> do
          parsePublic
          parseStmtList
        Just "private" -> do
          parsePrivate
          parseStmtList
        Just x -> do
          m <- currentHint
          raiseParseError m $ "invalid statement: " <> x
        Nothing -> do
          m <- currentHint
          raiseParseError m "found the empty symbol when expecting a statement"

--
-- parser for statements
--

parsePublic :: IO ()
parsePublic = do
  parseToken "public"
  modifyIORef' isPrivateRef $ const False

parsePrivate :: IO ()
parsePrivate = do
  parseToken "private"
  modifyIORef' isPrivateRef $ const True

-- define name (x1 : A1) ... (xn : An) : A = e
parseDefine :: Opacity -> IO WeakStmt
parseDefine opacity = do
  m <- currentHint
  case opacity of
    OpacityOpaque ->
      parseToken "define"
    OpacityTransparent ->
      parseToken "define-inline"
  ((_, name), impArgs, expArgs, codType, e) <- parseTopDefInfo
  name' <- attachSectionPrefix name
  modifyIORef' impArgEnvRef $ Map.insert name' (length impArgs)
  defineFunction opacity m name' (impArgs ++ expArgs) codType e

defineFunction :: Opacity -> Hint -> T.Text -> [BinderF WeakTerm] -> WeakTerm -> WeakTerm -> IO WeakStmt
defineFunction opacity m name binder codType e = do
  registerTopLevelName m name
  return $ WeakStmtDefine opacity m name binder codType e

parseStmtUse :: IO ()
parseStmtUse = do
  parseToken "use"
  (_, name) <- parseDefiniteDescription
  activateLocalLocator name

parseDefinePrefix :: IO ()
parseDefinePrefix = do
  m <- currentHint
  parseToken "define-prefix"
  from <- parseVarText
  parseToken "="
  to <- parseVarText
  handleDefinePrefix m from to

parseDefineData :: IO [WeakStmt]
parseDefineData = do
  m <- currentHint
  parseToken "define-data"
  a <- parseVarText >>= attachSectionPrefix
  dataArgs <- parseArgList weakAscription
  consInfoList <- parseAsBlock $ parseManyList parseDefineDataClause
  defineData m a dataArgs consInfoList

defineData :: Hint -> T.Text -> [BinderF WeakTerm] -> [(Hint, T.Text, [BinderF WeakTerm])] -> IO [WeakStmt]
defineData m dataName dataArgs consInfoList = do
  consInfoList' <- mapM (modifyConstructorName dataName) consInfoList
  setAsData dataName (length dataArgs) consInfoList'
  let consType = m :< WeakTermPi [] (m :< WeakTermTau)
  formRule <- defineFunction OpacityOpaque m dataName dataArgs (m :< WeakTermTau) consType
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
  modifyIORef' impArgEnvRef $ Map.insert consName (length dataArgs)
  defineFunction
    OpacityTransparent
    m
    consName
    dataConsArgs
    dataType
    $ m
      :< WeakTermPiIntro
        (LamKindCons dataName consName consNumber dataType)
        [ (m, asIdent consName, m :< WeakTermPi consArgs (m :< WeakTermTau))
        ]
        (m :< WeakTermPiElim (weakVar m consName) consArgs')

constructDataType :: Hint -> T.Text -> [BinderF WeakTerm] -> WeakTerm
constructDataType m dataName dataArgs =
  m :< WeakTermPiElim (m :< WeakTermVarGlobal dataName) (map identPlusToVar dataArgs)

parseDefineDataClause :: IO (Hint, T.Text, [BinderF WeakTerm])
parseDefineDataClause = do
  m <- currentHint
  b <- parseSymbol
  yts <- parseArgList parseDefineDataClauseArg
  return (m, b, yts)

parseDefineDataClauseArg :: IO (BinderF WeakTerm)
parseDefineDataClauseArg = do
  m <- currentHint
  tryPlanList
    [weakAscription]
    (weakTermToWeakIdent m weakTerm)

parseDefineCodata :: IO [WeakStmt]
parseDefineCodata = do
  m <- currentHint
  parseToken "define-codata"
  dataName <- parseVarText >>= attachSectionPrefix
  dataArgs <- parseArgList weakAscription
  elemInfoList <- parseAsBlock $ parseManyList weakAscription
  formRule <- defineData m dataName dataArgs [(m, "new", elemInfoList)]
  -- formRule <- defineData m dataName dataArgs [(m, dataName <> ":" <> "new", elemInfoList)]
  elimRuleList <- mapM (parseDefineCodataElim dataName dataArgs elemInfoList) elemInfoList
  return $ formRule ++ elimRuleList

parseDefineCodataElim :: T.Text -> [BinderF WeakTerm] -> [BinderF WeakTerm] -> BinderF WeakTerm -> IO WeakStmt
parseDefineCodataElim dataName dataArgs elemInfoList (m, elemName, elemType) = do
  let codataType = constructDataType m dataName dataArgs
  recordVarText <- newText
  let projArgs = dataArgs ++ [(m, asIdent recordVarText, codataType)]
  elemName' <- attachSectionPrefix $ dataName <> nsSep <> asText elemName
  defineFunction
    OpacityOpaque
    m
    elemName'
    projArgs
    elemType
    $ m
      :< WeakTermMatch
        Nothing
        (weakVar m recordVarText, codataType)
        [((m, dataName <> nsSep <> "new", elemInfoList), weakVar m (asText elemName))]

parseDefineResource :: IO WeakStmt
parseDefineResource = do
  m <- currentHint
  _ <- parseToken "define-resource"
  name <- parseVarText >>= attachSectionPrefix
  [discarder, copier] <- parseAsBlock $ takeN 2 $ parseToken "-" >> weakTerm
  registerTopLevelName m name
  modifyIORef' resourceTypeSetRef $ S.insert name
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
  isPrivate <- readIORef isPrivateRef
  when isPrivate $
    modifyIORef' privateNameSetRef $ S.insert x

initializeNamespace :: Source -> IO ()
initializeNamespace source = do
  additionalChecksumAlias <- getAdditionalChecksumAlias source
  writeIORef moduleAliasMapRef $ Map.fromList $ additionalChecksumAlias ++ getChecksumAliasList (sourceModule source)
  writeIORef globalLocatorListRef []
  writeIORef localLocatorListRef []

getAdditionalChecksumAlias :: Source -> IO [(T.Text, T.Text)]
getAdditionalChecksumAlias source = do
  domain <- getDomain $ sourceModule source
  if defaultModulePrefix == domain
    then return []
    else return [(defaultModulePrefix, domain)]

{-# NOINLINE isPrivateRef #-}
isPrivateRef :: IORef Bool
isPrivateRef =
  unsafePerformIO (newIORef True)

{-# NOINLINE privateNameSetRef #-}
privateNameSetRef :: IORef (S.Set T.Text)
privateNameSetRef =
  unsafePerformIO (newIORef S.empty)
