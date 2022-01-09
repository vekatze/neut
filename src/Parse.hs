module Parse
  ( parseMain,
    parseOther,
  )
where

import Control.Comonad.Cofree (Cofree (..))
import Control.Monad (forM_, when)
import Data.Basic (AliasInfo (..), BinderF, Hint, LamKindF (LamKindCons), Opacity (..), asIdent, asText)
import Data.Global
  ( aliasEnvRef,
    constructorEnvRef,
    currentSectionRef,
    dataEnvRef,
    getCurrentFilePath,
    newText,
    prefixEnvRef,
    sourceAliasMapRef,
    topNameSetRef,
  )
import qualified Data.HashMap.Lazy as Map
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.Log (raiseCritical', raiseError)
import Data.Module (defaultModulePrefix, getChecksumAliasList)
import Data.Namespace
  ( attachSectionPrefix,
    handleDefinePrefix,
    handleUse,
  )
import qualified Data.Set as S
import Data.Source (Source (..), getDomain, getSection)
import Data.Stmt
  ( EnumInfo,
    Stmt (StmtDefine),
    WeakStmt (..),
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
        WeakTermVar
      ),
  )
import Parse.Core (argList, asBlock, char, currentHint, initializeParserForFile, isSymbolChar, lookAhead, manyList, newTextualIdentFromText, raiseParseError, simpleVar, skip, symbol, symbolMaybe, textRef, token, tryPlanList, varText, weakTermToWeakIdent, weakVar)
import Parse.Discern (discernStmtList)
import Parse.Enum (insEnumEnv, parseDefineEnum)
import Parse.Import (skipImportSequence)
import Parse.WeakTerm
  ( parseDefInfo,
    weakAscription,
    weakTerm,
    weakTermSimple,
  )

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
  initializeNamespace source
  setupSectionPrefix source
  case mCache of
    Just (stmtList, enumInfoList) -> do
      forM_ enumInfoList $ \(mEnum, name, itemList) ->
        insEnumEnv mEnum name itemList
      let names = S.fromList $ map (\(StmtDefine _ _ x _ _ _) -> x) stmtList
      modifyIORef' topNameSetRef $ S.union names
      return $ Left stmtList
    Nothing -> do
      let path = sourceFilePath source
      initializeParserForFile path
      skip
      arrangeNamespace
      (defList, enumInfoList) <- parseHeader
      return $ Right (defList, enumInfoList)

ensureMain :: T.Text -> IO ()
ensureMain mainFunctionName = do
  m <- currentHint
  topNameSet <- readIORef topNameSetRef
  currentSection <- readIORef currentSectionRef
  if S.member mainFunctionName topNameSet
    then return ()
    else raiseError m $ "`main` is missing in `" <> currentSection <> "`"

setupSectionPrefix :: Source -> IO ()
setupSectionPrefix currentSource = do
  section <- getSection currentSource
  handleUse section
  writeIORef currentSectionRef section

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
    AliasInfoUse namespace ->
      handleUse namespace
    AliasInfoPrefix from to ->
      handleDefinePrefix from to

parseHeader' :: IO ([WeakStmt], [EnumInfo])
parseHeader' =
  parseHeaderBase $ do
    headSymbol <- lookAhead (symbolMaybe isSymbolChar)
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
      headSymbol <- lookAhead (symbolMaybe isSymbolChar)
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
        Just "define-resource-type" -> do
          def <- parseDefineResourceType
          stmtList <- parseStmtList
          return $ def : stmtList
        Just x -> do
          m <- currentHint
          raiseParseError m $ "invalid statement: " <> x
        Nothing -> do
          m <- currentHint
          raiseParseError m "found the empty symbol when expecting a statement"

--
-- parser for statements
--

-- define name (x1 : A1) ... (xn : An) : A = e
parseDefine :: Opacity -> IO WeakStmt
parseDefine opacity = do
  m <- currentHint
  case opacity of
    OpacityOpaque ->
      token "define"
    OpacityTransparent ->
      token "define-inline"
  ((_, name), domBinderList, codType, e) <- parseDefInfo
  name' <- attachSectionPrefix name
  defineFunction opacity m name' domBinderList codType e

defineFunction :: Opacity -> Hint -> T.Text -> [BinderF WeakTerm] -> WeakTerm -> WeakTerm -> IO WeakStmt
defineFunction opacity m name binder codType e = do
  registerTopLevelName m name
  return $ WeakStmtDefine opacity m name binder codType e

parseStmtUse :: IO ()
parseStmtUse = do
  token "use"
  name <- varText
  handleUse name

parseDefinePrefix :: IO ()
parseDefinePrefix = do
  token "define-prefix"
  from <- varText
  token "="
  to <- varText
  handleDefinePrefix from to

parseDefineData :: IO [WeakStmt]
parseDefineData = do
  m <- currentHint
  token "define-data"
  a <- varText >>= attachSectionPrefix
  dataArgs <- argList weakAscription
  consInfoList <- asBlock $ manyList parseDefineDataClause
  defineData m a dataArgs consInfoList

defineData :: Hint -> T.Text -> [BinderF WeakTerm] -> [(Hint, T.Text, [BinderF WeakTerm])] -> IO [WeakStmt]
defineData m a dataArgs consInfoList = do
  consInfoList' <- mapM modifyConstructorName consInfoList
  setAsData a (length dataArgs) consInfoList'
  let consType = m :< WeakTermPi [] (m :< WeakTermTau)
  formRule <- defineFunction OpacityOpaque m a dataArgs (m :< WeakTermTau) consType
  introRuleList <- mapM (parseDefineDataConstructor a dataArgs) $ zip consInfoList' [0 ..]
  return $ formRule : introRuleList

modifyConstructorName :: (Hint, T.Text, [BinderF WeakTerm]) -> IO (Hint, T.Text, [BinderF WeakTerm])
modifyConstructorName (mb, b, yts) = do
  b' <- attachSectionPrefix b
  return (mb, b', yts)

parseDefineDataConstructor :: T.Text -> [BinderF WeakTerm] -> ((Hint, T.Text, [BinderF WeakTerm]), Integer) -> IO WeakStmt
parseDefineDataConstructor dataName dataArgs ((m, consName, consArgs), consNumber) = do
  let dataConsArgs = dataArgs ++ consArgs
  let consArgs' = map identPlusToVar consArgs
  let dataType = constructDataType m dataName dataArgs
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
  m :< WeakTermPiElim (weakVar m dataName) (map identPlusToVar dataArgs)

parseDefineDataClause :: IO (Hint, T.Text, [BinderF WeakTerm])
parseDefineDataClause = do
  -- token "-"
  m <- currentHint
  b <- symbol
  yts <- argList parseDefineDataClauseArg
  return (m, b, yts)

parseDefineDataClauseArg :: IO (BinderF WeakTerm)
parseDefineDataClauseArg = do
  m <- currentHint
  tryPlanList
    [ weakAscription,
      weakTermToWeakIdent m weakTerm
    ]

parseDefineCodata :: IO [WeakStmt]
parseDefineCodata = do
  m <- currentHint
  token "define-codata"
  dataName <- varText >>= attachSectionPrefix
  dataArgs <- argList weakAscription
  elemInfoList <- asBlock $ manyList weakAscription
  formRule <- defineData m dataName dataArgs [(m, dataName <> ":" <> "new", elemInfoList)]
  elimRuleList <- mapM (parseDefineCodataElim dataName dataArgs elemInfoList) elemInfoList
  return $ formRule ++ elimRuleList

parseDefineCodataElim :: T.Text -> [BinderF WeakTerm] -> [BinderF WeakTerm] -> BinderF WeakTerm -> IO WeakStmt
parseDefineCodataElim dataName dataArgs elemInfoList (m, elemName, elemType) = do
  let codataType = constructDataType m dataName dataArgs
  recordVarText <- newText
  let projArgs = dataArgs ++ [(m, asIdent recordVarText, codataType)]
  elemName' <- attachSectionPrefix $ asText elemName
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
        [((m, dataName <> ":" <> "new", elemInfoList), weakVar m (asText elemName))]

parseDefineResourceType :: IO WeakStmt
parseDefineResourceType = do
  -- m <- currentHint
  _ <- token "define-resource-type"
  -- name <- varText >>= attachSectionPrefix
  -- discarder <- weakTermSimple
  -- copier <- weakTermSimple
  -- flag <- newTextualIdentFromText "flag"
  -- value <- newTextualIdentFromText "value"
  undefined

-- defineTerm
--   OpacityOpaque
--   m
--   name
--   (m :< WeakTermTau)
--   $ m
--     :< WeakTermPiIntro
--       LamKindResourceHandler
--       [ (m, flag, weakVar m bool),
--         (m, value, weakVar m unsafePtr)
--       ]
--       ( m
--           :< WeakTermEnumElim
--             (weakVar m (asText flag), weakVar m bool)
--             [ ( m :< EnumCaseLabel boolTrue,
--                 m :< WeakTermPiElim copier [weakVar m (asText value)]
--               ),
--               ( m :< EnumCaseLabel boolFalse,
--                 m
--                   :< WeakTermPiElim
--                     (weakVar m unsafeCast)
--                     [ weakVar m "top",
--                       weakVar m unsafePtr,
--                       m :< WeakTermPiElim discarder [weakVar m (asText value)]
--                     ]
--               )
--             ]
--       )

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

initializeNamespace :: Source -> IO ()
initializeNamespace source = do
  additionalChecksumAlias <- getAdditionalChecksumAlias source
  writeIORef aliasEnvRef $ additionalChecksumAlias ++ getChecksumAliasList (sourceModule source)
  writeIORef prefixEnvRef []

getAdditionalChecksumAlias :: Source -> IO [(T.Text, T.Text)]
getAdditionalChecksumAlias source = do
  domain <- getDomain $ sourceModule source
  if defaultModulePrefix == domain
    then return []
    else return [(defaultModulePrefix, domain)]
