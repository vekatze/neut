module Parse
  ( parseMain,
    parseOther,
  )
where

import Control.Comonad.Cofree (Cofree (..))
import Control.Monad (forM_, when)
import Data.Basic (AliasInfo (..), EnumCaseF (EnumCaseLabel), Hint, Ident, IsReducible, LamKind (LamKindCons, LamKindFix, LamKindResourceHandler), Opacity (OpacityTranslucent, OpacityTransparent), asIdent, asText)
import Data.Global
  ( aliasEnvRef,
    boolFalse,
    boolTrue,
    constructorEnvRef,
    currentSectionRef,
    dataEnvRef,
    getCurrentFilePath,
    initialPrefixEnv,
    newText,
    nsSep,
    prefixEnvRef,
    sourceAliasMapRef,
    topNameSetRef,
  )
import qualified Data.HashMap.Lazy as Map
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.Log (raiseCritical', raiseError)
import Data.Module (getChecksumAliasList)
import Data.Namespace
  ( attachSectionPrefix,
    handleDefinePrefix,
    handleUse,
  )
import qualified Data.Set as S
import Data.Source (Source (..), getSection)
import Data.Stmt
  ( EnumInfo,
    Stmt (StmtDef),
    WeakStmt (..),
    loadCache,
  )
import qualified Data.Text as T
import Data.WeakTerm
  ( WeakBinder,
    WeakTerm,
    WeakTermF
      ( WeakTermCase,
        WeakTermEnumElim,
        WeakTermPi,
        WeakTermPiElim,
        WeakTermPiIntro,
        WeakTermTau,
        WeakTermVar
      ),
  )
import Parse.Core
  ( currentHint,
    initializeParserForFile,
    isSymbolChar,
    lookAhead,
    many,
    newTextualIdentFromText,
    raiseParseError,
    skip,
    symbol,
    symbolMaybe,
    textRef,
    token,
    tryPlanList,
    var,
    varText,
    weakTermToWeakIdent,
    weakVar,
  )
import Parse.Discern (discernStmtList)
import Parse.Enum (insEnumEnv, parseDefineEnum)
import Parse.Import (skipImportSequence)
import Parse.WeakTerm
  ( ascriptionInner,
    weakAscription,
    weakBinder,
    weakTerm,
    weakTermSimple,
  )

--
-- core functions
--

parseMain :: T.Text -> Source -> IO (Either [Stmt] ([WeakStmt], [EnumInfo]))
parseMain mainFunctionName source = do
  result <- parse' source
  ensureMain mainFunctionName
  return result

parseOther :: Source -> IO (Either [Stmt] ([WeakStmt], [EnumInfo]))
parseOther =
  parse'

parse' :: Source -> IO (Either [Stmt] ([WeakStmt], [EnumInfo]))
parse' source = do
  m <- currentHint
  mCache <- loadCache m source
  setupSectionPrefix source
  case mCache of
    Just (stmtList, enumInfoList) -> do
      forM_ enumInfoList $ \(mEnum, name, itemList) -> do
        insEnumEnv mEnum name itemList
      let names = S.fromList $ map (\(StmtDef _ _ x _ _) -> x) stmtList
      modifyIORef' topNameSetRef $ S.union names
      return $ Left stmtList
    Nothing -> do
      let path = sourceFilePath source
      initializeParserForFile path
      initializeNamespace source
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
          def <- parseDefine False
          stmtList <- parseStmtList
          return $ def : stmtList
        Just "define-inline" -> do
          def <- parseDefine True
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
parseDefine :: Bool -> IO WeakStmt
parseDefine isReducible = do
  m <- currentHint
  if isReducible
    then token "define-inline"
    else token "define"
  (mTerm, name) <- var
  name' <- attachSectionPrefix name
  argList <- many weakBinder
  token ":"
  codType <- weakTerm
  token "="
  e <- weakTerm
  case argList of
    [] ->
      defineTerm isReducible m name' codType e
    _ ->
      defineFunction isReducible m mTerm name' argList codType e

defineFunction :: IsReducible -> Hint -> Hint -> T.Text -> [WeakBinder] -> WeakTerm -> WeakTerm -> IO WeakStmt
defineFunction isReducible m mFun name argList codType e = do
  let piType = m :< WeakTermPi argList codType
  let e' = m :< WeakTermPiIntro OpacityTranslucent (LamKindFix (mFun, asIdent name, piType)) argList e
  defineTerm isReducible m name piType e'

defineTerm :: IsReducible -> Hint -> T.Text -> WeakTerm -> WeakTerm -> IO WeakStmt
defineTerm isReducible m name codType e = do
  registerTopLevelName m name
  return $ WeakStmtDef isReducible m name codType e

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
  mFun <- currentHint
  a <- varText >>= attachSectionPrefix
  xts <- many weakAscription
  bts <- many parseDefineDataClause
  defineData m mFun a xts bts

defineData :: Hint -> Hint -> T.Text -> [WeakBinder] -> [(Hint, T.Text, [WeakBinder])] -> IO [WeakStmt]
defineData m mFun a xts bts = do
  setAsData a (length xts) bts
  z <- newTextualIdentFromText "cod"
  let lamArgs = (m, z, m :< WeakTermTau) : map (toPiTypeWith z) bts
  let baseType = m :< WeakTermPi lamArgs (m :< WeakTermVar z)
  case xts of
    [] -> do
      registerTopLevelName m a
      let formRule = WeakStmtDef False m a (m :< WeakTermTau) (m :< WeakTermPi [] (m :< WeakTermTau)) -- fake type
      introRuleList <- mapM (parseDefineDataConstructor m lamArgs baseType a xts) bts
      return $ formRule : introRuleList
    _ -> do
      formRule <- defineFunction False m mFun a xts (m :< WeakTermTau) baseType
      introRuleList <- mapM (parseDefineDataConstructor m lamArgs baseType a xts) bts
      return $ formRule : introRuleList

parseDefineDataConstructor :: Hint -> [WeakBinder] -> WeakTerm -> T.Text -> [WeakBinder] -> (Hint, T.Text, [WeakBinder]) -> IO WeakStmt
parseDefineDataConstructor m lamArgs baseType a xts (mb, b, yts) = do
  let consArgs = xts ++ yts
  let args = map identPlusToVar yts
  let b' = a <> nsSep <> b
  let indType =
        case xts of
          [] ->
            weakVar m a
          _ ->
            m :< WeakTermPiElim (weakVar m a) (map identPlusToVar xts)
  case consArgs of
    [] ->
      defineTerm
        True
        m
        b'
        indType
        ( m
            :< WeakTermPiElim
              (weakVar m "unsafe.cast")
              [ baseType,
                indType,
                m
                  :< WeakTermPiIntro
                    OpacityTransparent
                    (LamKindCons a b')
                    lamArgs
                    (m :< WeakTermPiElim (weakVar m b) args)
              ]
        )
    _ ->
      defineFunction
        True
        m
        mb
        b'
        consArgs
        indType
        ( m
            :< WeakTermPiElim
              (weakVar m "unsafe.cast")
              [ baseType,
                indType,
                m
                  :< WeakTermPiIntro
                    OpacityTransparent
                    (LamKindCons a b')
                    lamArgs
                    (m :< WeakTermPiElim (weakVar m b) args)
              ]
        )

parseDefineDataClause :: IO (Hint, T.Text, [WeakBinder])
parseDefineDataClause = do
  token "-"
  m <- currentHint
  b <- symbol
  yts <- many parseDefineDataClauseArg
  return (m, b, yts)

parseDefineDataClauseArg :: IO WeakBinder
parseDefineDataClauseArg = do
  m <- currentHint
  tryPlanList
    [ weakAscription,
      weakTermToWeakIdent m weakTermSimple
    ]

parseDefineCodata :: IO [WeakStmt]
parseDefineCodata = do
  m <- currentHint
  token "define-codata"
  mFun <- currentHint
  a <- varText >>= attachSectionPrefix
  xts <- many weakAscription
  yts <- many (token "-" >> ascriptionInner)
  formRule <- defineData m mFun a xts [(m, "new", yts)]
  elimRuleList <- mapM (parseDefineCodataElim m a xts yts) yts
  return $ formRule ++ elimRuleList

parseDefineCodataElim :: Hint -> T.Text -> [WeakBinder] -> [WeakBinder] -> WeakBinder -> IO WeakStmt
parseDefineCodataElim m a xts yts (mY, y, elemType) = do
  let codataType =
        case xts of
          [] ->
            weakVar m a
          _ ->
            m :< WeakTermPiElim (weakVar m a) (map identPlusToVar xts)
  recordVarText <- newText
  let projArgs = xts ++ [(m, asIdent recordVarText, codataType)]
  defineFunction
    True
    m
    mY
    (a <> nsSep <> asText y)
    projArgs
    elemType
    ( m
        :< WeakTermCase
          elemType
          Nothing
          (weakVar m recordVarText, codataType)
          [((m, a <> nsSep <> "new", yts), weakVar m (asText y))]
    )

parseDefineResourceType :: IO WeakStmt
parseDefineResourceType = do
  m <- currentHint
  _ <- token "define-resource-type"
  name <- varText >>= attachSectionPrefix
  discarder <- weakTermSimple
  copier <- weakTermSimple
  flag <- newTextualIdentFromText "flag"
  value <- newTextualIdentFromText "value"
  defineTerm
    True
    m
    name
    (m :< WeakTermTau)
    ( m
        :< WeakTermPiElim
          (weakVar m "unsafe.cast")
          [ m
              :< WeakTermPi
                [ (m, flag, weakVar m "bool"),
                  (m, value, weakVar m "unsafe.pointer")
                ]
                (weakVar m "unsafe.pointer"),
            m :< WeakTermTau,
            m
              :< WeakTermPiIntro
                OpacityTransparent
                LamKindResourceHandler
                [ (m, flag, weakVar m "bool"),
                  (m, value, weakVar m "unsafe.pointer")
                ]
                ( m
                    :< WeakTermEnumElim
                      (weakVar m (asText flag), weakVar m "bool")
                      [ ( m :< EnumCaseLabel boolTrue,
                          m :< WeakTermPiElim copier [weakVar m (asText value)]
                        ),
                        ( m :< EnumCaseLabel boolFalse,
                          m
                            :< WeakTermPiElim
                              (weakVar m "unsafe.cast")
                              [ weakVar m "top",
                                weakVar m "unsafe.pointer",
                                m :< WeakTermPiElim discarder [weakVar m (asText value)]
                              ]
                        )
                      ]
                )
          ]
    )

setAsData :: T.Text -> Int -> [(Hint, T.Text, [WeakBinder])] -> IO ()
setAsData a i bts = do
  let bs = map (\(_, b, _) -> a <> nsSep <> b) bts
  modifyIORef' dataEnvRef $ Map.insert a bs
  forM_ (zip bs [0 ..]) $ \(x, k) ->
    modifyIORef' constructorEnvRef $ Map.insert x (i, k)

toPiTypeWith :: Ident -> (Hint, T.Text, [WeakBinder]) -> WeakBinder
toPiTypeWith cod (m, b, yts) =
  (m, asIdent b, m :< WeakTermPi yts (m :< WeakTermVar cod))

identPlusToVar :: WeakBinder -> WeakTerm
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
  writeIORef aliasEnvRef $ getChecksumAliasList $ sourceModule source
  writeIORef prefixEnvRef initialPrefixEnv
