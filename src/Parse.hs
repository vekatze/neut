module Parse
  ( parse,
  )
where

import Control.Monad (forM_, unless, when)
import Data.Basic
  ( EnumCase (EnumCaseLabel),
    Hint,
    Ident,
    IsReducible,
    LamKind (LamKindCons, LamKindFix, LamKindResourceHandler),
    Opacity (OpacityTranslucent, OpacityTransparent),
    asIdent,
    asText,
    getExecPath,
  )
import Data.Global
  ( VisitInfo (VisitInfoActive, VisitInfoFinish),
    aliasEnv,
    constructorEnv,
    dataEnv,
    defaultAliasEnv,
    enumEnv,
    fileEnv,
    getCurrentFilePath,
    getLibraryDirPath,
    initialPrefixEnv,
    isMain,
    mainModuleDirRef,
    newText,
    note',
    popTrace,
    prefixEnv,
    pushTrace,
    revEnumEnv,
    sectionEnv,
    topNameEnv,
  )
import qualified Data.HashMap.Lazy as Map
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.List (find)
import Data.Log (raiseError)
import Data.Module (Source (..), getMainModule, getModuleRootDir)
import Data.Namespace
  ( handleDefinePrefix,
    handleUse,
    nsSep,
    withSectionPrefix,
  )
import qualified Data.Set as S
import Data.Spec (Spec)
import Data.Stmt
  ( EnumInfo,
    HeaderStmtPlus,
    WeakStmt (..),
    WeakStmtPlus,
  )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.WeakTerm
  ( WeakIdentPlus,
    WeakTerm
      ( WeakTermCase,
        WeakTermEnumElim,
        WeakTermPi,
        WeakTermPiElim,
        WeakTermPiIntro,
        WeakTermTau,
        WeakTermVar
      ),
    WeakTermPlus,
  )
import GHC.IO.Handle (Handle, hGetContents)
import Parse.Core
  ( currentHint,
    initializeState,
    integer,
    isSymbolChar,
    lookAhead,
    many,
    newTextualIdentFromText,
    raiseParseError,
    skip,
    string,
    symbol,
    symbolMaybe,
    text,
    token,
    tryPlanList,
    var,
    weakTermToWeakIdent,
    weakVar,
    withNestedState,
  )
import Parse.Discern (discern, discernStmtList)
import Parse.Import (parseImport)
import Parse.Section (pathToSection)
import Parse.Spec (moduleToSpec)
import Parse.WeakTerm
  ( ascriptionInner,
    weakAscription,
    weakIdentPlus,
    weakTerm,
    weakTermSimple,
  )
import Path
  ( Abs,
    Dir,
    File,
    Path,
    parseRelDir,
    toFilePath,
    (</>),
  )
import Path.IO
  ( doesDirExist,
    ensureDir,
    removeDir,
  )
import System.Exit (ExitCode (..))
import System.Process
  ( CreateProcess (cwd, std_err, std_in, std_out),
    StdStream (CreatePipe, UseHandle),
    createProcess,
    proc,
    waitForProcess,
  )

--
-- core functions
--

parse :: Path Abs File -> IO ([HeaderStmtPlus], WeakStmtPlus)
parse mainSourceFilePath = do
  mainSource <- getMainSource mainSourceFilePath
  writeIORef mainModuleDirRef $ getModuleRootDir $ sourceModule mainSource
  setupEnumEnv
  (headerInfo, bodyInfo, _) <- visit mainSource
  _ <- error "finished parsing"
  pushTrace mainSourceFilePath
  ensureMain
  return (headerInfo, bodyInfo)

getMainSource :: Path Abs File -> IO Source
getMainSource mainSourceFilePath = do
  mainModule <- getMainModule
  return $
    Source
      { sourceModule = mainModule,
        sourceFilePath = mainSourceFilePath
      }

ensureMain :: IO ()
ensureMain = do
  flag <- readIORef isMain
  when flag $ do
    m <- currentHint
    _ <- discern (m, WeakTermVar $ asIdent "main")
    return ()

visit :: Source -> IO ([HeaderStmtPlus], WeakStmtPlus, [EnumInfo])
visit source = do
  initializeNamespace
  let path = sourceFilePath source
  pushTrace path
  modifyIORef' fileEnv $ \env -> Map.insert path VisitInfoActive env
  withNestedState $ do
    TIO.readFile (toFilePath path) >>= initializeState
    skip
    m <- currentHint
    spec <- moduleToSpec m (sourceModule source)
    parseHeader spec path

leave :: IO [WeakStmt]
leave = do
  path <- getCurrentFilePath
  modifyIORef' fileEnv $ \env -> Map.insert path VisitInfoFinish env
  popTrace
  return []

parseHeaderBase ::
  Path Abs File ->
  IO ([HeaderStmtPlus], WeakStmtPlus, [EnumInfo]) ->
  IO ([HeaderStmtPlus], WeakStmtPlus, [EnumInfo])
parseHeaderBase currentFilePath action = do
  s <- readIORef text
  if T.null s
    then leave >>= \result -> return ([], (currentFilePath, result), [])
    else action

parseHeader :: Spec -> Path Abs File -> IO ([HeaderStmtPlus], WeakStmtPlus, [EnumInfo])
parseHeader currentSpec currentFilePath = do
  parseHeaderBase currentFilePath $ do
    headSymbol <- lookAhead (symbolMaybe isSymbolChar)
    case headSymbol of
      Just "import" -> do
        defList1 <- parseImport currentSpec visit
        (defList2, main, enumInfoList) <- parseHeader currentSpec currentFilePath
        return (defList1 ++ defList2, main, enumInfoList)
      _ -> do
        parseHeader' currentSpec currentFilePath

setupSectionPrefix :: Spec -> Path Abs File -> IO ()
setupSectionPrefix currentSpec currentFilePath = do
  (moduleName, pathInfo) <- pathToSection currentSpec currentFilePath
  let section = moduleName : pathInfo
  handleUse $ T.intercalate nsSep section
  writeIORef sectionEnv section

parseHeader' :: Spec -> Path Abs File -> IO ([HeaderStmtPlus], WeakStmtPlus, [EnumInfo])
parseHeader' currentSpec currentFilePath = do
  parseHeaderBase currentFilePath $ do
    headSymbol <- lookAhead (symbolMaybe isSymbolChar)
    case headSymbol of
      Just "define-prefix" -> do
        stmtDefinePrefix
        parseHeader' currentSpec currentFilePath
      Just "ensure" -> do
        stmtEnsure
        parseHeader' currentSpec currentFilePath
      Just "use" -> do
        stmtUse
        parseHeader' currentSpec currentFilePath
      _ -> do
        setupSectionPrefix currentSpec currentFilePath
        parseHeader'' currentFilePath

parseHeader'' :: Path Abs File -> IO ([HeaderStmtPlus], WeakStmtPlus, [EnumInfo])
parseHeader'' currentFilePath = do
  parseHeaderBase currentFilePath $ do
    headSymbol <- lookAhead (symbolMaybe isSymbolChar)
    case headSymbol of
      Just "define-enum" -> do
        enumInfo <- stmtDefineEnum
        (parseHeader''Info, bodyInfo, enumInfoList) <- parseHeader'' currentFilePath
        return (parseHeader''Info, bodyInfo, enumInfo : enumInfoList)
      _ -> do
        stmtList <- stmt >>= discernStmtList
        return ([], (currentFilePath, stmtList), [])

stmt :: IO [WeakStmt]
stmt = do
  s <- readIORef text
  if T.null s
    then leave
    else do
      headSymbol <- lookAhead (symbolMaybe isSymbolChar)
      case headSymbol of
        Just "define" -> do
          def <- stmtDefine False
          stmtList <- stmt
          return $ def : stmtList
        Just "define-inline" -> do
          -- fixme: define-reducibleとdefine-inlineは概念として別物ですわよ。
          def <- stmtDefine True
          stmtList <- stmt
          return $ def : stmtList
        Just "define-data" -> do
          stmtList1 <- stmtDefineData
          stmtList2 <- stmt
          return $ stmtList1 ++ stmtList2
        Just "define-codata" -> do
          stmtList1 <- stmtDefineCodata
          stmtList2 <- stmt
          return $ stmtList1 ++ stmtList2
        Just "define-resource-type" -> do
          def <- stmtDefineResourceType
          stmtList <- stmt
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
stmtDefine :: Bool -> IO WeakStmt
stmtDefine isReducible = do
  m <- currentHint
  if isReducible
    then token "define-inline"
    else token "define"
  (mTerm, name) <- var
  name' <- withSectionPrefix name
  argList <- many weakIdentPlus
  token ":"
  codType <- weakTerm
  token "="
  e <- weakTerm
  case argList of
    [] ->
      defineTerm isReducible m name' codType e
    _ ->
      defineFunction isReducible m mTerm name' argList codType e

defineFunction :: IsReducible -> Hint -> Hint -> T.Text -> [WeakIdentPlus] -> WeakTermPlus -> WeakTermPlus -> IO WeakStmt
defineFunction isReducible m mFun name argList codType e = do
  let piType = (m, WeakTermPi argList codType)
  let e' = (m, WeakTermPiIntro OpacityTranslucent (LamKindFix (mFun, asIdent name, piType)) argList e)
  defineTerm isReducible m name piType e'

defineTerm :: IsReducible -> Hint -> T.Text -> WeakTermPlus -> WeakTermPlus -> IO WeakStmt
defineTerm isReducible m name codType e = do
  registerTopLevelName m $ asIdent name
  return $ WeakStmtDef isReducible m name codType e

stmtDefineEnum :: IO EnumInfo
stmtDefineEnum = do
  m <- currentHint
  token "define-enum"
  name <- varText >>= withSectionPrefix
  itemList <- many stmtDefineEnumClause
  let itemList' = arrangeEnumItemList name 0 itemList
  unless (isLinear (map snd itemList')) $
    raiseError m "found a collision of discriminant"
  path <- toFilePath <$> getCurrentFilePath
  insEnumEnv path m name itemList'
  return (m, name, itemList')

arrangeEnumItemList :: T.Text -> Int -> [(T.Text, Maybe Int)] -> [(T.Text, Int)]
arrangeEnumItemList name currentValue clauseList =
  case clauseList of
    [] ->
      []
    (item, Nothing) : rest ->
      (name <> nsSep <> item, currentValue) : arrangeEnumItemList name (currentValue + 1) rest
    (item, Just v) : rest ->
      (name <> nsSep <> item, v) : arrangeEnumItemList name (v + 1) rest

stmtDefineEnumClause :: IO (T.Text, Maybe Int)
stmtDefineEnumClause = do
  tryPlanList
    [ stmtDefineEnumClauseWithDiscriminant,
      stmtDefineEnumClauseWithoutDiscriminant
    ]

stmtDefineEnumClauseWithDiscriminant :: IO (T.Text, Maybe Int)
stmtDefineEnumClauseWithDiscriminant = do
  token "-"
  item <- varText
  token "<-"
  discriminant <- integer
  return (item, Just (fromInteger discriminant))

stmtDefineEnumClauseWithoutDiscriminant :: IO (T.Text, Maybe Int)
stmtDefineEnumClauseWithoutDiscriminant = do
  token "-"
  item <- varText
  return (item, Nothing)

stmtEnsure :: IO ()
stmtEnsure = do
  token "ensure"
  pkgStr <- symbol
  mUrl <- currentHint
  urlStr <- string
  libDirPath <- getLibraryDirPath
  pkgStr' <- parseRelDir $ T.unpack pkgStr
  let pkgStrDirPath = libDirPath </> pkgStr'
  isAlreadyInstalled <- doesDirExist pkgStrDirPath
  unless isAlreadyInstalled $ do
    ensureDir pkgStrDirPath
    let urlStr' = T.unpack urlStr
    let curlCmd = proc "curl" ["-s", "-S", "-L", urlStr']
    let tarCmd = proc "tar" ["xJf", "-", "-C", toFilePath pkgStr', "--strip-components=1"]
    (_, Just stdoutHandler, Just curlErrorHandler, curlHandler) <-
      createProcess curlCmd {cwd = Just (toFilePath libDirPath), std_out = CreatePipe, std_err = CreatePipe}
    (_, _, Just tarErrorHandler, tarHandler) <-
      createProcess tarCmd {cwd = Just (toFilePath libDirPath), std_in = UseHandle stdoutHandler, std_err = CreatePipe}
    note' $ "downloading " <> pkgStr <> " from " <> T.pack urlStr'
    curlExitCode <- waitForProcess curlHandler
    raiseIfFailure mUrl "curl" curlExitCode curlErrorHandler pkgStrDirPath
    note' $ "extracting " <> pkgStr <> " into " <> T.pack (toFilePath pkgStrDirPath)
    tarExitCode <- waitForProcess tarHandler
    raiseIfFailure mUrl "tar" tarExitCode tarErrorHandler pkgStrDirPath

raiseIfFailure :: Hint -> String -> ExitCode -> Handle -> Path Abs Dir -> IO ()
raiseIfFailure m procName exitCode h pkgDirPath =
  case exitCode of
    ExitSuccess ->
      return ()
    ExitFailure i -> do
      removeDir pkgDirPath
      errStr <- hGetContents h
      raiseError m $ T.pack $ "the child process `" ++ procName ++ "` failed with the following message (exitcode = " ++ show i ++ "):\n" ++ errStr

stmtUse :: IO ()
stmtUse = do
  token "use"
  name <- varText
  handleUse name

stmtDefinePrefix :: IO ()
stmtDefinePrefix = do
  token "define-prefix"
  from <- varText
  token "="
  to <- varText
  handleDefinePrefix from to

stmtDefineData :: IO [WeakStmt]
stmtDefineData = do
  m <- currentHint
  token "define-data"
  mFun <- currentHint
  a <- varText >>= withSectionPrefix
  xts <- many weakAscription
  bts <- many stmtDefineDataClause
  defineData m mFun a xts bts

defineData :: Hint -> Hint -> T.Text -> [WeakIdentPlus] -> [(Hint, T.Text, [WeakIdentPlus])] -> IO [WeakStmt]
defineData m mFun a xts bts = do
  setAsData a (length xts) bts
  z <- newTextualIdentFromText "cod"
  let lamArgs = (m, z, (m, WeakTermTau)) : map (toPiTypeWith z) bts
  let baseType = (m, WeakTermPi lamArgs (m, WeakTermVar z))
  case xts of
    [] -> do
      registerTopLevelName m $ asIdent a
      let formRule = WeakStmtDef False m a (m, WeakTermTau) (m, WeakTermPi [] (m, WeakTermTau)) -- fake type
      introRuleList <- mapM (stmtDefineDataConstructor m lamArgs baseType a xts) bts
      return $ formRule : introRuleList
    _ -> do
      formRule <- defineFunction False m mFun a xts (m, WeakTermTau) baseType
      introRuleList <- mapM (stmtDefineDataConstructor m lamArgs baseType a xts) bts
      return $ formRule : introRuleList

stmtDefineDataConstructor :: Hint -> [WeakIdentPlus] -> WeakTermPlus -> T.Text -> [WeakIdentPlus] -> (Hint, T.Text, [WeakIdentPlus]) -> IO WeakStmt
stmtDefineDataConstructor m lamArgs baseType a xts (mb, b, yts) = do
  let consArgs = xts ++ yts
  let args = map identPlusToVar yts
  let b' = a <> nsSep <> b
  let indType =
        case xts of
          [] ->
            weakVar m a
          _ ->
            (m, WeakTermPiElim (weakVar m a) (map identPlusToVar xts))
  case consArgs of
    [] ->
      defineTerm
        True
        m
        b'
        indType
        ( m,
          WeakTermPiElim
            (weakVar m "unsafe.cast")
            [ baseType,
              indType,
              ( m,
                WeakTermPiIntro
                  OpacityTransparent
                  (LamKindCons a b')
                  lamArgs
                  (m, WeakTermPiElim (weakVar m b) args)
              )
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
        ( m,
          WeakTermPiElim
            (weakVar m "unsafe.cast")
            [ baseType,
              indType,
              ( m,
                WeakTermPiIntro
                  OpacityTransparent
                  (LamKindCons a b')
                  lamArgs
                  (m, WeakTermPiElim (weakVar m b) args)
              )
            ]
        )

stmtDefineDataClause :: IO (Hint, T.Text, [WeakIdentPlus])
stmtDefineDataClause = do
  token "-"
  m <- currentHint
  b <- symbol
  yts <- many stmtDefineDataClauseArg
  return (m, b, yts)

stmtDefineDataClauseArg :: IO WeakIdentPlus
stmtDefineDataClauseArg = do
  m <- currentHint
  tryPlanList
    [ weakAscription,
      weakTermToWeakIdent m weakTermSimple
    ]

stmtDefineCodata :: IO [WeakStmt]
stmtDefineCodata = do
  m <- currentHint
  token "define-codata"
  mFun <- currentHint
  a <- varText >>= withSectionPrefix
  xts <- many weakAscription
  yts <- many (token "-" >> ascriptionInner)
  formRule <- defineData m mFun a xts [(m, "new", yts)]
  elimRuleList <- mapM (stmtDefineCodataElim m a xts yts) yts
  return $ formRule ++ elimRuleList

stmtDefineCodataElim :: Hint -> T.Text -> [WeakIdentPlus] -> [WeakIdentPlus] -> WeakIdentPlus -> IO WeakStmt
stmtDefineCodataElim m a xts yts (mY, y, elemType) = do
  let codataType =
        case xts of
          [] ->
            weakVar m a
          _ ->
            (m, WeakTermPiElim (weakVar m a) (map identPlusToVar xts))
  recordVarText <- newText
  let projArgs = xts ++ [(m, asIdent recordVarText, codataType)]
  defineFunction
    True
    m
    mY
    (a <> nsSep <> asText y)
    projArgs
    elemType
    ( m,
      WeakTermCase
        elemType
        Nothing
        (weakVar m recordVarText, codataType)
        [((m, ("", a <> nsSep <> "new"), yts), weakVar m (asText y))]
    )

stmtDefineResourceType :: IO WeakStmt
stmtDefineResourceType = do
  m <- currentHint
  _ <- token "define-resource-type"
  name <- varText >>= withSectionPrefix
  discarder <- weakTermSimple
  copier <- weakTermSimple
  flag <- newTextualIdentFromText "flag"
  value <- newTextualIdentFromText "value"
  path <- toFilePath <$> getExecPath
  defineTerm
    True
    m
    name
    (m, WeakTermTau)
    ( m,
      WeakTermPiElim
        (weakVar m "unsafe.cast")
        [ ( m,
            WeakTermPi
              [ (m, flag, weakVar m "bool"),
                (m, value, weakVar m "unsafe.pointer")
              ]
              (weakVar m "unsafe.pointer")
          ),
          (m, WeakTermTau),
          ( m,
            WeakTermPiIntro
              OpacityTransparent
              LamKindResourceHandler
              [ (m, flag, weakVar m "bool"),
                (m, value, weakVar m "unsafe.pointer")
              ]
              ( m,
                WeakTermEnumElim
                  (weakVar m (asText flag), weakVar m "bool")
                  [ ( (m, EnumCaseLabel path "bool.true"),
                      (m, WeakTermPiElim copier [weakVar m (asText value)])
                    ),
                    ( (m, EnumCaseLabel path "bool.false"),
                      ( m,
                        WeakTermPiElim
                          (weakVar m "unsafe.cast")
                          [ weakVar m "top",
                            weakVar m "unsafe.pointer",
                            (m, WeakTermPiElim discarder [weakVar m (asText value)])
                          ]
                      )
                    )
                  ]
              )
          )
        ]
    )

setAsData :: T.Text -> Int -> [(Hint, T.Text, [WeakIdentPlus])] -> IO ()
setAsData a i bts = do
  let bs = map (\(_, b, _) -> a <> nsSep <> b) bts
  modifyIORef' dataEnv $ \env -> Map.insert a bs env
  forM_ (zip bs [0 ..]) $ \(x, k) ->
    modifyIORef' constructorEnv $ \env -> Map.insert x (i, k) env

toPiTypeWith :: Ident -> (Hint, T.Text, [WeakIdentPlus]) -> WeakIdentPlus
toPiTypeWith cod (m, b, yts) =
  (m, asIdent b, (m, WeakTermPi yts (m, WeakTermVar cod)))

identPlusToVar :: WeakIdentPlus -> WeakTermPlus
identPlusToVar (m, x, _) =
  (m, WeakTermVar x)

{-# INLINE isLinear #-}
isLinear :: [Int] -> Bool
isLinear =
  isLinear' S.empty

isLinear' :: S.Set Int -> [Int] -> Bool
isLinear' found input =
  case input of
    [] ->
      True
    (x : xs)
      | x `S.member` found ->
        False
      | otherwise ->
        isLinear' (S.insert x found) xs

setupEnumEnv :: IO ()
setupEnumEnv =
  forM_ initEnumEnvInfo $ uncurry setupEnumEnvWith

setupEnumEnvWith :: T.Text -> [(T.Text, Int)] -> IO ()
setupEnumEnvWith name xis = do
  path <- toFilePath <$> getExecPath
  let (xs, is) = unzip xis
  modifyIORef' enumEnv $ \env -> Map.insert name (path, xis) env
  let rev = Map.fromList $ zip xs (zip3 (repeat path) (repeat name) is)
  modifyIORef' revEnumEnv $ \env -> Map.union rev env

initEnumEnvInfo :: [(T.Text, [(T.Text, Int)])]
initEnumEnvInfo =
  [ ("bottom", []),
    ("top", [("top.unit", 0)]),
    ("bool", [("bool.false", 0), ("bool.true", 1)])
  ]

insEnumEnv :: FilePath -> Hint -> T.Text -> [(T.Text, Int)] -> IO ()
insEnumEnv path m name xis = do
  eenv <- readIORef enumEnv
  let definedEnums = Map.keys eenv ++ map fst (concatMap snd (Map.elems eenv))
  case find (`elem` definedEnums) $ name : map fst xis of
    Just x ->
      raiseError m $ "the constant `" <> x <> "` is already defined [ENUM]"
    _ -> do
      let (xs, is) = unzip xis
      let rev = Map.fromList $ zip xs (zip3 (repeat path) (repeat name) is)
      modifyIORef' enumEnv $ \env -> Map.insert name (path, xis) env
      modifyIORef' revEnumEnv $ \env -> Map.union rev env

varText :: IO T.Text
varText =
  snd <$> var

registerTopLevelName :: Hint -> Ident -> IO ()
registerTopLevelName m x = do
  nenv <- readIORef topNameEnv
  when (Map.member (asText x) nenv) $
    raiseError m $ "the variable `" <> asText x <> "` is already defined at the top level"
  path <- toFilePath <$> getCurrentFilePath
  modifyIORef' topNameEnv $ \env -> Map.insert (asText x) path env

initializeNamespace :: IO ()
initializeNamespace = do
  writeIORef topNameEnv Map.empty
  readIORef defaultAliasEnv >>= writeIORef aliasEnv
  writeIORef prefixEnv initialPrefixEnv
