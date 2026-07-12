module Command.Common.Build
  ( Config (..),
    Handle,
    new,
    buildTarget,
  )
where

import App.App (App)
import App.Error (newError')
import App.Error qualified as E
import App.Run (forP, raiseError', runApp)
import Command.Common.Build.EnsureMain qualified as EnsureMain
import Command.Common.Build.Execute qualified as Execute
import Command.Common.Build.Generate qualified as Gen
import Command.Common.Build.Install qualified as Install
import Command.Common.Build.Link qualified as Link
import Console.Handle qualified as Console
import Control.Monad
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class
import Data.Containers.ListUtils (nubOrdOn)
import Data.Either (lefts)
import Data.Foldable
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time
import Kernel.Clarify.Clarify qualified as Clarify
import Kernel.Common.Cache
import Kernel.Common.ClangOption qualified as CL
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.GlobalRemark qualified as GlobalRemark
import Kernel.Common.Handle.Global.ModulePath qualified as ModulePath
import Kernel.Common.Handle.Global.Path qualified as Path
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.ManageCache qualified as Cache
import Kernel.Common.Module qualified as M
import Kernel.Common.OutputKind
import Kernel.Common.OutputKind qualified as OK
import Kernel.Common.RunProcess qualified as RunProcess
import Kernel.Common.Source
import Kernel.Common.Target
import Kernel.Common.Trace qualified as Trace
import Kernel.Common.ZenConfig qualified as Z
import Kernel.Elaborate.Elaborate qualified as Elaborate
import Kernel.Elaborate.Internal.Handle.Elaborate qualified as Elaborate
import Kernel.Emit.Emit qualified as Emit
import Kernel.Load.Load qualified as Load
import Kernel.Lower.Lower qualified as Lower
import Kernel.Parse.Internal.Handle.UnusedTopLevelName qualified as UnusedTopLevelName
import Kernel.Parse.Interpret qualified as Interpret
import Kernel.Parse.Parse qualified as Parse
import Kernel.Unravel.Unravel qualified as Unravel
import Language.Common.ModuleID qualified as MID
import Language.LowComp.LowComp qualified as LC
import Language.Term.Stmt (getStmtName)
import Logger.Debug qualified as Logger
import Logger.Print qualified as Logger
import Path
import Path.IO
import ProgressIndicator.ShowProgress qualified as Indicator
import System.Console.ANSI
import System.Process (CmdSpec (RawCommand, ShellCommand))
import UnliftIO.Async
import Prelude hiding (log)

data Config = Config
  { outputKindList :: [OutputKind],
    shouldSkipLink :: Bool,
    shouldExecute :: Bool,
    installDir :: Maybe FilePath,
    executeArgs :: [String]
  }

data Handle = Handle
  { globalHandle :: Global.Handle,
    runProcessHandle :: RunProcess.Handle,
    _outputKindList :: [OutputKind],
    _shouldSkipLink :: Bool,
    _shouldExecute :: Bool,
    _installDir :: Maybe FilePath,
    _executeArgs :: [String]
  }

new ::
  Config ->
  Global.Handle ->
  Handle
new cfg globalHandle = do
  let runProcessHandle = RunProcess.new (Global.loggerHandle globalHandle)
  let _outputKindList = outputKindList cfg
  let _shouldSkipLink = shouldSkipLink cfg
  let _shouldExecute = shouldExecute cfg
  let _installDir = installDir cfg
  let _executeArgs = executeArgs cfg
  Handle {..}

buildTarget :: Handle -> M.MainModule -> Target -> App ()
buildTarget h (M.MainModule baseModule) target = do
  liftIO $ Logger.report (Global.loggerHandle (globalHandle h)) $ "Building: " <> T.pack (show target)
  target' <- expandClangOptions h target
  liftIO $
    Logger.report (Global.loggerHandle (globalHandle h)) $
      "Build configuration: target=" <> T.pack (show target') <> ", outputs=" <> T.pack (show $ _outputKindList h) <> ", skip-link=" <> T.pack (show $ _shouldSkipLink h) <> ", execute=" <> T.pack (show $ _shouldExecute h)
  unravelHandle <- liftIO $ Unravel.new (globalHandle h)
  (artifactTime, dependenceSeq) <- Unravel.unravel unravelHandle baseModule target'
  let traceReport = Console.getTraceConfig $ Global.consoleHandle $ globalHandle h
  traceConfig <- either raiseError' return $ Trace.new (Env.getMainModule $ Global.envHandle $ globalHandle h) traceReport
  let moduleList = nubOrdOn M.moduleID $ map sourceModule dependenceSeq
  didPerformForeignCompilation <- compileForeign h target moduleList
  let loadHandle = Load.new (globalHandle h)
  contentSeq <- Load.load loadHandle (Trace.isEnabled traceConfig) target dependenceSeq
  compile h traceConfig target' (_outputKindList h) contentSeq
  liftIO $
    GlobalRemark.get (Global.globalRemarkHandle (globalHandle h))
      >>= Logger.printLogList (Global.loggerHandle (globalHandle h))
  case target' of
    Peripheral {} ->
      return ()
    PeripheralSingle {} ->
      return ()
    Main ct -> do
      let linkHandle = Link.new (globalHandle h)
      Link.link linkHandle ct (_shouldSkipLink h) didPerformForeignCompilation artifactTime (toList dependenceSeq)
      execute h (_shouldExecute h) ct (_executeArgs h)
      install h (_installDir h) ct

compile :: Handle -> Trace.Config -> Target -> [OutputKind] -> [(Source, Either Cache T.Text)] -> App ()
compile h traceConfig target outputKindList contentSeq = do
  let cacheHandle = Cache.new (globalHandle h)
  bs <- mapM (needsCodeGeneration cacheHandle . fst) contentSeq
  forM_ (zip contentSeq bs) $ \((source, _), shouldGenerateCode) -> do
    let sourcePath = T.pack $ toFilePath $ sourceFilePath source
    let message =
          if shouldGenerateCode
            then "Scheduling code generation: " <> sourcePath
            else "Skipping code generation: " <> sourcePath <> " (requested outputs are fresh)"
    liftIO $ Logger.report (Global.loggerHandle (globalHandle h)) message
  c <- getEntryPointCompilationCount h target outputKindList
  let numOfItems = length (filter id bs) + c
  let consoleHandle = Global.consoleHandle (globalHandle h)
  let loggerHandle = Global.loggerHandle (globalHandle h)
  currentTime <- liftIO getCurrentTime
  let color = [SetColor Foreground Vivid Green]
  let workingTitle = getWorkingTitle numOfItems
  let completedTitle = getCompletedTitle numOfItems
  hp <- liftIO $ Indicator.new consoleHandle loggerHandle (Just numOfItems) workingTitle completedTitle color
  cacheOrProgList <- Parse.parse (globalHandle h) contentSeq
  cacheOrStmtList <- forP cacheOrProgList $ \(localHandle, (source, cacheOrProg)) -> do
    liftIO $ Logger.report (Global.loggerHandle (globalHandle h)) $ "Interpreting: " <> T.pack (toFilePath $ sourceFilePath source)
    interpretHandle <- liftIO $ Interpret.new (globalHandle h) localHandle (sourceModule source)
    item <- Interpret.interpret interpretHandle target source cacheOrProg
    return (localHandle, (source, item))
  contentAsync <- fmap catMaybes $ forM cacheOrStmtList $ \(localHandle, (source, (cacheOrStmt, logs))) -> do
    liftIO $ Logger.report (Global.loggerHandle (globalHandle h)) $ "Elaborating: " <> T.pack (toFilePath $ sourceFilePath source)
    elaborateHandle <- liftIO $ Elaborate.new (globalHandle h) traceConfig localHandle source
    let ensureMainHandle = EnsureMain.new (Global.envHandle (globalHandle h))
    stmtList <- Elaborate.elaborate elaborateHandle target logs cacheOrStmt
    EnsureMain.ensureMain ensureMainHandle target source (map snd $ getStmtName stmtList)
    b <- needsCodeGeneration cacheHandle source
    if b
      then do
        fmap Just $ liftIO $ async $ runApp $ do
          liftIO $ Logger.report (Global.loggerHandle (globalHandle h)) $ "Clarifying: " <> T.pack (toFilePath $ sourceFilePath source)
          clarifyHandle <- liftIO $ Clarify.new (globalHandle h) traceConfig
          (stmtList', auxStmtList, defMap) <- Clarify.clarify clarifyHandle stmtList
          liftIO $ Logger.report (Global.loggerHandle (globalHandle h)) $ "Lowering: " <> T.pack (toFilePath $ sourceFilePath source)
          lowerHandle <- Lower.new (globalHandle h) traceConfig target defMap
          virtualCode <- Lower.lower lowerHandle stmtList' auxStmtList
          emit h hp currentTime target outputKindList (Right source) virtualCode
      else return Nothing
  when (shouldRegisterUnusedTopLevelNameRemarks target) $
    registerUnusedTopLevelNameRemarks h
  entryPointVirtualCode <- compileEntryPoint h target outputKindList
  entryPointAsync <- forM entryPointVirtualCode $ \(src, code) -> liftIO $ do
    async $ runApp $ emit h hp currentTime target outputKindList src code
  errors <- fmap lefts $ mapM wait $ entryPointAsync ++ contentAsync
  liftIO $ Indicator.close hp
  if null errors
    then return ()
    else throwError $ E.join errors
  where
    needsCodeGeneration cacheHandle source = do
      if Trace.isEnabled traceConfig
        then return True
        else Cache.needsCompilation cacheHandle outputKindList source

registerUnusedTopLevelNameRemarks :: Handle -> App ()
registerUnusedTopLevelNameRemarks h = do
  modulePathMap <- liftIO $ ModulePath.get $ Global.modulePathHandle $ globalHandle h
  logs <- liftIO $ UnusedTopLevelName.flushRemarks modulePathMap $ Global.unusedTopLevelNameHandle $ globalHandle h
  liftIO $ GlobalRemark.insert (Global.globalRemarkHandle $ globalHandle h) logs

shouldRegisterUnusedTopLevelNameRemarks :: Target -> Bool
shouldRegisterUnusedTopLevelNameRemarks target = do
  case target of
    Peripheral {} ->
      True
    PeripheralSingle {} ->
      False
    Main {} ->
      False

getCompletedTitle :: Int -> T.Text
getCompletedTitle numOfItems = do
  let suffix = if numOfItems <= 1 then "" else "s"
  "Compiled " <> T.pack (show numOfItems) <> " file" <> suffix

getWorkingTitle :: Int -> T.Text
getWorkingTitle numOfItems = do
  let suffix = if numOfItems <= 1 then "" else "s"
  "Compiling " <> T.pack (show numOfItems) <> " file" <> suffix

emit ::
  Handle ->
  Indicator.Handle ->
  UTCTime ->
  Target ->
  [OutputKind] ->
  Either MainTarget Source ->
  LC.LowCode ->
  App ()
emit h progressBar currentTime target outputKindList src code = do
  emitHandle <- Emit.new (globalHandle h) target
  let llvmHandle = Gen.new (globalHandle h)
  let clangOptions = getCompileOption target
  llvmIR' <- liftIO $ Emit.emit emitHandle code
  forM_ outputKindList $ \outputKind -> do
    case outputKind of
      OK.Object -> do
        Gen.generateObject llvmHandle target clangOptions currentTime src llvmIR'
      OK.LLVM -> do
        Gen.generateAsm llvmHandle target currentTime src llvmIR'
  progressLabel <- liftIO $ getProgressLabel h src
  liftIO $ Indicator.increment progressBar progressLabel

getProgressLabel :: Handle -> Either MainTarget Source -> IO T.Text
getProgressLabel h src = do
  case src of
    Left _ ->
      return "entrypoint"
    Right source -> do
      modulePathMap <- ModulePath.get $ Global.modulePathHandle $ globalHandle h
      ModulePath.renderSource modulePathMap source

getEntryPointCompilationCount :: Handle -> Target -> [OutputKind] -> App Int
getEntryPointCompilationCount h target outputKindList = do
  case target of
    Peripheral {} ->
      return 0
    PeripheralSingle {} ->
      return 0
    Main t -> do
      let pathHandle = Global.pathHandle (globalHandle h)
      b <- Cache.isEntryPointCompilationSkippable pathHandle t outputKindList
      return $ if b then 0 else 1

compileEntryPoint :: Handle -> Target -> [OutputKind] -> App [(Either MainTarget Source, LC.LowCode)]
compileEntryPoint h target outputKindList = do
  case target of
    Peripheral {} ->
      return []
    PeripheralSingle {} ->
      return []
    Main t -> do
      traceConfig <- newTraceConfig h
      let pathHandle = Global.pathHandle (globalHandle h)
      b <- Cache.isEntryPointCompilationSkippable pathHandle t outputKindList
      if b
        then do
          liftIO $ Logger.report (Global.loggerHandle (globalHandle h)) $ "Skipping entry-point code generation: " <> T.pack (show t) <> " (requested outputs are fresh)"
          return []
        else do
          liftIO $ Logger.report (Global.loggerHandle (globalHandle h)) $ "Generating entry point: " <> T.pack (show t)
          clarifyMainHandle <- liftIO $ Clarify.newMain (globalHandle h)
          (stmtList, defMap) <- liftIO $ Clarify.clarifyEntryPoint clarifyMainHandle
          liftIO $ Logger.report (Global.loggerHandle (globalHandle h)) $ "Lowering entry point: " <> T.pack (show t)
          lowerHandle <- Lower.new (globalHandle h) traceConfig target defMap
          mainVirtualCode <-
            Lower.lowerEntryPoint lowerHandle t stmtList
          return [(Left t, mainVirtualCode)]

newTraceConfig :: Handle -> App Trace.Config
newTraceConfig h = do
  let traceReport = Console.getTraceConfig $ Global.consoleHandle $ globalHandle h
  either raiseError' return $ Trace.new (Env.getMainModule $ Global.envHandle $ globalHandle h) traceReport

execute :: Handle -> Bool -> MainTarget -> [String] -> App ()
execute h shouldExecute target args = do
  when shouldExecute $ do
    let executeHandle = Execute.new (globalHandle h)
    Execute.execute executeHandle target args

install :: Handle -> Maybe FilePath -> MainTarget -> App ()
install h filePathOrNone target = do
  mDir <- mapM Path.getInstallDir filePathOrNone
  let installHandle = Install.new (globalHandle h)
  mapM_ (Install.install installHandle target) mDir

compileForeign :: Handle -> Target -> [M.Module] -> App Bool
compileForeign h t moduleList = do
  currentTime <- liftIO getCurrentTime
  bs <- forP moduleList (compileForeign' h t currentTime)
  return $ or bs

compileForeign' :: Handle -> Target -> UTCTime -> M.Module -> App Bool
compileForeign' h t currentTime m = do
  sub <- getForeignSubst h t m
  let cmdList = M.script $ M.moduleForeign m
  let moduleRootDir = M.getModuleRootDir m
  foreignDir <- Path.getForeignDir (Global.pathHandle (globalHandle h)) t m
  inputPathList <- fmap concat $ mapM (getInputPathList moduleRootDir) $ M.input $ M.moduleForeign m
  let outputPathList = map (foreignDir </>) $ M.output $ M.moduleForeign m
  for_ outputPathList $ \outputPath -> do
    ensureDir $ parent outputPath
  inputTime <- Path.getLastModifiedSup inputPathList
  outputTime <- Path.getLastModifiedInf outputPathList
  case (inputTime, outputTime) of
    (Just t1, Just t2)
      | t1 <= t2 -> do
          liftIO $
            Logger.report (Global.loggerHandle (globalHandle h)) $
              "Cache found; skipping foreign compilation of `" <> MID.reify (M.moduleID m) <> "`"
          return False
    _ -> do
      let cmdList' = map (naiveReplace sub) cmdList
      unless (null cmdList') $ do
        liftIO $
          Logger.report (Global.loggerHandle (globalHandle h)) $
            "Performing foreign compilation of `" <> MID.reify (M.moduleID m) <> "` with " <> T.pack (show sub)
      forM_ cmdList' $ \cmd -> do
        let spec =
              RunProcess.Spec
                { cmdspec = ShellCommand (T.unpack cmd),
                  cwd = Just (toFilePath moduleRootDir)
                }
        result <- liftIO $ RunProcess.run00 (runProcessHandle h) spec
        case result of
          Right _ ->
            return ()
          Left err -> do
            raiseError' $
              "Foreign compilation of `"
                <> MID.reify (M.moduleID m)
                <> "` failed at `"
                <> cmd
                <> "` with the following error:\n"
                <> err
      forM_ outputPathList $ \outputPath -> do
        b <- doesFileExist outputPath
        if b
          then setModificationTime outputPath currentTime
          else raiseError' $ "Missing foreign output: " <> T.pack (toFilePath outputPath)
      return $ not $ null cmdList

naiveReplace :: [(T.Text, T.Text)] -> T.Text -> T.Text
naiveReplace sub t =
  case sub of
    [] ->
      t
    (from, to) : rest -> do
      T.replace from to (naiveReplace rest t)

getForeignSubst :: Handle -> Target -> M.Module -> App [(T.Text, T.Text)]
getForeignSubst h t m = do
  clang <- liftIO Platform.getClang
  foreignDir <- Path.getForeignDir (Global.pathHandle (globalHandle h)) t m
  return
    [ ("{{module-root}}", shellQuote $ T.pack $ toFilePath $ M.getModuleRootDir m),
      ("{{clang}}", shellQuote $ T.pack clang),
      ("{{foreign}}", shellQuote $ T.pack $ toFilePath foreignDir)
    ]

shellQuote :: T.Text -> T.Text
shellQuote text =
  "'" <> T.replace "'" "'\\''" text <> "'"

getInputPathList :: Path Abs Dir -> M.SomePath Rel -> App [Path Abs File]
getInputPathList moduleRootDir =
  Path.unrollPath . attachPrefixPath moduleRootDir

attachPrefixPath :: Path Abs Dir -> M.SomePath Rel -> M.SomePath Abs
attachPrefixPath baseDirPath path =
  case path of
    Left dirPath ->
      Left $ baseDirPath </> dirPath
    Right filePath ->
      Right $ baseDirPath </> filePath

expandClangOptions :: Handle -> Target -> App Target
expandClangOptions h target =
  case target of
    Main concreteTarget ->
      case concreteTarget of
        Named targetName summary -> do
          let cl = clangOption summary
          compileOption' <- expandOptions h (CL.compileOption cl)
          linkOption' <- expandOptions h (CL.linkOption cl)
          return $
            Main $
              Named
                targetName
                ( summary
                    { clangOption =
                        CL.ClangOption
                          { compileOption = compileOption',
                            linkOption = linkOption'
                          }
                    }
                )
        Zen path zenConfig -> do
          let cl = Z.clangOption zenConfig
          compileOption' <- expandOptions h (CL.compileOption cl)
          linkOption' <- expandOptions h (CL.linkOption cl)
          let cl' = CL.ClangOption {compileOption = compileOption', linkOption = linkOption'}
          let zenConfig' = zenConfig {Z.clangOption = cl'}
          return $ Main $ Zen path zenConfig'
    Peripheral {} ->
      return target
    PeripheralSingle {} ->
      return target

expandOptions :: Handle -> [T.Text] -> App [T.Text]
expandOptions h textList =
  concat <$> mapM (expandText h) textList

expandText :: Handle -> T.Text -> App [T.Text]
expandText h t = do
  let spec =
        RunProcess.Spec
          { cmdspec = RawCommand "sh" ["-c", "printf '%s\\n' " ++ T.unpack t],
            cwd = Nothing
          }
  output <- liftIO $ RunProcess.run01 (runProcessHandle h) spec
  case output of
    Right value ->
      return $ filter (not . T.null) $ T.lines $ decodeUtf8 value
    Left err ->
      throwError $ newError' err
