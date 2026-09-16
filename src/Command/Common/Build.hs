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
import App.Run (forP, onFailure, raiseError', runApp)
import Command.Common.Build.EnsureMain qualified as EnsureMain
import Command.Common.Build.Execute qualified as Execute
import Command.Common.Build.Generate qualified as Gen
import Command.Common.Build.Install qualified as Install
import Command.Common.Build.Link qualified as Link
import Command.Common.Dependency qualified as Dependency
import Console.Handle qualified as Console
import Control.Comonad.Cofree
import Control.Concurrent (getNumCapabilities)
import Control.Exception (mask_)
import Control.Monad
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class
import Data.Containers.ListUtils (nubOrdOn)
import Data.Either (lefts)
import Data.Foldable
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time
import Ens.Ens qualified as E
import Ens.ToDoc qualified as E
import Gensym.CreateHandle qualified as Gensym
import Gensym.Handle qualified as Gensym
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
import Kernel.Common.Module.EnsureDeclaredPathExistence (ensureDeclaredPathExistence)
import Kernel.Common.OutputKind
import Kernel.Common.OutputKind qualified as OK
import Kernel.Common.Placeholder qualified as Placeholder
import Kernel.Common.RunProcess qualified as RunProcess
import Kernel.Common.Source
import Kernel.Common.SourceDependencyMap (SourceDependencyMap)
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
import Logger.Handle qualified as LoggerHandle
import Logger.Hint (internalHint)
import Logger.Print qualified as Logger
import Path
import Path.IO
import Path.Read (readTextFromPath)
import Path.Write (writeText)
import ProgressIndicator.ShowProgress qualified as Indicator
import SyntaxTree.Series qualified as SE
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
  discardStaleBuildDir h target' baseModule
  liftIO $
    Logger.report (Global.loggerHandle (globalHandle h)) $
      "Build configuration: target=" <> T.pack (show target') <> ", outputs=" <> T.pack (show $ _outputKindList h) <> ", skip-link=" <> T.pack (show $ _shouldSkipLink h) <> ", execute=" <> T.pack (show $ _shouldExecute h)
  startTime <- liftIO getCurrentTime
  unravelHandle <- liftIO $ Unravel.new (globalHandle h)
  Unravel.Result {..} <- Unravel.unravel unravelHandle baseModule target'
  let dependenceSeq = resultSourceList
  let liveSeq = filter (flip S.member resultLiveSourceSet . sourceFilePath) dependenceSeq
  sourceDependencyMap <- liftIO $ Unravel.getSourceDependencyMap unravelHandle dependenceSeq
  let traceReport = Console.getTraceConfig $ Global.consoleHandle $ globalHandle h
  traceConfig <- either raiseError' return $ Trace.new (Env.getMainModule $ Global.envHandle $ globalHandle h) traceReport
  let moduleList = nubOrdOn M.moduleID $ map sourceModule liveSeq
  didPerformForeignCompilation <- compileForeign h target' startTime moduleList
  let loadHandle = Load.new (globalHandle h)
  contentSeq <- Load.load loadHandle (Trace.isEnabled traceConfig) target' dependenceSeq
  withSystemTempDir "neut-object" $ \stagingDir -> do
    compile h traceConfig target' (_outputKindList h) sourceDependencyMap resultLiveSourceSet contentSeq stagingDir startTime
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
      Link.link linkHandle ct (_shouldSkipLink h) didPerformForeignCompilation resultArtifactTime liveSeq
      execute h (_shouldExecute h) ct (_executeArgs h)
      install h (_installDir h) ct

compile ::
  Handle ->
  Trace.Config ->
  Target ->
  [OutputKind] ->
  SourceDependencyMap ->
  S.Set (Path Abs File) ->
  [(Source, Either Cache T.Text)] ->
  Path Abs Dir ->
  UTCTime ->
  App ()
compile h traceConfig target outputKindList sourceDependencyMap liveSourceSet contentSeq stagingDir startTime = do
  numCapabilities <- liftIO getNumCapabilities
  generateHandle <- liftIO $ Gen.new (globalHandle h) stagingDir numCapabilities
  let cacheHandle = Cache.new (globalHandle h)
  bs <- mapM (needsCodeGeneration traceConfig cacheHandle outputKindList liveSourceSet . fst) contentSeq
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
  let color = [SetColor Foreground Vivid Green]
  let workingTitle = getWorkingTitle numOfItems
  let completedTitle = getCompletedTitle numOfItems
  Indicator.with consoleHandle loggerHandle (Just numOfItems) workingTitle completedTitle color $ \hp -> do
    cacheOrProgList <- Parse.parse (globalHandle h) contentSeq
    cacheOrStmtList <- forP cacheOrProgList $ \(gensymHandle, localHandle, (source, cacheOrProg)) -> do
      liftIO $ Logger.report (Global.loggerHandle (globalHandle h)) $ "Interpreting: " <> T.pack (toFilePath $ sourceFilePath source)
      interpretHandle <- liftIO $ Interpret.new gensymHandle (globalHandle h) localHandle (sourceModule source)
      item <- Interpret.interpret interpretHandle target source cacheOrProg
      return (gensymHandle, localHandle, (source, item))
    codeGenerationListRef <- liftIO $ newIORef []
    errors <- (`onFailure` cancelCodeGenerations codeGenerationListRef) $ do
      Dependency.run numCapabilities sourceDependencyMap Parse.getSourcePath cacheOrStmtList $ \(gensymHandle, localHandle, (source, (cacheOrStmt, logs))) -> do
        liftIO $ Logger.report (Global.loggerHandle (globalHandle h)) $ "Elaborating: " <> T.pack (toFilePath $ sourceFilePath source)
        elaborateHandle <- liftIO $ Elaborate.new gensymHandle (globalHandle h) traceConfig localHandle source startTime
        let ensureMainHandle = EnsureMain.new (Global.envHandle (globalHandle h))
        stmtList <- Elaborate.elaborate elaborateHandle target logs cacheOrStmt
        EnsureMain.ensureMain ensureMainHandle target source (map snd $ getStmtName stmtList)
        b <- needsCodeGeneration traceConfig cacheHandle outputKindList liveSourceSet source
        when b $ liftIO $ spawnCodeGeneration codeGenerationListRef $ do
          liftIO $ Logger.report (Global.loggerHandle (globalHandle h)) $ "Clarifying: " <> T.pack (toFilePath $ sourceFilePath source)
          clarifyHandle <- liftIO $ Clarify.new gensymHandle (globalHandle h) traceConfig
          (stmtList', auxStmtList, defMap) <- Clarify.clarify clarifyHandle stmtList
          liftIO $ Logger.report (Global.loggerHandle (globalHandle h)) $ "Lowering: " <> T.pack (toFilePath $ sourceFilePath source)
          lowerHandle <- Lower.new gensymHandle (globalHandle h) traceConfig target defMap
          virtualCode <- Lower.lower lowerHandle stmtList' auxStmtList
          emit h generateHandle gensymHandle hp startTime target outputKindList (Right source) virtualCode
      when (shouldRegisterUnusedTopLevelNameRemarks target) $
        registerUnusedTopLevelNameRemarks h
      entryPointVirtualCode <- compileEntryPoint h target outputKindList
      forM_ entryPointVirtualCode $ \(gensymHandle, src, code) ->
        liftIO $ spawnCodeGeneration codeGenerationListRef $ emit h generateHandle gensymHandle hp startTime target outputKindList src code
      codeGenerationList <- liftIO $ readIORef codeGenerationListRef
      fmap lefts $ mapM wait codeGenerationList
    objectErrors <-
      if null errors
        then do
          result <- liftIO $ runApp $ Gen.flushObjects generateHandle
          return $ lefts [result]
        else return []
    let allErrors = errors <> objectErrors
    if null allErrors
      then return ()
      else throwError $ E.join allErrors

spawnCodeGeneration :: IORef [Async (Either E.Error ())] -> App () -> IO ()
spawnCodeGeneration codeGenerationListRef generation = mask_ $ do
  codeGeneration <- asyncWithUnmask $ \unmask -> unmask $ runApp generation
  atomicModifyIORef' codeGenerationListRef $ \codeGenerationList -> (codeGeneration : codeGenerationList, ())

cancelCodeGenerations :: IORef [Async (Either E.Error ())] -> IO ()
cancelCodeGenerations codeGenerationListRef =
  readIORef codeGenerationListRef >>= mapM_ cancel

needsCodeGeneration :: Trace.Config -> Cache.Handle -> [OutputKind] -> S.Set (Path Abs File) -> Source -> App Bool
needsCodeGeneration traceConfig cacheHandle outputKindList liveSourceSet source
  | not $ S.member (sourceFilePath source) liveSourceSet =
      return False
  | Trace.isEnabled traceConfig =
      return True
  | otherwise =
      Cache.needsCompilation cacheHandle outputKindList source

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
  Gen.Handle ->
  Gensym.Handle ->
  Indicator.Handle ->
  UTCTime ->
  Target ->
  [OutputKind] ->
  Either MainTarget Source ->
  LC.LowCode ->
  App ()
emit h generateHandle gensymHandle progressBar timeStamp target outputKindList src code = do
  emitHandle <- Emit.new gensymHandle (globalHandle h) target
  let clangOptions = getCompileOption target
  llvmIR' <- liftIO $ Emit.emit emitHandle code
  progressLabel <- liftIO $ getProgressLabel h src
  forM_ outputKindList $ \outputKind -> do
    case outputKind of
      OK.Object -> do
        Gen.generateObject generateHandle target clangOptions timeStamp progressLabel src llvmIR'
      OK.LLVM -> do
        Gen.generateAsm generateHandle target timeStamp src llvmIR'
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

compileEntryPoint :: Handle -> Target -> [OutputKind] -> App [(Gensym.Handle, Either MainTarget Source, LC.LowCode)]
compileEntryPoint h target outputKindList = do
  case target of
    Peripheral {} ->
      return []
    PeripheralSingle {} ->
      return []
    Main t -> do
      gensymHandle <- liftIO Gensym.createHandle
      traceConfig <- newTraceConfig h
      let pathHandle = Global.pathHandle (globalHandle h)
      b <- Cache.isEntryPointCompilationSkippable pathHandle t outputKindList
      if b
        then do
          liftIO $ Logger.report (Global.loggerHandle (globalHandle h)) $ "Skipping entry-point code generation: " <> T.pack (show t) <> " (requested outputs are fresh)"
          return []
        else do
          liftIO $ Logger.report (Global.loggerHandle (globalHandle h)) $ "Generating entry point: " <> T.pack (show t)
          clarifyMainHandle <- liftIO $ Clarify.newMain gensymHandle (Platform.getDataSize (Global.platformHandle (globalHandle h)))
          (stmtList, defMap) <- liftIO $ Clarify.clarifyEntryPoint clarifyMainHandle
          liftIO $ Logger.report (Global.loggerHandle (globalHandle h)) $ "Lowering entry point: " <> T.pack (show t)
          lowerHandle <- Lower.new gensymHandle (globalHandle h) traceConfig target defMap
          mainVirtualCode <-
            Lower.lowerEntryPoint lowerHandle t stmtList
          return [(gensymHandle, Left t, mainVirtualCode)]

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

compileForeign :: Handle -> Target -> UTCTime -> [M.Module] -> App Bool
compileForeign h t startTime moduleList = do
  bs <- forP moduleList (compileForeign' h t startTime)
  return $ or bs

compileForeign' :: Handle -> Target -> UTCTime -> M.Module -> App Bool
compileForeign' h t startTime m = do
  let cmdList = M.script $ M.moduleForeign m
  let moduleRootDir = M.getModuleRootDir m
  foreignDir <- Path.getForeignDir (Global.pathHandle (globalHandle h)) t m
  let resolver = getForeignResolver h foreignDir m
  forM_ (M.input $ M.moduleForeign m) $ ensureDeclaredPathExistence moduleRootDir
  inputPathList <- fmap concat $ mapM (liftIO . Path.unrollPath . M.attachPrefixPath moduleRootDir . snd) $ M.input $ M.moduleForeign m
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
      cmdList' <- mapM (Placeholder.expand M.keyForeignScript resolver) cmdList
      unless (null cmdList') $ do
        liftIO $
          Logger.report (Global.loggerHandle (globalHandle h)) $
            "Performing foreign compilation of `" <> MID.reify (M.moduleID m) <> "`"
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
          then setModificationTime outputPath startTime
          else raiseError' $ "Missing foreign output: " <> T.pack (toFilePath outputPath)
      return $ not $ null cmdList

getForeignResolver :: Handle -> Path Abs Dir -> M.Module -> Placeholder.Resolver
getForeignResolver h foreignDir hostModule hint name = do
  case name of
    "clang" -> do
      clangCommand <- liftIO $ getForeignClangCommand (Global.loggerHandle (globalHandle h)) (Global.platformHandle (globalHandle h))
      return $ Just clangCommand
    "foreign" ->
      return $ Just $ Placeholder.quote $ T.pack $ toFilePath foreignDir
    _ -> do
      let mainModule = Env.getMainModule (Global.envHandle (globalHandle h))
      Placeholder.moduleResolver (Global.moduleHandle (globalHandle h)) mainModule hostModule hint name

getForeignClangCommand :: LoggerHandle.Handle -> Platform.Handle -> IO T.Text
getForeignClangCommand loggerHandle platformHandle = do
  let clang = Platform.getClang platformHandle
  let targetTriple = Platform.getClangTargetTriple platformHandle
  sysrootOption <- Platform.getSysrootOption loggerHandle platformHandle
  let toolchainOption = Platform.getToolchainOption platformHandle
  return $
    Placeholder.quoteWords $
      map T.pack $
        [clang, "-target", targetTriple] ++ sysrootOption ++ toolchainOption

discardStaleBuildDir :: Handle -> Target -> M.Module -> App ()
discardStaleBuildDir h target baseModule = do
  recordPath <- Path.getExpandedClangOptionPath (Global.pathHandle (globalHandle h)) target baseModule
  let expandedClangOption = showExpandedClangOption target
  recordExists <- doesFileExist recordPath
  recordedClangOption <- if recordExists then Just <$> readTextFromPath recordPath else return Nothing
  when (recordedClangOption /= Just expandedClangOption) $ do
    buildDir <- Path.getBuildDir (Global.pathHandle (globalHandle h)) target baseModule
    liftIO $ Logger.report (Global.loggerHandle (globalHandle h)) "The expansion of the clang options changed; discarding the build directory"
    ignoringAbsence $ removeDirRecur buildDir
    ensureDir buildDir
    liftIO $ writeText recordPath expandedClangOption

showExpandedClangOption :: Target -> T.Text
showExpandedClangOption target = do
  let compileOption' = map T.pack $ getCompileOption target
  let linkOption' = case target of
        Main mainTarget ->
          map T.pack $ getLinkOption mainTarget
        _ ->
          []
  E.pp $
    E.inject $
      E.dictFromListVertical internalHint $
        [ (M.keyCompileOption, internalHint :< E.List (SE.fromList SE.Bracket SE.Comma (map (\option -> internalHint :< E.String option) compileOption'))),
          (M.keyLinkOption, internalHint :< E.List (SE.fromList SE.Bracket SE.Comma (map (\option -> internalHint :< E.String option) linkOption')))
        ]

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
      return $ filter (not . T.null) $ T.lines $ decodeUtf8With lenientDecode value
    Left err ->
      throwError $ newError' err
