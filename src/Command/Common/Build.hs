module Command.Common.Build
  ( Config (..),
    Handle,
    new,
    buildTarget,
  )
where

import Command.Common.Build.EnsureMain qualified as EnsureMain
import Command.Common.Build.Execute qualified as Execute
import Command.Common.Build.Generate qualified as Gen
import Command.Common.Build.Install qualified as Install
import Command.Common.Build.Link qualified as Link
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
import Error.EIO (EIO)
import Error.Error (newError')
import Error.Error qualified as E
import Error.Run (forP, raiseError', runEIO)
import Kernel.Clarify.Clarify qualified as Clarify
import Kernel.Common.Cache
import Kernel.Common.ClangOption qualified as CL
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.GlobalRemark qualified as GlobalRemark
import Kernel.Common.Handle.Global.Path qualified as Path
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.ManageCache (needsCompilation)
import Kernel.Common.ManageCache qualified as Cache
import Kernel.Common.Module qualified as M
import Kernel.Common.OutputKind
import Kernel.Common.OutputKind qualified as OK
import Kernel.Common.RunProcess qualified as RunProcess
import Kernel.Common.Source
import Kernel.Common.Target
import Kernel.Elaborate.Elaborate qualified as Elaborate
import Kernel.Elaborate.Internal.Handle.Elaborate qualified as Elaborate
import Kernel.Emit.Emit qualified as Emit
import Kernel.Load.Load qualified as Load
import Kernel.Lower.Lower qualified as Lower
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

buildTarget :: Handle -> M.MainModule -> Target -> EIO ()
buildTarget h (M.MainModule baseModule) target = do
  liftIO $ Logger.report (Global.loggerHandle (globalHandle h)) $ "Building: " <> T.pack (show target)
  target' <- expandClangOptions h target
  unravelHandle <- liftIO $ Unravel.new (globalHandle h)
  (artifactTime, dependenceSeq) <- Unravel.unravel unravelHandle baseModule target'
  let moduleList = nubOrdOn M.moduleID $ map sourceModule dependenceSeq
  didPerformForeignCompilation <- compileForeign h target moduleList
  let loadHandle = Load.new (globalHandle h)
  contentSeq <- Load.load loadHandle target dependenceSeq
  compile h target' (_outputKindList h) contentSeq
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

compile :: Handle -> Target -> [OutputKind] -> [(Source, Either Cache T.Text)] -> EIO ()
compile h target outputKindList contentSeq = do
  let cacheHandle = Cache.new (globalHandle h)
  bs <- mapM (needsCompilation cacheHandle outputKindList . fst) contentSeq
  c <- getEntryPointCompilationCount h target outputKindList
  let numOfItems = length (filter id bs) + c
  let colorHandle = Global.colorHandle (globalHandle h)
  currentTime <- liftIO getCurrentTime
  let color = [SetColor Foreground Vivid Green]
  let workingTitle = getWorkingTitle numOfItems
  let completedTitle = getCompletedTitle numOfItems
  let silentMode = Env.getSilentMode (Global.envHandle (globalHandle h))
  hp <- liftIO $ Indicator.new colorHandle silentMode (Just numOfItems) workingTitle completedTitle color
  cacheOrProgList <- Parse.parse (globalHandle h) contentSeq
  cacheOrStmtList <- forP cacheOrProgList $ \(localHandle, (source, cacheOrProg)) -> do
    interpretHandle <- liftIO $ Interpret.new (globalHandle h) localHandle
    item <- Interpret.interpret interpretHandle target source cacheOrProg
    return (localHandle, (source, item))
  contentAsync <- fmap catMaybes $ forM cacheOrStmtList $ \(localHandle, (source, (cacheOrStmt, logs))) -> do
    elaborateHandle <- liftIO $ Elaborate.new (globalHandle h) localHandle source
    let ensureMainHandle = EnsureMain.new (Global.envHandle (globalHandle h))
    stmtList <- Elaborate.elaborate elaborateHandle target logs cacheOrStmt
    EnsureMain.ensureMain ensureMainHandle target source (map snd $ getStmtName stmtList)
    clarifyHandle <- liftIO $ Clarify.new (globalHandle h) localHandle
    stmtList' <- Clarify.clarify clarifyHandle stmtList
    b <- Cache.needsCompilation cacheHandle outputKindList source
    if b
      then do
        fmap Just $ liftIO $ async $ runEIO $ do
          lowerHandle <- liftIO $ Lower.new (globalHandle h)
          virtualCode <- Lower.lower lowerHandle stmtList'
          emit h hp currentTime target outputKindList (Right source) virtualCode
      else return Nothing
  entryPointVirtualCode <- compileEntryPoint h target outputKindList
  entryPointAsync <- forM entryPointVirtualCode $ \(src, code) -> liftIO $ do
    async $ runEIO $ emit h hp currentTime target outputKindList src code
  errors <- fmap lefts $ mapM wait $ entryPointAsync ++ contentAsync
  liftIO $ Indicator.close hp
  if null errors
    then return ()
    else throwError $ E.join errors

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
  EIO ()
emit h progressBar currentTime target outputKindList src code = do
  let emitHandle = Emit.new (globalHandle h)
  let llvmHandle = Gen.new (globalHandle h)
  let clangOptions = getCompileOption target
  llvmIR' <- liftIO $ Emit.emit emitHandle code
  forM_ outputKindList $ \outputKind -> do
    case outputKind of
      OK.Object -> do
        Gen.generateObject llvmHandle target clangOptions currentTime src llvmIR'
      OK.LLVM -> do
        Gen.generateAsm llvmHandle target currentTime src llvmIR'
  liftIO $ Indicator.increment progressBar

getEntryPointCompilationCount :: Handle -> Target -> [OutputKind] -> EIO Int
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

compileEntryPoint :: Handle -> Target -> [OutputKind] -> EIO [(Either MainTarget Source, LC.LowCode)]
compileEntryPoint h target outputKindList = do
  case target of
    Peripheral {} ->
      return []
    PeripheralSingle {} ->
      return []
    Main t -> do
      let pathHandle = Global.pathHandle (globalHandle h)
      b <- Cache.isEntryPointCompilationSkippable pathHandle t outputKindList
      if b
        then return []
        else do
          clarifyMainHandle <- liftIO $ Clarify.newMain (globalHandle h)
          lowerHandle <- liftIO $ Lower.new (globalHandle h)
          mainVirtualCode <-
            liftIO (Clarify.clarifyEntryPoint clarifyMainHandle)
              >>= Lower.lowerEntryPoint lowerHandle t
          return [(Left t, mainVirtualCode)]

execute :: Handle -> Bool -> MainTarget -> [String] -> EIO ()
execute h shouldExecute target args = do
  when shouldExecute $ do
    let executeHandle = Execute.new (globalHandle h)
    Execute.execute executeHandle target args

install :: Handle -> Maybe FilePath -> MainTarget -> EIO ()
install h filePathOrNone target = do
  mDir <- mapM Path.getInstallDir filePathOrNone
  let installHandle = Install.new (globalHandle h)
  mapM_ (Install.install installHandle target) mDir

compileForeign :: Handle -> Target -> [M.Module] -> EIO Bool
compileForeign h t moduleList = do
  currentTime <- liftIO getCurrentTime
  bs <- forP moduleList (compileForeign' h t currentTime)
  return $ or bs

compileForeign' :: Handle -> Target -> UTCTime -> M.Module -> EIO Bool
compileForeign' h t currentTime m = do
  sub <- getForeignSubst h t m
  let cmdList = M.script $ M.moduleForeign m
  unless (null cmdList) $ do
    liftIO $
      Logger.report (Global.loggerHandle (globalHandle h)) $
        "Performing foreign compilation of `" <> MID.reify (M.moduleID m) <> "` with " <> T.pack (show sub)
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

getForeignSubst :: Handle -> Target -> M.Module -> EIO [(T.Text, T.Text)]
getForeignSubst h t m = do
  clang <- liftIO Platform.getClang
  foreignDir <- Path.getForeignDir (Global.pathHandle (globalHandle h)) t m
  return
    [ ("{{module-root}}", T.pack $ toFilePath $ M.getModuleRootDir m),
      ("{{clang}}", T.pack clang),
      ("{{foreign}}", T.pack $ toFilePath foreignDir)
    ]

getInputPathList :: Path Abs Dir -> M.SomePath Rel -> EIO [Path Abs File]
getInputPathList moduleRootDir =
  Path.unrollPath . attachPrefixPath moduleRootDir

attachPrefixPath :: Path Abs Dir -> M.SomePath Rel -> M.SomePath Abs
attachPrefixPath baseDirPath path =
  case path of
    Left dirPath ->
      Left $ baseDirPath </> dirPath
    Right filePath ->
      Right $ baseDirPath </> filePath

expandClangOptions :: Handle -> Target -> EIO Target
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
        Zen path clangOption -> do
          compileOption' <- expandOptions h (CL.compileOption clangOption)
          linkOption' <- expandOptions h (CL.linkOption clangOption)
          return $ Main $ Zen path $ CL.ClangOption {compileOption = compileOption', linkOption = linkOption'}
    Peripheral {} ->
      return target
    PeripheralSingle {} ->
      return target

expandOptions :: Handle -> [T.Text] -> EIO [T.Text]
expandOptions h textList =
  map T.strip <$> mapM (expandText h) textList

expandText :: Handle -> T.Text -> EIO T.Text
expandText h t = do
  let spec =
        RunProcess.Spec
          { cmdspec = RawCommand "sh" ["-c", unwords [T.unpack "printf", "%s", "\"" ++ T.unpack t ++ "\""]],
            cwd = Nothing
          }
  output <- liftIO $ RunProcess.run01 (runProcessHandle h) spec
  case output of
    Right value ->
      return $ decodeUtf8 value
    Left err ->
      throwError $ newError' err
