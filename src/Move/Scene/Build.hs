module Move.Scene.Build
  ( Config (..),
    Handle,
    new,
    buildTarget,
  )
where

import Control.Monad
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class
import Data.Containers.ListUtils (nubOrdOn)
import Data.Either (isLeft, lefts)
import Data.Foldable
import Data.Maybe
import Data.Text qualified as T
import Data.Time
import Move.Console.Report qualified as Report
import Move.Context.Cache (needsCompilation)
import Move.Context.Cache qualified as Cache
import Move.Context.Clang qualified as Clang
import Move.Context.Color qualified as Color
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (EIO, forP, raiseError', runEIO)
import Move.Context.Elaborate qualified as Elaborate
import Move.Context.Env qualified as Env
import Move.Context.External qualified as External
import Move.Context.LLVM qualified as LLVM
import Move.Context.Path qualified as Path
import Move.Scene.Clarify qualified as Clarify
import Move.Scene.Elaborate qualified as Elaborate
import Move.Scene.Emit qualified as Emit
import Move.Scene.EnsureMain qualified as EnsureMain
import Move.Scene.Execute qualified as Execute
import Move.Scene.Init.Base qualified as Base
import Move.Scene.Init.Local qualified as Local
import Move.Scene.Install qualified as Install
import Move.Scene.Link qualified as Link
import Move.Scene.Load qualified as Load
import Move.Scene.Lower qualified as Lower
import Move.Scene.Parse qualified as Parse
import Move.Scene.ShowProgress qualified as ProgressBar
import Move.Scene.Unravel qualified as Unravel
import Move.UI.Handle.GlobalRemark qualified as GlobalRemark
import Path
import Path.IO
import Rule.Cache
import Rule.ClangOption qualified as CL
import Rule.Error qualified as E
import Rule.LowComp qualified as LC
import Rule.Module qualified as M
import Rule.ModuleID qualified as MID
import Rule.OutputKind
import Rule.Source
import Rule.Stmt (getStmtName)
import Rule.Target
import System.Console.ANSI
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
  { baseHandle :: Base.Handle,
    _outputKindList :: [OutputKind],
    _shouldSkipLink :: Bool,
    _shouldExecute :: Bool,
    _installDir :: Maybe FilePath,
    _executeArgs :: [String]
  }

new ::
  Config ->
  Base.Handle ->
  Handle
new cfg baseHandle = do
  let _outputKindList = outputKindList cfg
  let _shouldSkipLink = shouldSkipLink cfg
  let _shouldExecute = shouldExecute cfg
  let _installDir = installDir cfg
  let _executeArgs = executeArgs cfg
  Handle {..}

buildTarget :: Handle -> M.MainModule -> Target -> EIO ()
buildTarget h (M.MainModule baseModule) target = do
  Debug.report (Base.debugHandle (baseHandle h)) $ "Building: " <> T.pack (show target)
  target' <- expandClangOptions target
  unravelHandle <- liftIO $ Unravel.new (baseHandle h)
  (artifactTime, dependenceSeq) <- Unravel.unravel unravelHandle baseModule target'
  let moduleList = nubOrdOn M.moduleID $ map sourceModule dependenceSeq
  didPerformForeignCompilation <- compileForeign h target moduleList
  let loadHandle = Load.new (baseHandle h)
  contentSeq <- Load.load loadHandle target dependenceSeq
  compile h target' (_outputKindList h) contentSeq
  liftIO $
    GlobalRemark.get (Base.globalRemarkHandle (baseHandle h))
      >>= Report.printRemarkList (Base.reportHandle (baseHandle h))
  case target' of
    Peripheral {} ->
      return ()
    PeripheralSingle {} ->
      return ()
    Main ct -> do
      let linkHandle = Link.new' (baseHandle h)
      Link.link linkHandle ct (_shouldSkipLink h) didPerformForeignCompilation artifactTime (toList dependenceSeq)
      execute h (_shouldExecute h) ct (_executeArgs h)
      install h (_installDir h) ct

compile :: Handle -> Target -> [OutputKind] -> [(Source, Either Cache T.Text)] -> EIO ()
compile h target outputKindList contentSeq = do
  let mainModule = Env.getMainModule (Base.envHandle (baseHandle h))
  let cacheHandle = Cache.new' (baseHandle h)
  bs <- mapM (needsCompilation cacheHandle outputKindList . fst) contentSeq
  c <- getEntryPointCompilationCount h mainModule target outputKindList
  let numOfItems = length (filter id bs) + c
  let colorHandle = Base.colorHandle (baseHandle h)
  currentTime <- liftIO getCurrentTime
  color <- do
    if Color.getShouldColorizeStdout colorHandle
      then return [SetColor Foreground Vivid Green]
      else return []
  let workingTitle = getWorkingTitle numOfItems
  let completedTitle = getCompletedTitle numOfItems
  hp <- liftIO $ ProgressBar.new (Base.envHandle (baseHandle h)) colorHandle (Just numOfItems) workingTitle completedTitle color
  let emitHandle = Emit.new (baseHandle h)
  let llvmHandle = LLVM.new' (baseHandle h)
  contentAsync <- fmap catMaybes $ forM contentSeq $ \(source, cacheOrContent) -> do
    localHandle <- Local.new (baseHandle h) source
    let parseHandle = Parse.new (baseHandle h) localHandle
    elaborateHandle <- liftIO $ Elaborate.new (baseHandle h) localHandle source
    let ensureMainHandle = EnsureMain.new (Local.locatorHandle localHandle)
    let suffix = if isLeft cacheOrContent then " (cache found)" else ""
    Debug.report (Base.debugHandle (baseHandle h)) $ "Compiling: " <> T.pack (toFilePath $ sourceFilePath source) <> suffix
    cacheOrStmtList <- Parse.parse parseHandle target source cacheOrContent
    stmtList <- Elaborate.elaborate elaborateHandle target cacheOrStmtList
    EnsureMain.ensureMain ensureMainHandle target source (map snd $ getStmtName stmtList)
    b <- Cache.needsCompilation cacheHandle outputKindList source
    if b
      then do
        clarifyHandle <- liftIO $ Clarify.new' (baseHandle h) localHandle
        stmtList' <- Clarify.clarify clarifyHandle stmtList
        fmap Just $ liftIO $ async $ runEIO $ do
          lowerHandle <- liftIO $ Lower.new (baseHandle h)
          virtualCode <- Lower.lower lowerHandle stmtList'
          emit emitHandle llvmHandle hp currentTime target outputKindList (Right source) virtualCode
      else return Nothing
  entryPointVirtualCode <- compileEntryPoint h mainModule target outputKindList
  entryPointAsync <- forM entryPointVirtualCode $ \(src, code) -> liftIO $ do
    async $ runEIO $ emit emitHandle llvmHandle hp currentTime target outputKindList src code
  errors <- fmap lefts $ mapM wait $ entryPointAsync ++ contentAsync
  liftIO $ ProgressBar.close hp
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
  Emit.Handle ->
  LLVM.Handle ->
  ProgressBar.Handle ->
  UTCTime ->
  Target ->
  [OutputKind] ->
  Either MainTarget Source ->
  LC.LowCode ->
  EIO ()
emit h he progressBar currentTime target outputKindList src code = do
  let clangOptions = getCompileOption target
  llvmIR' <- liftIO $ Emit.emit h code
  LLVM.emit he target clangOptions currentTime src outputKindList llvmIR'
  liftIO $ ProgressBar.increment progressBar

getEntryPointCompilationCount :: Handle -> M.MainModule -> Target -> [OutputKind] -> EIO Int
getEntryPointCompilationCount h mainModule target outputKindList = do
  case target of
    Peripheral {} ->
      return 0
    PeripheralSingle {} ->
      return 0
    Main t -> do
      let pathHandle = Base.pathHandle (baseHandle h)
      b <- Cache.isEntryPointCompilationSkippable pathHandle mainModule t outputKindList
      return $ if b then 0 else 1

compileEntryPoint :: Handle -> M.MainModule -> Target -> [OutputKind] -> EIO [(Either MainTarget Source, LC.LowCode)]
compileEntryPoint h mainModule target outputKindList = do
  case target of
    Peripheral {} ->
      return []
    PeripheralSingle {} ->
      return []
    Main t -> do
      let pathHandle = Base.pathHandle (baseHandle h)
      b <- Cache.isEntryPointCompilationSkippable pathHandle mainModule t outputKindList
      if b
        then return []
        else do
          clarifyMainHandle <- liftIO $ Clarify.newMain (baseHandle h)
          lowerHandle <- liftIO $ Lower.new (baseHandle h)
          mainVirtualCode <-
            liftIO (Clarify.clarifyEntryPoint clarifyMainHandle)
              >>= Lower.lowerEntryPoint lowerHandle t
          return [(Left t, mainVirtualCode)]

execute :: Handle -> Bool -> MainTarget -> [String] -> EIO ()
execute h shouldExecute target args = do
  when shouldExecute $ do
    let executeHandle = Execute.new (baseHandle h)
    Execute.execute executeHandle target args

install :: Handle -> Maybe FilePath -> MainTarget -> EIO ()
install h filePathOrNone target = do
  mDir <- mapM Path.getInstallDir filePathOrNone
  let installHandle = Install.new (baseHandle h)
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
    Debug.report (Base.debugHandle (baseHandle h)) $
      "Performing foreign compilation of `" <> MID.reify (M.moduleID m) <> "` with " <> T.pack (show sub)
  let moduleRootDir = M.getModuleRootDir m
  foreignDir <- Path.getForeignDir (Base.pathHandle (baseHandle h)) t m
  inputPathList <- fmap concat $ mapM (getInputPathList moduleRootDir) $ M.input $ M.moduleForeign m
  let outputPathList = map (foreignDir </>) $ M.output $ M.moduleForeign m
  for_ outputPathList $ \outputPath -> do
    ensureDir $ parent outputPath
  inputTime <- Path.getLastModifiedSup inputPathList
  outputTime <- Path.getLastModifiedInf outputPathList
  case (inputTime, outputTime) of
    (Just t1, Just t2)
      | t1 <= t2 -> do
          Debug.report (Base.debugHandle (baseHandle h)) $
            "Cache found; skipping foreign compilation of `" <> MID.reify (M.moduleID m) <> "`"
          return False
    _ -> do
      let cmdList' = map (naiveReplace sub) cmdList
      forM_ cmdList' $ \c -> do
        let externalHandle = External.new (Base.debugHandle (baseHandle h))
        result <- External.runOrFail' externalHandle moduleRootDir $ T.unpack c
        case result of
          Right _ ->
            return ()
          Left err -> do
            let External.ExternalError {cmd, exitCode, errStr} = err
            raiseError' $
              "Foreign compilation of `"
                <> MID.reify (M.moduleID m)
                <> "` failed at `"
                <> T.pack cmd
                <> "` with the following error (exitcode = "
                <> T.pack (show exitCode)
                <> "):\n"
                <> errStr
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
  clang <- liftIO Clang.getClang
  foreignDir <- Path.getForeignDir (Base.pathHandle (baseHandle h)) t m
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

expandClangOptions :: Target -> EIO Target
expandClangOptions target =
  case target of
    Main concreteTarget ->
      case concreteTarget of
        Named targetName summary -> do
          let cl = clangOption summary
          compileOption' <- expandOptions (CL.compileOption cl)
          linkOption' <- expandOptions (CL.linkOption cl)
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
          compileOption' <- expandOptions (CL.compileOption clangOption)
          linkOption' <- expandOptions (CL.linkOption clangOption)
          return $ Main $ Zen path $ CL.ClangOption {compileOption = compileOption', linkOption = linkOption'}
    Peripheral {} ->
      return target
    PeripheralSingle {} ->
      return target

expandOptions :: [T.Text] -> EIO [T.Text]
expandOptions foo =
  map T.strip <$> mapM External.expandText foo
