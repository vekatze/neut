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
import Move.Context.App
import Move.Context.Cache (needsCompilation)
import Move.Context.Cache qualified as Cache
import Move.Context.Clang qualified as Clang
import Move.Context.Color qualified as Color
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (EIO, forP, raiseError', runEIO, toApp)
import Move.Context.Env qualified as Env
import Move.Context.External qualified as External
import Move.Context.LLVM qualified as LLVM
import Move.Context.Locator qualified as Locator
import Move.Context.Path qualified as Path
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Clarify qualified as Clarify
import Move.Scene.Elaborate qualified as Elaborate
import Move.Scene.Emit qualified as Emit
import Move.Scene.EnsureMain qualified as EnsureMain
import Move.Scene.Execute qualified as Execute
import Move.Scene.Init.Source qualified as InitSource
import Move.Scene.Init.Target qualified as InitTarget
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
  { gensymHandle :: Gensym.Handle,
    debugHandle :: Debug.Handle,
    initTargetHandle :: InitTarget.Handle,
    unravelHandle :: Unravel.Handle,
    loadHandle :: Load.Handle,
    globalRemarkHandle :: GlobalRemark.Handle,
    reportHandle :: Report.Handle,
    envHandle :: Env.Handle,
    locatorHandle :: Locator.Handle,
    cacheHandle :: Cache.Handle,
    colorHandle :: Color.Handle,
    initSourceHandle :: InitSource.Handle,
    pathHandle :: Path.Handle,
    externalHandle :: External.Handle,
    ensureMainHandle :: EnsureMain.Handle,
    parseHandle :: Parse.Handle,
    clarifyHandle :: Clarify.Handle,
    lowerHandle :: Lower.Handle,
    llvmHandle :: LLVM.Handle,
    emitHandle :: Emit.Handle,
    linkHandle :: Link.Handle,
    installHandle :: Install.Handle,
    executeHandle :: Execute.Handle,
    elaborateConfig :: Elaborate.Config,
    _outputKindList :: [OutputKind],
    _shouldSkipLink :: Bool,
    _shouldExecute :: Bool,
    _installDir :: Maybe FilePath,
    _executeArgs :: [String]
  }

new ::
  Config ->
  Gensym.Handle ->
  Debug.Handle ->
  InitTarget.Handle ->
  Unravel.Handle ->
  Load.Handle ->
  GlobalRemark.Handle ->
  Report.Handle ->
  Env.Handle ->
  Locator.Handle ->
  Cache.Handle ->
  Color.Handle ->
  InitSource.Handle ->
  Path.Handle ->
  External.Handle ->
  EnsureMain.Handle ->
  Parse.Handle ->
  Clarify.Handle ->
  Lower.Handle ->
  LLVM.Handle ->
  Emit.Handle ->
  Link.Handle ->
  Install.Handle ->
  Execute.Handle ->
  Elaborate.Config ->
  Handle
new cfg gensymHandle debugHandle initTargetHandle unravelHandle loadHandle globalRemarkHandle reportHandle envHandle locatorHandle cacheHandle colorHandle initSourceHandle pathHandle externalHandle ensureMainHandle parseHandle clarifyHandle lowerHandle llvmHandle emitHandle linkHandle installHandle executeHandle elaborateConfig = do
  let _outputKindList = outputKindList cfg
  let _shouldSkipLink = shouldSkipLink cfg
  let _shouldExecute = shouldExecute cfg
  let _installDir = installDir cfg
  let _executeArgs = executeArgs cfg
  Handle {..}

buildTarget :: Handle -> M.MainModule -> Target -> App ()
buildTarget h (M.MainModule baseModule) target = do
  toApp $ Debug.report (debugHandle h) $ "Building: " <> T.pack (show target)
  target' <- toApp $ expandClangOptions target
  liftIO $ InitTarget.initializeForTarget (initTargetHandle h)
  (artifactTime, dependenceSeq) <- toApp $ Unravel.unravel (unravelHandle h) baseModule target'
  let moduleList = nubOrdOn M.moduleID $ map sourceModule dependenceSeq
  didPerformForeignCompilation <- toApp $ compileForeign h target moduleList
  contentSeq <- toApp $ Load.load (loadHandle h) target dependenceSeq
  toApp $ compile h target' (_outputKindList h) contentSeq
  liftIO $ GlobalRemark.get (globalRemarkHandle h) >>= Report.printRemarkList (reportHandle h)
  case target' of
    Peripheral {} ->
      return ()
    PeripheralSingle {} ->
      return ()
    Main ct -> do
      toApp $ Link.link (linkHandle h) ct (_shouldSkipLink h) didPerformForeignCompilation artifactTime (toList dependenceSeq)
      toApp $ execute h (_shouldExecute h) ct (_executeArgs h)
      toApp $ install h (_installDir h) ct

compile :: Handle -> Target -> [OutputKind] -> [(Source, Either Cache T.Text)] -> EIO ()
compile h target outputKindList contentSeq = do
  mainModule <- Env.getMainModule (envHandle h)
  bs <- mapM (needsCompilation (cacheHandle h) outputKindList . fst) contentSeq
  c <- getEntryPointCompilationCount h mainModule target outputKindList
  let numOfItems = length (filter id bs) + c
  currentTime <- liftIO getCurrentTime
  color <- do
    shouldColorize <- liftIO $ Color.getShouldColorizeStdout (colorHandle h)
    if shouldColorize
      then return [SetColor Foreground Vivid Green]
      else return []
  let workingTitle = getWorkingTitle numOfItems
  let completedTitle = getCompletedTitle numOfItems
  hp <- liftIO $ ProgressBar.new (envHandle h) (colorHandle h) (Just numOfItems) workingTitle completedTitle color
  contentAsync <- fmap catMaybes $ forM contentSeq $ \(source, cacheOrContent) -> do
    InitSource.initializeForSource (initSourceHandle h) source
    let suffix = if isLeft cacheOrContent then " (cache found)" else ""
    Debug.report (debugHandle h) $ "Compiling: " <> T.pack (toFilePath $ sourceFilePath source) <> suffix
    cacheOrStmtList <- Parse.parse (parseHandle h) target source cacheOrContent
    hElaborate <- liftIO $ Elaborate.new (elaborateConfig h) source
    stmtList <- Elaborate.elaborate hElaborate target cacheOrStmtList
    EnsureMain.ensureMain (ensureMainHandle h) target source (map snd $ getStmtName stmtList)
    b <- Cache.needsCompilation (cacheHandle h) outputKindList source
    if b
      then do
        stmtList' <- Clarify.clarify (clarifyHandle h) stmtList
        fmap Just $ liftIO $ async $ runEIO $ do
          virtualCode <- Lower.lower (lowerHandle h) stmtList'
          emit (emitHandle h) (llvmHandle h) hp currentTime target outputKindList (Right source) virtualCode
      else return Nothing
  entryPointVirtualCode <- compileEntryPoint h mainModule target outputKindList
  entryPointAsync <- forM entryPointVirtualCode $ \(src, code) -> liftIO $ do
    async $ runEIO $ emit (emitHandle h) (llvmHandle h) hp currentTime target outputKindList src code
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
      b <- Cache.isEntryPointCompilationSkippable (pathHandle h) mainModule t outputKindList
      return $ if b then 0 else 1

compileEntryPoint :: Handle -> M.MainModule -> Target -> [OutputKind] -> EIO [(Either MainTarget Source, LC.LowCode)]
compileEntryPoint h mainModule target outputKindList = do
  case target of
    Peripheral {} ->
      return []
    PeripheralSingle {} ->
      return []
    Main t -> do
      b <- Cache.isEntryPointCompilationSkippable (pathHandle h) mainModule t outputKindList
      if b
        then return []
        else do
          mainVirtualCode <- liftIO (Clarify.clarifyEntryPoint (clarifyHandle h)) >>= Lower.lowerEntryPoint (lowerHandle h) t
          return [(Left t, mainVirtualCode)]

execute :: Handle -> Bool -> MainTarget -> [String] -> EIO ()
execute h shouldExecute target args = do
  when shouldExecute $ do
    Execute.execute (executeHandle h) target args

install :: Handle -> Maybe FilePath -> MainTarget -> EIO ()
install h filePathOrNone target = do
  mDir <- mapM Path.getInstallDir filePathOrNone
  mapM_ (Install.install (installHandle h) target) mDir

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
    Debug.report (debugHandle h) $
      "Performing foreign compilation of `" <> MID.reify (M.moduleID m) <> "` with " <> T.pack (show sub)
  let moduleRootDir = M.getModuleRootDir m
  foreignDir <- Path.getForeignDir (pathHandle h) t m
  inputPathList <- fmap concat $ mapM (getInputPathList moduleRootDir) $ M.input $ M.moduleForeign m
  let outputPathList = map (foreignDir </>) $ M.output $ M.moduleForeign m
  for_ outputPathList $ \outputPath -> do
    ensureDir $ parent outputPath
  inputTime <- Path.getLastModifiedSup inputPathList
  outputTime <- Path.getLastModifiedInf outputPathList
  case (inputTime, outputTime) of
    (Just t1, Just t2)
      | t1 <= t2 -> do
          Debug.report (debugHandle h) $
            "Cache found; skipping foreign compilation of `" <> MID.reify (M.moduleID m) <> "`"
          return False
    _ -> do
      let cmdList' = map (naiveReplace sub) cmdList
      forM_ cmdList' $ \c -> do
        result <- External.runOrFail' (externalHandle h) moduleRootDir $ T.unpack c
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
  foreignDir <- Path.getForeignDir (pathHandle h) t m
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
