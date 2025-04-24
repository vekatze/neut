module Move.Scene.Build
  ( buildTarget,
    Axis (..),
    abstractAxis,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Containers.ListUtils (nubOrdOn)
import Data.Either (isLeft)
import Data.Foldable
import Data.Maybe
import Data.Text qualified as T
import Data.Time
import Move.Context.App
import Move.Context.Cache (needsCompilation)
import Move.Context.Cache qualified as Cache
import Move.Context.Clang qualified as Clang
import Move.Context.Color qualified as Color
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (EIO, toApp)
import Move.Context.Elaborate qualified as Elaborate
import Move.Context.Env qualified as Env
import Move.Context.External qualified as External
import Move.Context.LLVM qualified as LLVM
import Move.Context.Path qualified as Path
import Move.Context.Remark qualified as Remark
import Move.Context.Throw qualified as Throw
import Move.Scene.Clarify qualified as Clarify
import Move.Scene.Elaborate qualified as Elaborate
import Move.Scene.Emit qualified as Emit
import Move.Scene.EnsureMain qualified as EnsureMain
import Move.Scene.Execute qualified as Execute
import Move.Scene.Initialize qualified as Initialize
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

data Axis = Axis
  { _outputKindList :: [OutputKind],
    _shouldSkipLink :: Bool,
    _shouldExecute :: Bool,
    _installDir :: Maybe FilePath,
    _executeArgs :: [String]
  }

buildTarget :: Axis -> M.MainModule -> Target -> App ()
buildTarget axis (M.MainModule baseModule) target = do
  h <- Debug.new
  toApp $ Debug.report h $ "Building: " <> T.pack (show target)
  target' <- expandClangOptions target
  Initialize.initializeForTarget
  h' <- Unravel.new
  (artifactTime, dependenceSeq) <- toApp $ Unravel.unravel h' baseModule target'
  let moduleList = nubOrdOn M.moduleID $ map sourceModule dependenceSeq
  didPerformForeignCompilation <- compileForeign target moduleList
  h'' <- Load.new
  contentSeq <- toApp $ Load.load h'' target dependenceSeq
  compile target' (_outputKindList axis) contentSeq
  hgl <- GlobalRemark.new
  liftIO (GlobalRemark.get hgl) >>= Remark.printRemarkList
  case target' of
    Peripheral {} ->
      return ()
    PeripheralSingle {} ->
      return ()
    Main ct -> do
      Link.link ct (_shouldSkipLink axis) didPerformForeignCompilation artifactTime (toList dependenceSeq)
      execute (_shouldExecute axis) ct (_executeArgs axis)
      install (_installDir axis) ct

abstractAxis :: Axis
abstractAxis =
  Axis
    { _outputKindList = [Object],
      _shouldSkipLink = True,
      _shouldExecute = False,
      _installDir = Nothing,
      _executeArgs = []
    }

compile :: Target -> [OutputKind] -> [(Source, Either Cache T.Text)] -> App ()
compile target outputKindList contentSeq = do
  mainModule <- Env.getMainModule
  hCache <- Cache.new
  bs <- mapM (needsCompilation hCache outputKindList . fst) contentSeq
  c <- getEntryPointCompilationCount mainModule target outputKindList
  let numOfItems = length (filter id bs) + c
  currentTime <- liftIO getCurrentTime
  color <- do
    shouldColorize <- Color.getShouldColorizeStdout
    if shouldColorize
      then return [SetColor Foreground Vivid Green]
      else return []
  let workingTitle = getWorkingTitle numOfItems
  let completedTitle = getCompletedTitle numOfItems
  h <- ProgressBar.new (Just numOfItems) workingTitle completedTitle color
  contentAsync <- fmap catMaybes $ forM contentSeq $ \(source, cacheOrContent) -> do
    Initialize.initializeForSource source
    let suffix = if isLeft cacheOrContent then " (cache found)" else ""
    h' <- Debug.new
    toApp $ Debug.report h' $ "Compiling: " <> T.pack (toFilePath $ sourceFilePath source) <> suffix
    hParse <- Parse.new
    cacheOrStmtList <- toApp $ Parse.parse hParse target source cacheOrContent
    hEnv <- liftIO Elaborate.createNewEnv
    hElaborate <- Elaborate.new hEnv
    stmtList <- toApp $ Elaborate.elaborate hElaborate target cacheOrStmtList
    EnsureMain.ensureMain target source (map snd $ getStmtName stmtList)
    Cache.whenCompilationNecessary hCache outputKindList source $ do
      stmtList' <- Clarify.clarify stmtList
      async $ do
        virtualCode <- Lower.lower stmtList'
        emit h currentTime target outputKindList (Right source) virtualCode
  entryPointVirtualCode <- compileEntryPoint mainModule target outputKindList
  entryPointAsync <- forM entryPointVirtualCode $ \(src, code) -> async $ do
    emit h currentTime target outputKindList src code
  mapM_ wait $ entryPointAsync ++ contentAsync
  ProgressBar.close h

getCompletedTitle :: Int -> T.Text
getCompletedTitle numOfItems = do
  let suffix = if numOfItems <= 1 then "" else "s"
  "Compiled " <> T.pack (show numOfItems) <> " file" <> suffix

getWorkingTitle :: Int -> T.Text
getWorkingTitle numOfItems = do
  let suffix = if numOfItems <= 1 then "" else "s"
  "Compiling " <> T.pack (show numOfItems) <> " file" <> suffix

emit ::
  ProgressBar.Handle ->
  UTCTime ->
  Target ->
  [OutputKind] ->
  Either MainTarget Source ->
  LC.LowCode ->
  App ()
emit progressBar currentTime target outputKindList src code = do
  let clangOptions = getCompileOption target
  llvmIR' <- Emit.emit code
  LLVM.emit target clangOptions currentTime src outputKindList llvmIR'
  ProgressBar.increment progressBar

getEntryPointCompilationCount :: M.MainModule -> Target -> [OutputKind] -> App Int
getEntryPointCompilationCount mainModule target outputKindList = do
  case target of
    Peripheral {} ->
      return 0
    PeripheralSingle {} ->
      return 0
    Main t -> do
      h <- Path.new
      b <- toApp $ Cache.isEntryPointCompilationSkippable h mainModule t outputKindList
      return $ if b then 0 else 1

compileEntryPoint :: M.MainModule -> Target -> [OutputKind] -> App [(Either MainTarget Source, LC.LowCode)]
compileEntryPoint mainModule target outputKindList = do
  case target of
    Peripheral {} ->
      return []
    PeripheralSingle {} ->
      return []
    Main t -> do
      h <- Path.new
      b <- toApp $ Cache.isEntryPointCompilationSkippable h mainModule t outputKindList
      if b
        then return []
        else do
          mainVirtualCode <- Clarify.clarifyEntryPoint >>= Lower.lowerEntryPoint t
          return [(Left t, mainVirtualCode)]

execute :: Bool -> MainTarget -> [String] -> App ()
execute shouldExecute target args = do
  when shouldExecute $ do
    Execute.execute target args

install :: Maybe FilePath -> MainTarget -> App ()
install filePathOrNone target = do
  mDir <- mapM (toApp . Path.getInstallDir) filePathOrNone
  mapM_ (Install.install target) mDir

compileForeign :: Target -> [M.Module] -> App Bool
compileForeign t moduleList = do
  currentTime <- liftIO getCurrentTime
  bs <- pooledForConcurrently moduleList (compileForeign' t currentTime)
  return $ or bs

compileForeign' :: Target -> UTCTime -> M.Module -> App Bool
compileForeign' t currentTime m = do
  sub <- getForeignSubst t m
  let cmdList = M.script $ M.moduleForeign m
  unless (null cmdList) $ do
    h <- Debug.new
    toApp $
      Debug.report h $
        "Performing foreign compilation of `" <> MID.reify (M.moduleID m) <> "` with " <> T.pack (show sub)
  let moduleRootDir = M.getModuleRootDir m
  h <- Path.new
  foreignDir <- toApp $ Path.getForeignDir h t m
  inputPathList <- fmap concat $ mapM (toApp . getInputPathList moduleRootDir) $ M.input $ M.moduleForeign m
  let outputPathList = map (foreignDir </>) $ M.output $ M.moduleForeign m
  for_ outputPathList $ \outputPath -> do
    ensureDir $ parent outputPath
  inputTime <- toApp $ Path.getLastModifiedSup inputPathList
  outputTime <- toApp $ Path.getLastModifiedInf outputPathList
  case (inputTime, outputTime) of
    (Just t1, Just t2)
      | t1 <= t2 -> do
          h' <- Debug.new
          toApp $ Debug.report h' $ "Cache found; skipping foreign compilation of `" <> MID.reify (M.moduleID m) <> "`"
          return False
    _ -> do
      let cmdList' = map (naiveReplace sub) cmdList
      forM_ cmdList' $ \c -> do
        result <- External.runOrFail' moduleRootDir $ T.unpack c
        case result of
          Right _ ->
            return ()
          Left err -> do
            let External.ExternalError {cmd, exitCode, errStr} = err
            Throw.raiseError' $
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
          else Throw.raiseError' $ "Missing foreign output: " <> T.pack (toFilePath outputPath)
      return $ not $ null cmdList

naiveReplace :: [(T.Text, T.Text)] -> T.Text -> T.Text
naiveReplace sub t =
  case sub of
    [] ->
      t
    (from, to) : rest -> do
      T.replace from to (naiveReplace rest t)

getForeignSubst :: Target -> M.Module -> App [(T.Text, T.Text)]
getForeignSubst t m = do
  clang <- liftIO Clang.getClang
  h <- Path.new
  foreignDir <- toApp $ Path.getForeignDir h t m
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

expandClangOptions :: Target -> App Target
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

expandOptions :: [T.Text] -> App [T.Text]
expandOptions foo =
  map T.strip <$> mapM External.expandText foo
