module Scene.Build
  ( buildTarget,
    Axis (..),
    abstractAxis,
  )
where

import Context.App
import Context.Cache qualified as Cache
import Context.Debug (report)
import Context.Env qualified as Env
import Context.External qualified as External
import Context.LLVM qualified as LLVM
import Context.Path qualified as Path
import Context.Remark qualified as Remark
import Context.Throw qualified as Throw
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString qualified as B
import Data.Containers.ListUtils (nubOrdOn)
import Data.Either (isLeft, isRight)
import Data.Foldable
import Data.IORef (IORef, atomicModifyIORef, newIORef)
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Entity.Cache
import Entity.ClangOption qualified as CL
import Entity.LowComp qualified as LC
import Entity.Module qualified as M
import Entity.ModuleID qualified as MID
import Entity.OutputKind
import Entity.Source
import Entity.Stmt (getStmtName)
import Entity.Target
import Path
import Scene.Clarify qualified as Clarify
import Scene.Elaborate qualified as Elaborate
import Scene.Emit qualified as Emit
import Scene.EnsureMain qualified as EnsureMain
import Scene.Execute qualified as Execute
import Scene.Initialize qualified as Initialize
import Scene.Install qualified as Install
import Scene.Link qualified as Link
import Scene.Load qualified as Load
import Scene.Lower qualified as Lower
import Scene.Parse qualified as Parse
import Scene.Unravel qualified as Unravel
import System.Console.ANSI
import System.IO (stdout)
import UnliftIO.Async
import Prelude hiding (log)

data Axis = Axis
  { _outputKindList :: [OutputKind],
    _shouldSkipLink :: Bool,
    _shouldExecute :: Bool,
    _installDir :: Maybe FilePath,
    _executeArgs :: [String]
  }

buildTarget :: Axis -> M.Module -> Target -> App ()
buildTarget axis baseModule target = do
  report $ "Building: " <> T.pack (show target)
  target' <- expandClangOptions target
  Initialize.initializeForTarget
  (artifactTime, dependenceSeq) <- Unravel.unravel baseModule target'
  let moduleList = nubOrdOn M.moduleID $ map sourceModule dependenceSeq
  didPerformForeignCompilation <- compileForeign target moduleList
  contentSeq <- Load.load target dependenceSeq
  compile target' (_outputKindList axis) contentSeq
  Remark.getGlobalRemarkList >>= Remark.printRemarkList
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
  entryPointVirtualCode <- compileEntryPoint mainModule target outputKindList
  let numOfItems = length (filter (isRight . snd) contentSeq) + length entryPointVirtualCode
  if numOfItems == 0
    then return ()
    else do
      currentTime <- liftIO getCurrentTime
      ref <- liftIO $ newIORef (1 :: Int)
      entryPointConc <- forM entryPointVirtualCode $ \(src, code) -> async $ do
        emit currentTime target ref numOfItems outputKindList src code
      initializeProgressBar numOfItems
      contentConc <- fmap catMaybes $ forM contentSeq $ \(source, cacheOrContent) -> do
        Initialize.initializeForSource source
        let suffix = if isLeft cacheOrContent then " (cache found)" else ""
        report $ "Compiling: " <> T.pack (toFilePath $ sourceFilePath source) <> suffix
        cacheOrStmtList <- Parse.parse target source cacheOrContent
        stmtList <- Elaborate.elaborate target cacheOrStmtList
        EnsureMain.ensureMain target source (map snd $ getStmtName stmtList)
        Cache.whenCompilationNecessary outputKindList source $ do
          stmtList' <- Clarify.clarify stmtList
          virtualCode <- Lower.lower stmtList'
          async $ emit currentTime target ref numOfItems outputKindList (Right source) virtualCode
      mapM_ wait $ entryPointConc ++ contentConc
      finalizeProgressBar $ "Compiled " <> T.pack (show numOfItems) <> " files"

emit :: UTCTime -> Target -> IORef Int -> Int -> [OutputKind] -> Either MainTarget Source -> LC.LowCode -> App ()
emit currentTime target ref numOfItems outputKindList src code = do
  let clangOptions = getCompileOption target
  llvmIR' <- Emit.emit code
  LLVM.emit target clangOptions currentTime src outputKindList llvmIR'
  finishItem ref numOfItems

finishItem :: IORef Int -> Int -> App ()
finishItem ref numOfItems = do
  value <- liftIO $ atomicModifyIORef ref (\x -> (x + 1, x))
  renderProgressBar "Compiling" value numOfItems

compileEntryPoint :: M.Module -> Target -> [OutputKind] -> App [(Either MainTarget Source, LC.LowCode)]
compileEntryPoint mainModule target outputKindList = do
  case target of
    Peripheral {} ->
      return []
    PeripheralSingle {} ->
      return []
    Main t -> do
      b <- Cache.isEntryPointCompilationSkippable mainModule t outputKindList
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
  mDir <- mapM Path.getInstallDir filePathOrNone
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
    report $ "Performing foreign compilation of `" <> MID.reify (M.moduleID m) <> "` with " <> T.pack (show sub)
  let moduleRootDir = M.getModuleRootDir m
  foreignDir <- Path.getForeignDir t m
  inputPathList <- fmap concat $ mapM (getInputPathList moduleRootDir) $ M.input $ M.moduleForeign m
  let outputPathList = map (foreignDir </>) $ M.output $ M.moduleForeign m
  for_ outputPathList $ \outputPath -> do
    Path.ensureDir $ parent outputPath
  inputTime <- Path.getLastModifiedSup inputPathList
  outputTime <- Path.getLastModifiedInf outputPathList
  case (inputTime, outputTime) of
    (Just t1, Just t2)
      | t1 <= t2 -> do
          report $ "Cache found; skipping foreign compilation of `" <> MID.reify (M.moduleID m) <> "`"
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
        b <- Path.doesFileExist outputPath
        if b
          then Path.setModificationTime outputPath currentTime
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
  clang <- liftIO External.getClang
  foreignDir <- Path.getForeignDir t m
  return
    [ ("{{module-root}}", T.pack $ toFilePath $ M.getModuleRootDir m),
      ("{{clang}}", T.pack clang),
      ("{{foreign}}", T.pack $ toFilePath foreignDir)
    ]

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

renderProgressBar :: T.Text -> Int -> Int -> App ()
renderProgressBar title current size = do
  let frac :: Double = fromIntegral current / fromIntegral size
  let pivot = floor $ fromIntegral barLength * frac
  spinner <- Remark.withSGR [SetColor Foreground Vivid Green] $ chooseSpinner current
  let title' = spinner <> " " <> title
  prefix <- Remark.withSGR [SetColor Foreground Vivid Green] $ T.replicate pivot barFinished
  let suffix = T.replicate (barLength - pivot) barInProgress
  let bar = prefix <> suffix
  let content = "\r" <> title' <> ": " <> bar <> " " <> T.pack (show current) <> "/" <> T.pack (show size)
  liftIO $ B.hPutStr stdout $ encodeUtf8 content

barLength :: Int
barLength =
  32

barInProgress :: T.Text
barInProgress =
  -- "░"
  -- "─"
  " "

barFinished :: T.Text
barFinished =
  -- "█"
  "━"

initializeProgressBar :: Int -> App ()
initializeProgressBar numOfItems = do
  renderProgressBar "Compiling" 0 numOfItems

chooseSpinner :: Int -> T.Text
chooseSpinner i = do
  case i `rem` 10 of
    0 -> "⠋"
    1 -> "⠙"
    2 -> "⠹"
    3 -> "⠸"
    4 -> "⠼"
    5 -> "⠴"
    6 -> "⠦"
    7 -> "⠧"
    8 -> "⠇"
    _ -> "⠏"

finalizeProgressBar :: T.Text -> App ()
finalizeProgressBar title = do
  check <- Remark.withSGR [SetColor Foreground Vivid Green] "✓"
  let title' = check <> " " <> title
  liftIO $ B.hPutStr stdout $ encodeUtf8 "\r"
  liftIO clearFromCursorToLineEnd
  liftIO $ B.hPutStr stdout $ encodeUtf8 $ "\r" <> title' <> "\n"
