module Scene.Build
  ( buildTarget,
    Axis (..),
    abstractAxis,
  )
where

import Context.App
import Context.Cache qualified as Cache
import Context.Env qualified as Env
import Context.External qualified as External
import Context.LLVM qualified as LLVM
import Context.Path qualified as Path
import Context.Remark qualified as Remark
import Context.Throw qualified as Throw
import Control.Monad
import Control.Monad.IO.Class
import Data.Containers.ListUtils (nubOrdOn)
import Data.Foldable
import Data.Maybe
import Data.Text qualified as T
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
  target' <- expandClangOptions target
  Initialize.initializeForTarget
  (artifactTime, dependenceSeq) <- Unravel.unravel baseModule target'
  let moduleList = nubOrdOn M.moduleID $ map sourceModule dependenceSeq
  didPerformForeignCompilation <- compileForeign target moduleList
  contentSeq <- load target dependenceSeq
  virtualCodeList <- compile target' (_outputKindList axis) contentSeq
  Remark.getGlobalRemarkList >>= Remark.printRemarkList
  emitAndWrite target' (_outputKindList axis) virtualCodeList
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

load :: Target -> [Source] -> App [(Source, Either Cache T.Text)]
load t dependenceSeq =
  pooledForConcurrently dependenceSeq $ \source -> do
    cacheOrContent <- Load.load t source
    return (source, cacheOrContent)

compile :: Target -> [OutputKind] -> [(Source, Either Cache T.Text)] -> App [(Either MainTarget Source, LC.LowCode)]
compile target outputKindList contentSeq = do
  virtualCodeList <- fmap catMaybes $ forM contentSeq $ \(source, cacheOrContent) -> do
    Initialize.initializeForSource source
    stmtList <- Parse.parse target source cacheOrContent >>= Elaborate.elaborate target
    EnsureMain.ensureMain target source (map snd $ getStmtName stmtList)
    Cache.whenCompilationNecessary outputKindList source $ do
      virtualCode <- Clarify.clarify stmtList >>= Lower.lower
      return (Right source, virtualCode)
  mainModule <- Env.getMainModule
  entryPointVirtualCode <- compileEntryPoint mainModule target outputKindList
  return $ entryPointVirtualCode ++ virtualCodeList

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

emitAndWrite :: Target -> [OutputKind] -> [(Either MainTarget Source, LC.LowCode)] -> App ()
emitAndWrite target outputKindList virtualCodeList = do
  let clangOptions = getCompileOption target
  currentTime <- liftIO getCurrentTime
  pooledForConcurrently_ virtualCodeList $ \(sourceOrNone, llvmIR) -> do
    llvmIR' <- Emit.emit llvmIR
    LLVM.emit target clangOptions currentTime sourceOrNone outputKindList llvmIR'

execute :: Bool -> MainTarget -> [String] -> App ()
execute shouldExecute target args = do
  when shouldExecute $ Execute.execute target args

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
          return False
    _ -> do
      let cmdList' = map (naiveReplace sub) cmdList
      forM_ cmdList' $ \c -> do
        result <- External.runOrFail' moduleRootDir $ T.unpack c
        case result of
          Right _ -> do
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
