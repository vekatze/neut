module Act.Build
  ( build,
    buildTarget,
    Axis (..),
  )
where

import Context.App
import Context.Cache qualified as Cache
import Context.Env qualified as Env
import Context.LLVM qualified as LLVM
import Context.Module qualified as Module
import Context.Path qualified as Path
import Context.Remark qualified as Remark
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Maybe
import Data.Text qualified as T
import Data.Time
import Entity.Cache
import Entity.Config.Build
import Entity.LowComp qualified as LC
import Entity.OutputKind
import Entity.Source
import Entity.Target (Target)
import Scene.Clarify qualified as Clarify
import Scene.Collect qualified as Collect
import Scene.Elaborate qualified as Elaborate
import Scene.Emit qualified as Emit
import Scene.Execute qualified as Execute
import Scene.Fetch qualified as Fetch
import Scene.Initialize qualified as Initialize
import Scene.Install qualified as Install
import Scene.Link qualified as Link
import Scene.Load qualified as Load
import Scene.Lower qualified as Lower
import Scene.Parse qualified as Parse
import Scene.Unravel qualified as Unravel
import UnliftIO.Async
import Prelude hiding (log)

build :: Config -> App ()
build cfg = do
  setup cfg
  targetList <- Collect.collectTargetList $ mTarget cfg
  forM_ targetList $ buildTarget (fromConfig cfg)

data Axis = Axis
  { _outputKindList :: [OutputKind],
    _shouldSkipLink :: Bool,
    _shouldExecute :: Bool,
    _installDir :: Maybe FilePath,
    _executeArgs :: [String]
  }

fromConfig :: Config -> Axis
fromConfig cfg =
  Axis
    { _outputKindList = outputKindList cfg,
      _shouldSkipLink = shouldSkipLink cfg,
      _shouldExecute = shouldExecute cfg,
      _installDir = installDir cfg,
      _executeArgs = args cfg
    }

buildTarget :: Axis -> Target -> App ()
buildTarget axis target = do
  Initialize.initializeForTarget
  (artifactTime, dependenceSeq) <- Unravel.unravel target
  contentSeq <- load dependenceSeq
  virtualCodeList <- compile target (_outputKindList axis) contentSeq
  Remark.getGlobalRemarkList >>= Remark.printRemarkList
  emitAndWrite target (_outputKindList axis) virtualCodeList
  Link.link target (_shouldSkipLink axis) artifactTime (toList dependenceSeq)
  execute (_shouldExecute axis) target (_executeArgs axis)
  install (_installDir axis) target

setup :: Config -> App ()
setup cfg = do
  LLVM.ensureSetupSanity cfg
  Path.ensureNotInLibDir
  Initialize.initializeCompiler (remarkCfg cfg) (mClangOptString cfg)
  Env.setBuildMode $ buildMode cfg
  Module.getMainModule >>= Fetch.fetch

load :: [Source] -> App [(Source, Either Cache T.Text)]
load dependenceSeq =
  forConcurrently dependenceSeq $ \source -> do
    cacheOrContent <- Load.load source
    return (source, cacheOrContent)

compile :: Target -> [OutputKind] -> [(Source, Either Cache T.Text)] -> App [(Maybe Source, LC.LowCode)]
compile target outputKindList contentSeq = do
  virtualCodeList <- fmap catMaybes $ forM contentSeq $ \(source, cacheOrContent) -> do
    Initialize.initializeForSource source
    stmtList <- Parse.parse source cacheOrContent >>= Elaborate.elaborate
    Cache.whenCompilationNecessary outputKindList source $ do
      virtualCode <- Clarify.clarify stmtList >>= Lower.lower
      return (Just source, virtualCode)
  mainModule <- Module.getMainModule
  b <- Cache.isEntryPointCompilationSkippable mainModule target outputKindList
  if b
    then return virtualCodeList
    else do
      mainVirtualCode <- Clarify.clarifyEntryPoint >>= Lower.lowerEntryPoint target
      return $ (Nothing, mainVirtualCode) : virtualCodeList

emitAndWrite :: Target -> [OutputKind] -> [(Maybe Source, LC.LowCode)] -> App ()
emitAndWrite target outputKindList virtualCodeList = do
  currentTime <- liftIO getCurrentTime
  forConcurrently_ virtualCodeList $ \(sourceOrNone, llvmIR) -> do
    llvmIR' <- Emit.emit llvmIR
    LLVM.emit target currentTime sourceOrNone outputKindList llvmIR'

execute :: Bool -> Target -> [String] -> App ()
execute shouldExecute target args = do
  when shouldExecute $ Execute.execute target args

install :: Maybe FilePath -> Target -> App ()
install filePathOrNone target = do
  mDir <- mapM Path.getInstallDir filePathOrNone
  mapM_ (Install.install target) mDir
