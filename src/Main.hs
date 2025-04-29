module Main (main) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Act.Archive qualified as Archive
import Move.Act.Build qualified as Build
import Move.Act.Check qualified as Check
import Move.Act.Clean qualified as Clean
import Move.Act.Create qualified as Create
import Move.Act.Format qualified as Format
import Move.Act.Get qualified as Get
import Move.Act.LSP qualified as LSP
import Move.Act.Version qualified as Version
import Move.Act.Zen qualified as Zen
import Move.Console.EnsureExecutables (ensureExecutables)
import Move.Console.Report qualified as Report
import Move.Context.Antecedent qualified as Antecedent
import Move.Context.App
import Move.Context.Cache qualified as Cache
import Move.Context.Color qualified as Color
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Context.External qualified as External
import Move.Context.Global qualified as Global
import Move.Context.KeyArg qualified as KeyArg
import Move.Context.LLVM qualified as LLVM
import Move.Context.Locator qualified as Locator
import Move.Context.OptParse qualified as OptParse
import Move.Context.OptimizableData qualified as OptimizableData
import Move.Context.Path qualified as Path
import Move.Context.Tag qualified as Tag
import Move.Context.Throw qualified as Throw
import Move.Context.Type qualified as Type
import Move.Context.Unused qualified as Unused
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Archive qualified as SceneArchive
import Move.Scene.Build qualified as SceneBuild
import Move.Scene.Check qualified as SceneCheck
import Move.Scene.Clarify qualified as Clarify
import Move.Scene.Clean qualified as SceneClean
import Move.Scene.Collect qualified as Collect
import Move.Scene.Elaborate qualified as Elaborate
import Move.Scene.Emit qualified as Emit
import Move.Scene.Ens.Reflect qualified as EnsReflect
import Move.Scene.EnsureMain qualified as EnsureMain
import Move.Scene.Execute qualified as Execute
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Format qualified as SceneFormat
import Move.Scene.Init.Compiler qualified as InitCompiler
import Move.Scene.Init.Logger qualified as InitLogger
import Move.Scene.Init.Source qualified as InitSource
import Move.Scene.Init.Target qualified as InitTarget
import Move.Scene.Install qualified as Install
import Move.Scene.LSP qualified as L
import Move.Scene.LSP.Format qualified as LSPFormat
import Move.Scene.Link qualified as Link
import Move.Scene.Load qualified as Load
import Move.Scene.Module.Save qualified as ModuleSave
import Move.Scene.New qualified as New
import Move.Scene.PackageVersion.ChooseNewVersion qualified as PV
import Move.Scene.Parse qualified as Parse
import Move.Scene.Parse.Discern.Handle qualified as Discern
import Move.Scene.Unravel qualified as Unravel
import Move.UI.Handle.GlobalRemark qualified as GlobalRemark
import Rule.Command qualified as C
import System.IO

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  Main.execute

execute :: IO ()
execute = do
  runApp $ do
    gensymHandle <- liftIO Gensym.new
    envHandle <- liftIO Env.new
    tagHandle <- liftIO Tag.new
    locatorHandle <- liftIO $ Locator.new envHandle tagHandle
    antecedentHandle <- liftIO Antecedent.new
    colorHandle <- liftIO Color.new
    reportHandle <- liftIO $ Report.new colorHandle
    debugHandle <- liftIO $ Debug.new colorHandle
    keyArgHandle <- liftIO $ KeyArg.new envHandle
    optDataHandle <- liftIO OptimizableData.new
    unusedHandle <- liftIO Unused.new
    globalHandle <- liftIO $ Global.new envHandle locatorHandle optDataHandle keyArgHandle unusedHandle tagHandle
    typeHandle <- liftIO Type.new
    collectHandle <- Collect.new envHandle
    formatHandle <- SceneFormat.new envHandle gensymHandle debugHandle locatorHandle globalHandle optDataHandle keyArgHandle unusedHandle tagHandle antecedentHandle typeHandle
    lspFormatHandle <- LSPFormat.new formatHandle
    fetchHandle <- Fetch.new envHandle gensymHandle reportHandle debugHandle
    unravelHandle <- Unravel.new envHandle gensymHandle debugHandle locatorHandle globalHandle unusedHandle tagHandle antecedentHandle
    discernHandle <- Discern.new envHandle gensymHandle locatorHandle globalHandle optDataHandle keyArgHandle unusedHandle tagHandle antecedentHandle
    initLoggerHandle <- InitLogger.new envHandle colorHandle reportHandle debugHandle
    initCompilerHandle <- InitCompiler.new envHandle gensymHandle colorHandle reportHandle debugHandle
    initTargetHandle <- InitTarget.new envHandle gensymHandle debugHandle locatorHandle globalHandle optDataHandle unusedHandle tagHandle antecedentHandle typeHandle
    externalHandle <- External.new debugHandle
    moduleSaveHandle <- ModuleSave.new debugHandle
    loadHandle <- Load.new envHandle debugHandle
    globalRemarkHandle <- GlobalRemark.new
    cacheHandle <- Cache.new envHandle debugHandle
    initSourceHandle <- InitSource.new envHandle locatorHandle globalHandle unusedHandle tagHandle antecedentHandle
    ensureMainHandle <- EnsureMain.new locatorHandle
    pathHandle <- Path.new envHandle debugHandle
    parseHandle <- Parse.new envHandle gensymHandle debugHandle locatorHandle globalHandle optDataHandle keyArgHandle unusedHandle tagHandle antecedentHandle

    let elaborateConfig =
          Elaborate.Config
            { _envHandle = envHandle,
              _gensymHandle = gensymHandle,
              _debugHandle = debugHandle,
              _optDataHandle = optDataHandle,
              _keyArgHandle = keyArgHandle,
              _discernHandle = discernHandle,
              _typeHandle = typeHandle
            }
    checkHandle <- SceneCheck.new envHandle gensymHandle colorHandle debugHandle locatorHandle globalHandle optDataHandle keyArgHandle unusedHandle tagHandle antecedentHandle discernHandle elaborateConfig
    cleanHandle <- SceneClean.new envHandle gensymHandle debugHandle locatorHandle globalHandle unusedHandle tagHandle antecedentHandle
    clarifyHandle <- Clarify.new gensymHandle locatorHandle optDataHandle
    llvmHandle <- LLVM.new envHandle debugHandle
    emitHandle <- Emit.new gensymHandle
    linkHandle <- Link.new envHandle colorHandle debugHandle
    installHandle <- Install.new envHandle debugHandle
    executeHandle <- Execute.new envHandle debugHandle
    c <- liftIO OptParse.parseCommand
    Throw.run reportHandle $ do
      ensureExecutables
      case c of
        C.Build cfg -> do
          buildHandle <- SceneBuild.new (Build.toBuildConfig cfg) gensymHandle debugHandle initTargetHandle unravelHandle loadHandle globalRemarkHandle reportHandle envHandle locatorHandle cacheHandle colorHandle initSourceHandle pathHandle externalHandle ensureMainHandle parseHandle clarifyHandle llvmHandle emitHandle linkHandle installHandle executeHandle elaborateConfig
          h <- Build.new initCompilerHandle fetchHandle collectHandle envHandle buildHandle
          Build.build h cfg
        C.Check cfg -> do
          h <- Check.new initCompilerHandle fetchHandle envHandle reportHandle checkHandle
          Check.check h cfg
        C.Clean cfg -> do
          h <- Clean.new initCompilerHandle cleanHandle
          toApp $ Clean.clean h cfg
        C.Archive cfg -> do
          packageVersionHandle <- PV.new reportHandle
          ensReflectHandle <- EnsReflect.new gensymHandle
          archiveHandle <- SceneArchive.new externalHandle moduleSaveHandle envHandle
          h <- Archive.new initCompilerHandle envHandle packageVersionHandle ensReflectHandle archiveHandle
          toApp $ Archive.archive h cfg
        C.Create cfg -> do
          newHandle <- New.new reportHandle debugHandle
          h <- Create.new initLoggerHandle initCompilerHandle newHandle fetchHandle checkHandle
          Create.create h cfg
        C.Get cfg -> do
          h <- Get.new initCompilerHandle fetchHandle envHandle cleanHandle checkHandle
          Get.get h cfg
        C.Format cfg -> do
          h <- Format.new initCompilerHandle initTargetHandle formatHandle
          toApp $ Format.format h cfg
        C.LSP -> do
          lspHandle <- L.new envHandle gensymHandle colorHandle reportHandle debugHandle locatorHandle globalHandle optDataHandle keyArgHandle unusedHandle tagHandle antecedentHandle lspFormatHandle unravelHandle discernHandle checkHandle elaborateConfig
          h <- LSP.new initCompilerHandle fetchHandle envHandle lspHandle
          LSP.lsp h
        C.ShowVersion cfg ->
          liftIO $ Version.showVersion cfg
        C.Zen cfg -> do
          buildHandle <- SceneBuild.new (Zen.toBuildConfig cfg) gensymHandle debugHandle initTargetHandle unravelHandle loadHandle globalRemarkHandle reportHandle envHandle locatorHandle cacheHandle colorHandle initSourceHandle pathHandle externalHandle ensureMainHandle parseHandle clarifyHandle llvmHandle emitHandle linkHandle installHandle executeHandle elaborateConfig
          h <- Zen.new initCompilerHandle fetchHandle envHandle buildHandle
          Zen.zen h cfg
