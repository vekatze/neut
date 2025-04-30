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
import Move.Context.Alias qualified as Alias
import Move.Context.Antecedent qualified as Antecedent
import Move.Context.App
import Move.Context.AppM qualified as AppM
import Move.Context.Artifact qualified as Artifact
import Move.Context.Cache qualified as Cache
import Move.Context.Clang qualified as Clang
import Move.Context.Color qualified as Color
import Move.Context.CompDefinition qualified as CompDefinition
import Move.Context.Debug qualified as Debug
import Move.Context.Definition qualified as Definition
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Context.External qualified as External
import Move.Context.Global qualified as Global
import Move.Context.KeyArg qualified as KeyArg
import Move.Context.LLVM qualified as LLVM
import Move.Context.Locator qualified as Locator
import Move.Context.Module qualified as Module
import Move.Context.OptParse qualified as OptParse
import Move.Context.OptimizableData qualified as OptimizableData
import Move.Context.Path qualified as Path
import Move.Context.PreDecl qualified as PreDecl
import Move.Context.RawImportSummary qualified as RawImportSummary
import Move.Context.SymLoc qualified as SymLoc
import Move.Context.Tag qualified as Tag
import Move.Context.Throw qualified as Throw
import Move.Context.TopCandidate qualified as TopCandidate
import Move.Context.Type qualified as Type
import Move.Context.Unused qualified as Unused
import Move.Context.WeakDefinition qualified as WeakDefinition
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Archive qualified as SceneArchive
import Move.Scene.Build qualified as SceneBuild
import Move.Scene.Check qualified as SceneCheck
import Move.Scene.Clarify qualified as Clarify
import Move.Scene.Clarify.Handle.AuxEnv qualified as AuxEnv
import Move.Scene.Clarify.Linearize qualified as Linearize
import Move.Scene.Clarify.Sigma qualified as Sigma
import Move.Scene.Clarify.Utility qualified as ClarifyUtility
import Move.Scene.Clean qualified as SceneClean
import Move.Scene.Collect qualified as Collect
import Move.Scene.Comp.Reduce qualified as CompReduce
import Move.Scene.Comp.Subst qualified as CompSubst
import Move.Scene.Elaborate (Config (_rawImportSummaryHandle))
import Move.Scene.Elaborate qualified as Elaborate
import Move.Scene.Elaborate.Handle.WeakDecl qualified as WeakDecl
import Move.Scene.Emit qualified as Emit
import Move.Scene.Emit.LowComp qualified as EmitLowComp
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
import Move.Scene.LSP.Complete qualified as Complete
import Move.Scene.LSP.FindDefinition qualified as FindDefinition
import Move.Scene.LSP.Format qualified as LSPFormat
import Move.Scene.LSP.GetAllCachesInModule qualified as GAC
import Move.Scene.LSP.GetLocationTree qualified as GetLocationTree
import Move.Scene.LSP.GetSource qualified as GetSource
import Move.Scene.LSP.GetSymbolInfo qualified as GetSymbolInfo
import Move.Scene.LSP.Highlight qualified as Highlight
import Move.Scene.LSP.Lint qualified as Lint
import Move.Scene.LSP.References qualified as References
import Move.Scene.Link qualified as Link
import Move.Scene.Load qualified as Load
import Move.Scene.LowComp.Reduce qualified as LowCompReduce
import Move.Scene.Lower qualified as Lower
import Move.Scene.Module.GetEnabledPreset qualified as GetEnabledPreset
import Move.Scene.Module.GetModule qualified as GetModule
import Move.Scene.Module.Reflect qualified as ModuleReflect
import Move.Scene.Module.Save qualified as ModuleSave
import Move.Scene.New qualified as New
import Move.Scene.PackageVersion.ChooseNewVersion qualified as PV
import Move.Scene.Parse qualified as Parse
import Move.Scene.Parse.Core qualified as ParseCore
import Move.Scene.Parse.Discern.Handle qualified as Discern
import Move.Scene.Parse.Import qualified as Import
import Move.Scene.Source.Reflect qualified as SourceReflect
import Move.Scene.Source.ShiftToLatest qualified as ShiftToLatest
import Move.Scene.Term.Subst qualified as TermSubst
import Move.Scene.Unravel qualified as Unravel
import Move.UI.Handle.GlobalRemark qualified as GlobalRemark
import Move.UI.Handle.LocalRemark qualified as LocalRemark
import Rule.Command qualified as C
import Rule.LowComp.EmitOp qualified as EmitOp
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
    aliasHandle <- liftIO $ Alias.new antecedentHandle locatorHandle envHandle
    symLocHandle <- liftIO SymLoc.new
    topCandidateHandle <- liftIO TopCandidate.new
    preDeclHandle <- liftIO PreDecl.new
    discernHandle <- Discern.new gensymHandle locatorHandle globalHandle aliasHandle tagHandle keyArgHandle symLocHandle topCandidateHandle preDeclHandle optDataHandle unusedHandle envHandle
    initLoggerHandle <- InitLogger.new colorHandle reportHandle envHandle debugHandle
    moduleReflectHandle <- ModuleReflect.new gensymHandle
    initCompilerHandle <- InitCompiler.new initLoggerHandle moduleReflectHandle envHandle
    externalHandle <- External.new debugHandle
    moduleSaveHandle <- ModuleSave.new debugHandle
    globalRemarkHandle <- liftIO GlobalRemark.new
    clangHandle <- liftIO $ Clang.new debugHandle
    pathHandle <- liftIO $ Path.new envHandle debugHandle clangHandle
    artifactHandle <- liftIO Artifact.new
    let cacheHandle = Cache.new pathHandle artifactHandle
    loadHandle <- Load.new debugHandle cacheHandle
    localRemarkHandle <- liftIO LocalRemark.new
    rawImportSummaryHandle <- liftIO RawImportSummary.new
    weakDeclHandle <- liftIO WeakDecl.new
    initSourceHandle <- InitSource.new unusedHandle localRemarkHandle globalHandle envHandle aliasHandle locatorHandle tagHandle rawImportSummaryHandle symLocHandle topCandidateHandle preDeclHandle weakDeclHandle
    ensureMainHandle <- EnsureMain.new locatorHandle
    parseCoreHandle <- ParseCore.new gensymHandle
    moduleHandle <- liftIO Module.new
    getEnabledPresetHandle <- GetEnabledPreset.new gensymHandle envHandle moduleHandle
    shiftToLatestHandle <- ShiftToLatest.new antecedentHandle
    importHandle <- Import.new envHandle unusedHandle getEnabledPresetHandle shiftToLatestHandle locatorHandle aliasHandle globalHandle gensymHandle rawImportSummaryHandle moduleHandle tagHandle
    parseHandle <- Parse.new parseCoreHandle discernHandle pathHandle importHandle globalHandle localRemarkHandle unusedHandle
    baseSize <- toApp Env.getBaseSize'
    compSubstHandle <- CompSubst.new gensymHandle
    auxEnvHandle <- liftIO AuxEnv.new
    utilityHandle <- ClarifyUtility.new gensymHandle compSubstHandle auxEnvHandle baseSize
    linearizeHandle <- Linearize.new gensymHandle utilityHandle
    sigmaHandle <- Sigma.new gensymHandle linearizeHandle locatorHandle utilityHandle
    compDefHandle <- liftIO CompDefinition.new
    compReduceHandle <- CompReduce.new compDefHandle compSubstHandle gensymHandle
    termSubstHandle <- TermSubst.new gensymHandle
    clarifyHandle <- Clarify.new gensymHandle linearizeHandle utilityHandle auxEnvHandle sigmaHandle locatorHandle optDataHandle compReduceHandle termSubstHandle compDefHandle baseSize
    arch <- toApp $ Env.getArch Nothing
    lowerHandle <- Lower.new arch baseSize gensymHandle locatorHandle compReduceHandle compSubstHandle
    weakDefHandle <- liftIO $ WeakDefinition.new gensymHandle
    defHandle <- liftIO Definition.new
    ensReflectHandle <- EnsReflect.new gensymHandle
    unravelHandle <- liftIO $ Unravel.new envHandle debugHandle moduleReflectHandle pathHandle shiftToLatestHandle importHandle parseCoreHandle locatorHandle aliasHandle antecedentHandle artifactHandle
    fetchHandle <- Fetch.new ensReflectHandle moduleSaveHandle externalHandle moduleReflectHandle reportHandle envHandle
    initTargetHandle <- InitTarget.new clarifyHandle unravelHandle antecedentHandle globalRemarkHandle weakDefHandle defHandle typeHandle
    formatHandle <- SceneFormat.new unravelHandle loadHandle parseCoreHandle parseHandle envHandle ensReflectHandle getEnabledPresetHandle unusedHandle initTargetHandle initSourceHandle
    let elaborateConfig =
          Elaborate.Config
            { _envHandle = envHandle,
              _gensymHandle = gensymHandle,
              _debugHandle = debugHandle,
              _optDataHandle = optDataHandle,
              _keyArgHandle = keyArgHandle,
              _discernHandle = discernHandle,
              _typeHandle = typeHandle,
              _clangHandle = clangHandle,
              _rawImportSummaryHandle = rawImportSummaryHandle,
              _symLocHandle = symLocHandle,
              _pathHandle = pathHandle,
              _topCandidateHandle = topCandidateHandle,
              _localRemarkHandle = localRemarkHandle,
              _globalRemarkHandle = globalRemarkHandle,
              _weakDeclHandle = weakDeclHandle,
              _weakDefHandle = weakDefHandle,
              _defHandle = defHandle
            }
    getModuleHandle <- GetModule.new gensymHandle moduleHandle
    checkHandle <- SceneCheck.new debugHandle gensymHandle loadHandle unravelHandle parseHandle getModuleHandle envHandle initSourceHandle initTargetHandle globalRemarkHandle elaborateConfig
    cleanHandle <- SceneClean.new envHandle unravelHandle
    let llvmHandle = LLVM.new envHandle debugHandle pathHandle externalHandle
    let emitOpHandle = EmitOp.new baseSize
    emitLowCompHandle <- EmitLowComp.new gensymHandle emitOpHandle
    dataSize <- toApp Env.getDataSize'
    lowCompReduceHandle <- LowCompReduce.new gensymHandle
    emitHandle <- Emit.new gensymHandle emitLowCompHandle lowCompReduceHandle dataSize baseSize
    linkHandle <- Link.new debugHandle envHandle pathHandle colorHandle llvmHandle
    installHandle <- Install.new envHandle pathHandle
    executeHandle <- Execute.new envHandle pathHandle externalHandle
    c <- liftIO OptParse.parseCommand
    Throw.run reportHandle $ do
      toApp ensureExecutables
      case c of
        C.Build cfg -> do
          buildHandle <- SceneBuild.new (Build.toBuildConfig cfg) gensymHandle debugHandle initTargetHandle unravelHandle loadHandle globalRemarkHandle reportHandle envHandle locatorHandle cacheHandle colorHandle initSourceHandle pathHandle externalHandle ensureMainHandle parseHandle clarifyHandle lowerHandle llvmHandle emitHandle linkHandle installHandle executeHandle elaborateConfig
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
          let archiveHandle = SceneArchive.new externalHandle moduleSaveHandle envHandle
          h <- Archive.new initCompilerHandle envHandle packageVersionHandle ensReflectHandle archiveHandle
          toApp $ Archive.archive h cfg
        C.Create cfg -> do
          newHandle <- New.new moduleSaveHandle reportHandle
          h <- Create.new initLoggerHandle initCompilerHandle newHandle fetchHandle checkHandle
          Create.create h cfg
        C.Get cfg -> do
          h <- Get.new initCompilerHandle fetchHandle envHandle cleanHandle checkHandle
          Get.get h cfg
        C.Format cfg -> do
          h <- Format.new initCompilerHandle initTargetHandle formatHandle
          toApp $ Format.format h cfg
        C.LSP -> do
          let appHandle = AppM.new initCompilerHandle globalRemarkHandle
          lintHandle <- Lint.new fetchHandle envHandle appHandle checkHandle
          lspFormatHandle <- LSPFormat.new formatHandle
          sourceReflectHandle <- SourceReflect.new envHandle moduleReflectHandle
          getSourceHandle <- GetSource.new sourceReflectHandle
          getLocationTreeHandle <- GetLocationTree.new pathHandle
          findDefHandle <- FindDefinition.new getSourceHandle getLocationTreeHandle
          getSymbolInfoHandle <- GetSymbolInfo.new getSourceHandle pathHandle findDefHandle envHandle gensymHandle checkHandle locatorHandle tagHandle antecedentHandle colorHandle debugHandle keyArgHandle optDataHandle unusedHandle globalHandle discernHandle elaborateConfig
          gacHandle <- GAC.new shiftToLatestHandle pathHandle
          completeHandle <- Complete.new unravelHandle clangHandle pathHandle antecedentHandle getModuleHandle sourceReflectHandle envHandle gacHandle
          highlightHandle <- Highlight.new findDefHandle
          referencesHandle <- References.new unravelHandle getSourceHandle findDefHandle gacHandle
          lspHandle <- L.new initCompilerHandle appHandle completeHandle findDefHandle highlightHandle referencesHandle lspFormatHandle checkHandle getSymbolInfoHandle lintHandle
          h <- LSP.new initCompilerHandle fetchHandle envHandle lspHandle
          LSP.lsp h
        C.ShowVersion cfg ->
          liftIO $ Version.showVersion cfg
        C.Zen cfg -> do
          buildHandle <- SceneBuild.new (Zen.toBuildConfig cfg) gensymHandle debugHandle initTargetHandle unravelHandle loadHandle globalRemarkHandle reportHandle envHandle locatorHandle cacheHandle colorHandle initSourceHandle pathHandle externalHandle ensureMainHandle parseHandle clarifyHandle lowerHandle llvmHandle emitHandle linkHandle installHandle executeHandle elaborateConfig
          h <- Zen.new initCompilerHandle fetchHandle envHandle buildHandle
          Zen.zen h cfg
