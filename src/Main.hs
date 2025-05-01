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
import Move.Context.Alias qualified as Alias
import Move.Context.EIO (run)
import Move.Context.Elaborate qualified as Elaborate
import Move.Context.Env qualified as Env
import Move.Context.External qualified as External
import Move.Context.Global qualified as Global
import Move.Context.Locator qualified as Locator
import Move.Context.OptParse qualified as OptParse
import Move.Context.PreDecl qualified as PreDecl
import Move.Context.RawImportSummary qualified as RawImportSummary
import Move.Context.SymLoc qualified as SymLoc
import Move.Context.Tag qualified as Tag
import Move.Context.TopCandidate qualified as TopCandidate
import Move.Context.Unused qualified as Unused
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
import Move.Scene.Elaborate.Handle.WeakDecl qualified as WeakDecl
import Move.Scene.Ens.Reflect qualified as EnsReflect
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Format qualified as SceneFormat
import Move.Scene.Init.Base qualified as Base
import Move.Scene.Init.Compiler qualified as InitCompiler
import Move.Scene.Init.Logger qualified as InitLogger
import Move.Scene.Init.Source qualified as InitSource
import Move.Scene.Init.Target qualified as InitTarget
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
import Move.Scene.LSP.Util qualified as LspUtil
import Move.Scene.Load qualified as Load
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
import Move.UI.Handle.LocalRemark qualified as LocalRemark
import Rule.Command qualified as C
import System.IO

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  Main.execute

execute :: IO ()
execute = do
  userCommand <- liftIO OptParse.parseCommand
  baseHandle@(Base.Handle {..}) <- Base.new
  tagHandle <- Tag.new
  locatorHandle <- Locator.new envHandle tagHandle
  unusedHandle <- Unused.new
  globalHandle <- Global.new envHandle locatorHandle optDataHandle keyArgHandle unusedHandle tagHandle
  aliasHandle <- Alias.new antecedentHandle locatorHandle envHandle
  symLocHandle <- SymLoc.new
  topCandidateHandle <- TopCandidate.new
  preDeclHandle <- PreDecl.new
  localRemarkHandle <- LocalRemark.new
  rawImportSummaryHandle <- RawImportSummary.new
  weakDeclHandle <- WeakDecl.new
  auxEnvHandle <- AuxEnv.new
  run reportHandle $ do
    baseSize <- Env.getBaseSize'
    let collectHandle = Collect.new envHandle
    let discernHandle = Discern.new gensymHandle locatorHandle globalHandle aliasHandle tagHandle keyArgHandle symLocHandle topCandidateHandle preDeclHandle optDataHandle unusedHandle envHandle
    let initLoggerHandle = InitLogger.new colorHandle reportHandle envHandle debugHandle
    let moduleReflectHandle = ModuleReflect.new gensymHandle
    let initCompilerHandle = InitCompiler.new initLoggerHandle moduleReflectHandle envHandle
    let externalHandle = External.new debugHandle
    let moduleSaveHandle = ModuleSave.new debugHandle
    let loadHandle = Load.new baseHandle
    let initSourceHandle = InitSource.new unusedHandle localRemarkHandle globalHandle aliasHandle locatorHandle tagHandle rawImportSummaryHandle symLocHandle topCandidateHandle preDeclHandle weakDeclHandle
    let parseCoreHandle = ParseCore.new gensymHandle
    let getEnabledPresetHandle = GetEnabledPreset.new gensymHandle envHandle moduleHandle
    let shiftToLatestHandle = ShiftToLatest.new antecedentHandle
    let importHandle = Import.new envHandle unusedHandle getEnabledPresetHandle shiftToLatestHandle locatorHandle aliasHandle globalHandle gensymHandle rawImportSummaryHandle moduleHandle nameMapHandle tagHandle
    let parseHandle = Parse.new parseCoreHandle discernHandle pathHandle importHandle globalHandle localRemarkHandle nameMapHandle unusedHandle
    let compSubstHandle = CompSubst.new gensymHandle
    let utilityHandle = ClarifyUtility.new gensymHandle compSubstHandle auxEnvHandle baseSize
    let linearizeHandle = Linearize.new gensymHandle utilityHandle
    let sigmaHandle = Sigma.new gensymHandle linearizeHandle utilityHandle
    let compReduceHandle = CompReduce.new compDefHandle compSubstHandle gensymHandle
    let termSubstHandle = TermSubst.new gensymHandle
    let clarifyHandle = Clarify.new gensymHandle linearizeHandle utilityHandle auxEnvHandle sigmaHandle locatorHandle optDataHandle compReduceHandle termSubstHandle compDefHandle baseSize
    let ensReflectHandle = EnsReflect.new gensymHandle
    unravelHandle <- liftIO $ Unravel.new baseHandle
    let fetchHandle = Fetch.new ensReflectHandle moduleSaveHandle externalHandle moduleReflectHandle reportHandle envHandle
    let initTargetHandle = InitTarget.new clarifyHandle unravelHandle antecedentHandle globalRemarkHandle weakDefHandle defHandle typeHandle
    let formatHandle = SceneFormat.new unravelHandle loadHandle parseCoreHandle parseHandle envHandle ensReflectHandle getEnabledPresetHandle unusedHandle initTargetHandle initSourceHandle
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
    let getModuleHandle = GetModule.new baseHandle
    let checkHandle = SceneCheck.new baseHandle
    let cleanHandle = SceneClean.new envHandle unravelHandle
    ensureExecutables
    case userCommand of
      C.Build cfg -> do
        let buildHandle = SceneBuild.new (Build.toBuildConfig cfg) baseHandle
        let h = Build.new initCompilerHandle fetchHandle collectHandle envHandle buildHandle
        Build.build h cfg
      C.Check cfg -> do
        let h = Check.new initCompilerHandle fetchHandle envHandle reportHandle checkHandle
        Check.check h cfg
      C.Clean cfg -> do
        let h = Clean.new initCompilerHandle cleanHandle
        Clean.clean h cfg
      C.Archive cfg -> do
        let packageVersionHandle = PV.new reportHandle
        let archiveHandle = SceneArchive.new externalHandle moduleSaveHandle envHandle
        let h = Archive.new initCompilerHandle envHandle packageVersionHandle ensReflectHandle archiveHandle
        Archive.archive h cfg
      C.Create cfg -> do
        let newHandle = New.new moduleSaveHandle reportHandle
        let h = Create.new initLoggerHandle initCompilerHandle newHandle fetchHandle checkHandle
        Create.create h cfg
      C.Get cfg -> do
        let h = Get.new initCompilerHandle fetchHandle envHandle cleanHandle checkHandle
        Get.get h cfg
      C.Format cfg -> do
        let h = Format.new initCompilerHandle initTargetHandle formatHandle
        Format.format h cfg
      C.LSP -> do
        let lspUtilHandle = LspUtil.new initCompilerHandle globalRemarkHandle
        let lintHandle = Lint.new fetchHandle envHandle checkHandle lspUtilHandle
        let lspFormatHandle = LSPFormat.new formatHandle
        let sourceReflectHandle = SourceReflect.new envHandle moduleReflectHandle
        let getSourceHandle = GetSource.new sourceReflectHandle
        let getLocationTreeHandle = GetLocationTree.new pathHandle
        let findDefHandle = FindDefinition.new getSourceHandle getLocationTreeHandle
        let getSymbolInfoHandle = GetSymbolInfo.new getSourceHandle pathHandle findDefHandle envHandle gensymHandle checkHandle locatorHandle tagHandle antecedentHandle colorHandle debugHandle keyArgHandle optDataHandle unusedHandle globalHandle discernHandle elaborateConfig
        let gacHandle = GAC.new baseHandle
        let completeHandle = Complete.new unravelHandle clangHandle pathHandle antecedentHandle getModuleHandle sourceReflectHandle envHandle gacHandle
        let highlightHandle = Highlight.new findDefHandle
        let referencesHandle = References.new unravelHandle getSourceHandle findDefHandle gacHandle
        let lspHandle = L.new initCompilerHandle completeHandle findDefHandle highlightHandle referencesHandle lspFormatHandle checkHandle getSymbolInfoHandle lintHandle lspUtilHandle
        let h = LSP.new initCompilerHandle fetchHandle envHandle lspHandle
        LSP.lsp h
      C.ShowVersion cfg ->
        liftIO $ Version.showVersion cfg
      C.Zen cfg -> do
        let buildHandle = SceneBuild.new (Zen.toBuildConfig cfg) baseHandle
        let h = Zen.new initCompilerHandle fetchHandle envHandle buildHandle
        Zen.zen h cfg
