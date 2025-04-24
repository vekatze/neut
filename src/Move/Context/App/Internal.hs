module Move.Context.App.Internal
  ( Env (..),
    Ref,
    newEnv,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.IntMap qualified as IntMap
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Path
import Rule.ArgNum qualified as AN
import Rule.Artifact qualified as AR
import Rule.Binder
import Rule.BuildMode qualified as BM
import Rule.Comp
import Rule.DeclarationName qualified as DN
import Rule.DefiniteDescription qualified as DD
import Rule.Discriminant qualified as D
import Rule.ExternalName qualified as EN
import Rule.ForeignCodType qualified as F
import Rule.GlobalLocatorAlias qualified as GLA
import Rule.GlobalName qualified as GN
import Rule.Hint
import Rule.Ident
import Rule.Import
import Rule.IsConstLike
import Rule.Key
import Rule.LocalLocator qualified as LL
import Rule.LocalVarTree qualified as LVT
import Rule.LocationTree qualified as LT
import Rule.Module qualified as M
import Rule.Module qualified as Module
import Rule.ModuleAlias qualified as MA
import Rule.ModuleDigest qualified as MD
import Rule.ModuleID qualified as MID
import Rule.Opacity qualified as O
import Rule.OptimizableData
import Rule.RawImportSummary (RawImportSummary)
import Rule.Remark qualified as Remark
import Rule.Source qualified as Source
import Rule.StrictGlobalLocator qualified as SGL
import Rule.Term qualified as TM
import Rule.TopCandidate
import Rule.TopNameMap
import Rule.VarDefKind
import Rule.VisitInfo
import Rule.WeakTerm qualified as WT
import System.IO

data Env = Env
  { enableDebugMode :: IORef Bool,
    enableSilentMode :: IORef Bool,
    startTime :: UTCTime,
    counter :: IORef Int,
    endOfEntry :: IORef T.Text,
    shouldColorizeStdout :: IORef Bool,
    shouldColorizeStderr :: IORef Bool,
    buildMode :: IORef BM.BuildMode,
    moduleCacheMap :: IORef (Map.HashMap (Path Abs File) M.Module),
    moduleAliasMap :: IORef (Map.HashMap MA.ModuleAlias MD.ModuleDigest),
    locatorAliasMap :: IORef (Map.HashMap GLA.GlobalLocatorAlias SGL.StrictGlobalLocator),
    sourceNameMap :: IORef (Map.HashMap (Path Abs File) TopNameMap),
    nameMap :: IORef (Map.HashMap DD.DefiniteDescription (Hint, GN.GlobalName)),
    geistMap :: IORef (Map.HashMap DD.DefiniteDescription (Hint, IsConstLike)),
    antecedentMap :: IORef (Map.HashMap MID.ModuleID M.Module),
    reverseAntecedentMap :: IORef (Map.HashMap MID.ModuleID (S.Set MID.ModuleID)),
    antecedentDigestCache :: Ref T.Text,
    remarkList :: IORef [Remark.Remark], -- per file
    globalRemarkList :: IORef [Remark.Remark],
    tagMap :: IORef LT.LocationTree,
    importEnv :: IORef (Maybe RawImportSummary),
    localVarMap :: IORef LVT.LocalVarTree,
    topCandidateEnv :: IORef [TopCandidate],
    unusedVariableMap :: IORef (IntMap.IntMap (Hint, Ident, VarDefKind)),
    usedVariableSet :: IORef (S.Set Int),
    unusedGlobalLocatorMap :: IORef (Map.HashMap T.Text [(Hint, T.Text)]), -- (SGL ~> [(hint, locatorText)])
    unusedLocalLocatorMap :: IORef (Map.HashMap LL.LocalLocator Hint),
    unusedPresetMap :: IORef (Map.HashMap T.Text Hint), -- (ModuleID ~> Hint)
    unusedStaticFileMap :: IORef (Map.HashMap T.Text Hint),
    buildSignatureCache :: IORef (Maybe String), -- only for memoization
    sourceChildrenMap :: IORef (Map.HashMap (Path Abs File) [ImportItem]),
    traceSourceList :: IORef [Source.Source],
    weakTypeEnv :: IORef (IntMap.IntMap WT.WeakTerm),
    artifactMap :: IORef (Map.HashMap (Path Abs File) AR.ArtifactTime),
    visitEnv :: IORef (Map.HashMap (Path Abs File) VisitInfo),
    weakDefMap :: IORef (Map.HashMap DD.DefiniteDescription WT.WeakTerm),
    defMap :: IORef (Map.HashMap DD.DefiniteDescription ([BinderF TM.Term], TM.Term)),
    compAuxEnv :: IORef (Map.HashMap DD.DefiniteDescription (O.Opacity, [Ident], Comp)),
    dataDefMap :: IORef (Map.HashMap DD.DefiniteDescription [(D.Discriminant, [BinderF TM.Term], [BinderF TM.Term])]),
    keyArgMap :: IORef (Map.HashMap DD.DefiniteDescription (IsConstLike, (AN.ArgNum, [Key]))),
    optDataMap :: IORef (Map.HashMap DD.DefiniteDescription OptimizableData),
    preDeclEnv :: IORef (Map.HashMap EN.ExternalName Hint),
    weakDeclEnv :: IORef (Map.HashMap DN.DeclarationName ([WT.WeakTerm], F.ForeignCodType WT.WeakTerm)),
    compEnv :: IORef (Map.HashMap DD.DefiniteDescription (O.Opacity, [Ident], Comp)),
    typeEnv :: IORef (Map.HashMap DD.DefiniteDescription WT.WeakTerm),
    activeGlobalLocatorList :: IORef [SGL.StrictGlobalLocator],
    activeDefiniteDescriptionList :: IORef (Map.HashMap LL.LocalLocator DD.DefiniteDescription),
    activeStaticFileList :: IORef (Map.HashMap T.Text (Path Abs File, T.Text)),
    currentGlobalLocator :: Ref SGL.StrictGlobalLocator,
    currentSource :: Ref Source.Source,
    clangDigest :: Ref T.Text,
    mainModule :: Ref Module.MainModule
  }

type Ref a = IORef (Maybe a)

newRef :: IO (Ref a)
newRef =
  newIORef Nothing

newEnv :: IO Env
newEnv = do
  counter <- newIORef 0
  startTime <- getCurrentTime
  enableDebugMode <- newIORef False
  enableSilentMode <- newIORef False
  endOfEntry <- newIORef ""
  shouldColorizeStdout <- hIsTerminalDevice stdout >>= newIORef
  shouldColorizeStderr <- hIsTerminalDevice stderr >>= newIORef
  buildMode <- newIORef BM.Develop
  moduleCacheMap <- newIORef Map.empty
  moduleAliasMap <- newIORef Map.empty
  locatorAliasMap <- newIORef Map.empty
  sourceNameMap <- newIORef Map.empty
  remarkList <- newIORef []
  importEnv <- newIORef Nothing
  globalRemarkList <- newIORef []
  tagMap <- newIORef LT.empty
  localVarMap <- newIORef LVT.empty
  topCandidateEnv <- newIORef []
  unusedVariableMap <- newIORef IntMap.empty
  usedVariableSet <- newIORef S.empty
  unusedGlobalLocatorMap <- newIORef Map.empty
  unusedPresetMap <- newIORef Map.empty
  unusedLocalLocatorMap <- newIORef Map.empty
  unusedStaticFileMap <- newIORef Map.empty
  nameMap <- newIORef Map.empty
  geistMap <- newIORef Map.empty
  antecedentMap <- newIORef Map.empty
  reverseAntecedentMap <- newIORef Map.empty
  antecedentDigestCache <- newRef
  buildSignatureCache <- newIORef Nothing
  sourceChildrenMap <- newIORef Map.empty
  weakTypeEnv <- newIORef IntMap.empty
  traceSourceList <- newIORef []
  artifactMap <- newIORef Map.empty
  visitEnv <- newIORef Map.empty
  weakDefMap <- newIORef Map.empty
  defMap <- newIORef Map.empty
  compAuxEnv <- newIORef Map.empty
  dataDefMap <- newIORef Map.empty
  keyArgMap <- newIORef Map.empty
  optDataMap <- newIORef Map.empty
  preDeclEnv <- newIORef Map.empty
  weakDeclEnv <- newIORef Map.empty
  compEnv <- newIORef Map.empty
  typeEnv <- newIORef Map.empty
  activeGlobalLocatorList <- newIORef []
  activeDefiniteDescriptionList <- newIORef Map.empty
  activeStaticFileList <- newIORef Map.empty
  currentGlobalLocator <- newRef
  currentSource <- newRef
  clangDigest <- newRef
  mainModule <- newRef
  return Env {..}
