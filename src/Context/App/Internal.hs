module Context.App.Internal
  ( Env (..),
    Ref,
    newEnv,
  )
where

import Data.ByteString.Builder
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.IORef.Unboxed
import Data.IntMap qualified as IntMap
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Entity.ArgNum qualified as AN
import Entity.Artifact qualified as AR
import Entity.BaseLowType qualified as BLT
import Entity.Binder
import Entity.BuildMode qualified as BM
import Entity.Comp
import Entity.Constraint qualified as C
import Entity.DeclarationName qualified as DN
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.ExternalName qualified as EN
import Entity.ForeignCodType qualified as F
import Entity.GlobalLocatorAlias qualified as GLA
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.HoleSubst qualified as HS
import Entity.Ident
import Entity.Import
import Entity.IsConstLike
import Entity.Key
import Entity.LocalLocator qualified as LL
import Entity.LocalVarTree qualified as LVT
import Entity.LocationTree qualified as LT
import Entity.Module qualified as M
import Entity.Module qualified as Module
import Entity.ModuleAlias qualified as MA
import Entity.ModuleDigest qualified as MD
import Entity.ModuleID qualified as MID
import Entity.Opacity qualified as O
import Entity.OptimizableData
import Entity.RawImportSummary (RawImportSummary)
import Entity.Remark qualified as Remark
import Entity.Source qualified as Source
import Entity.StrictGlobalLocator qualified as SGL
import Entity.Term qualified as TM
import Entity.TopCandidate
import Entity.TopNameMap
import Entity.VarDefKind
import Entity.VisitInfo
import Entity.WeakTerm qualified as WT
import Path
import System.IO

data Env = Env
  { enableDebugMode :: IORef Bool,
    enableSilentMode :: IORef Bool,
    startTime :: UTCTime,
    counter :: IORefU Int,
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
    constraintEnv :: IORef [C.Constraint],
    suspendedEnv :: IORef [C.SuspendedConstraint],
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
    holeSubst :: IORef HS.HoleSubst,
    sourceChildrenMap :: IORef (Map.HashMap (Path Abs File) [ImportItem]),
    traceSourceList :: IORef [Source.Source],
    weakTypeEnv :: IORef (IntMap.IntMap WT.WeakTerm),
    holeEnv :: IORef (IntMap.IntMap (WT.WeakTerm, WT.WeakTerm)),
    artifactMap :: IORef (Map.HashMap (Path Abs File) AR.ArtifactTime),
    visitEnv :: IORef (Map.HashMap (Path Abs File) VisitInfo),
    weakDefMap :: IORef (Map.HashMap DD.DefiniteDescription WT.WeakTerm),
    defMap :: IORef (Map.HashMap DD.DefiniteDescription ([BinderF TM.Term], TM.Term)),
    staticTextList :: IORef [(DD.DefiniteDescription, (Builder, Int))],
    compAuxEnv :: IORef (Map.HashMap DD.DefiniteDescription (O.Opacity, [Ident], Comp)),
    dataDefMap :: IORef (Map.HashMap DD.DefiniteDescription [(D.Discriminant, [BinderF TM.Term], [BinderF TM.Term])]),
    keyArgMap :: IORef (Map.HashMap DD.DefiniteDescription (IsConstLike, (AN.ArgNum, [Key]))),
    optDataMap :: IORef (Map.HashMap DD.DefiniteDescription OptimizableData),
    preDeclEnv :: IORef (Map.HashMap EN.ExternalName Hint),
    declEnv :: IORef (Map.HashMap DN.DeclarationName ([BLT.BaseLowType], F.ForeignCodType BLT.BaseLowType)),
    weakDeclEnv :: IORef (Map.HashMap DN.DeclarationName ([WT.WeakTerm], F.ForeignCodType WT.WeakTerm)),
    definedNameSet :: IORef (S.Set DD.DefiniteDescription),
    compEnv :: IORef (Map.HashMap DD.DefiniteDescription (O.Opacity, [Ident], Comp)),
    typeEnv :: IORef (Map.HashMap DD.DefiniteDescription WT.WeakTerm),
    activeGlobalLocatorList :: IORef [SGL.StrictGlobalLocator],
    activeDefiniteDescriptionList :: IORef (Map.HashMap LL.LocalLocator DD.DefiniteDescription),
    activeStaticFileList :: IORef (Map.HashMap T.Text (Path Abs File, T.Text)),
    currentGlobalLocator :: Ref SGL.StrictGlobalLocator,
    currentSource :: Ref Source.Source,
    clangDigest :: Ref T.Text,
    mainModule :: Ref Module.Module
  }

type Ref a = IORef (Maybe a)

newRef :: IO (Ref a)
newRef =
  newIORef Nothing

newEnv :: IO Env
newEnv = do
  counter <- newIORefU 0
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
  constraintEnv <- newIORef []
  suspendedEnv <- newIORef []
  buildSignatureCache <- newIORef Nothing
  holeSubst <- newIORef HS.empty
  sourceChildrenMap <- newIORef Map.empty
  weakTypeEnv <- newIORef IntMap.empty
  holeEnv <- newIORef IntMap.empty
  traceSourceList <- newIORef []
  artifactMap <- newIORef Map.empty
  definedNameSet <- newIORef S.empty
  visitEnv <- newIORef Map.empty
  weakDefMap <- newIORef Map.empty
  defMap <- newIORef Map.empty
  staticTextList <- newIORef []
  compAuxEnv <- newIORef Map.empty
  dataDefMap <- newIORef Map.empty
  keyArgMap <- newIORef Map.empty
  optDataMap <- newIORef Map.empty
  preDeclEnv <- newIORef Map.empty
  declEnv <- newIORef Map.empty
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
