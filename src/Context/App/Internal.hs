module Context.App.Internal where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.IORef.Unboxed
import Data.IntMap qualified as IntMap
import Data.PQueue.Min qualified as Q
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.ArgNum qualified as AN
import Entity.Arity qualified as A
import Entity.Artifact qualified as AR
import Entity.Binder
import Entity.BuildMode qualified as BM
import Entity.Comp
import Entity.Constraint qualified as C
import Entity.DeclarationName qualified as DN
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.GlobalLocatorAlias qualified as GLA
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.HoleSubst qualified as HS
import Entity.Ident
import Entity.Key
import Entity.LowType qualified as LT
import Entity.Module qualified as M
import Entity.Module qualified as Module
import Entity.ModuleAlias qualified as MA
import Entity.ModuleChecksum qualified as MC
import Entity.Opacity qualified as O
import Entity.Remark qualified as Remark
import Entity.Source qualified as Source
import Entity.StrictGlobalLocator qualified as SGL
import Entity.TargetPlatform qualified as TP
import Entity.Term qualified as TM
import Entity.VisitInfo
import Entity.WeakTerm qualified as WT
import Path

data Env = Env
  { counter :: IORefU Int,
    endOfEntry :: FastRef T.Text,
    clangOptString :: FastRef String,
    shouldColorize :: FastRef Bool,
    buildMode :: FastRef BM.BuildMode,
    moduleCacheMap :: FastRef (Map.HashMap (Path Abs File) M.Module),
    moduleAliasMap :: FastRef (Map.HashMap MA.ModuleAlias MC.ModuleChecksum),
    locatorAliasMap :: FastRef (Map.HashMap GLA.GlobalLocatorAlias SGL.StrictGlobalLocator),
    sourceNameMap :: FastRef (Map.HashMap (Path Abs File) (Map.HashMap DD.DefiniteDescription GN.GlobalName)),
    nameMap :: FastRef (Map.HashMap DD.DefiniteDescription GN.GlobalName),
    antecedentMap :: FastRef (Map.HashMap MC.ModuleChecksum M.Module),
    constraintEnv :: FastRef [C.Constraint],
    remarkList :: FastRef [Remark.Remark],
    unusedVariableMap :: FastRef (IntMap.IntMap (Hint, Ident)),
    holeSubst :: FastRef HS.HoleSubst,
    sourceChildrenMap :: FastRef (Map.HashMap (Path Abs File) [Source.Source]),
    traceSourceList :: FastRef [Source.Source],
    weakTypeEnv :: FastRef (IntMap.IntMap WT.WeakTerm),
    preHoleEnv :: FastRef (IntMap.IntMap WT.WeakTerm),
    holeEnv :: FastRef (IntMap.IntMap (WT.WeakTerm, WT.WeakTerm)),
    constraintQueue :: FastRef (Q.MinQueue C.SuspendedConstraint),
    artifactMap :: FastRef (Map.HashMap (Path Abs File) AR.ArtifactTime),
    visitEnv :: FastRef (Map.HashMap (Path Abs File) VisitInfo),
    weakDefMap :: FastRef (Map.HashMap DD.DefiniteDescription WT.WeakTerm),
    defMap :: FastRef (Map.HashMap DD.DefiniteDescription TM.Term),
    staticTextList :: FastRef [(DD.DefiniteDescription, (T.Text, Int))],
    compDefMap :: FastRef (Map.HashMap DD.DefiniteDescription (O.Opacity, [Ident], Comp)),
    dataDefMap :: FastRef (Map.HashMap DD.DefiniteDescription [(D.Discriminant, [BinderF TM.Term], [BinderF TM.Term])]),
    codataDefMap :: FastRef (Map.HashMap DD.DefiniteDescription ((DD.DefiniteDescription, A.Arity, A.Arity), [DD.DefiniteDescription])),
    keyArgMap :: FastRef (Map.HashMap DD.DefiniteDescription (A.Arity, [Key])),
    enumSet :: FastRef (S.Set DD.DefiniteDescription),
    impArgEnv :: FastRef (Map.HashMap DD.DefiniteDescription AN.ArgNum),
    declEnv :: FastRef (Map.HashMap DN.DeclarationName ([LT.LowType], LT.LowType)),
    extEnv :: FastRef (S.Set DD.DefiniteDescription),
    definedNameSet :: FastRef (S.Set DD.DefiniteDescription),
    compEnv :: FastRef (Map.HashMap DD.DefiniteDescription (O.Opacity, [Ident], Comp)),
    typeEnv :: FastRef (Map.HashMap DD.DefiniteDescription WT.WeakTerm),
    activeGlobalLocatorList :: FastRef [SGL.StrictGlobalLocator],
    currentGlobalLocator :: Ref SGL.StrictGlobalLocator,
    currentSource :: Ref Source.Source,
    mainModule :: Ref Module.Module,
    targetPlatform :: Ref TP.TargetPlatform
  }

type Ref a = IORef (Maybe a)

type FastRef a = IORef a

newFastRef :: Monoid a => IO (FastRef a)
newFastRef =
  newIORef mempty

newRef :: IO (Ref a)
newRef =
  newIORef Nothing

newEnv :: IO Env
newEnv = do
  counter <- newIORefU 0
  endOfEntry <- newFastRef
  clangOptString <- newFastRef
  shouldColorize <- newIORef True
  buildMode <- newIORef BM.Develop
  moduleCacheMap <- newFastRef
  moduleAliasMap <- newFastRef
  locatorAliasMap <- newFastRef
  sourceNameMap <- newFastRef
  remarkList <- newFastRef
  unusedVariableMap <- newFastRef
  nameMap <- newFastRef
  antecedentMap <- newFastRef
  constraintEnv <- newFastRef
  holeSubst <- newFastRef
  sourceChildrenMap <- newFastRef
  weakTypeEnv <- newFastRef
  preHoleEnv <- newFastRef
  holeEnv <- newFastRef
  constraintQueue <- newFastRef
  traceSourceList <- newFastRef
  artifactMap <- newFastRef
  definedNameSet <- newFastRef
  visitEnv <- newFastRef
  weakDefMap <- newFastRef
  defMap <- newFastRef
  staticTextList <- newFastRef
  compDefMap <- newFastRef
  dataDefMap <- newFastRef
  codataDefMap <- newFastRef
  keyArgMap <- newFastRef
  enumSet <- newFastRef
  impArgEnv <- newFastRef
  declEnv <- newFastRef
  extEnv <- newFastRef
  compEnv <- newFastRef
  typeEnv <- newFastRef
  activeGlobalLocatorList <- newFastRef
  currentGlobalLocator <- newRef
  currentSource <- newRef
  mainModule <- newRef
  targetPlatform <- newRef
  return Env {..}
