module Move.Context.App.Internal
  ( Env (..),
    Ref,
    newEnv,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Path
import Rule.Artifact qualified as AR
import Rule.Binder
import Rule.Comp
import Rule.DeclarationName qualified as DN
import Rule.DefiniteDescription qualified as DD
import Rule.ExternalName qualified as EN
import Rule.ForeignCodType qualified as F
import Rule.Hint
import Rule.Ident
import Rule.LocalVarTree qualified as LVT
import Rule.Opacity qualified as O
import Rule.Remark qualified as Remark
import Rule.Term qualified as TM
import Rule.TopCandidate
import Rule.WeakTerm qualified as WT

data Env = Env
  { remarkList :: IORef [Remark.Remark], -- per file
    globalRemarkList :: IORef [Remark.Remark],
    localVarMap :: IORef LVT.LocalVarTree,
    topCandidateEnv :: IORef [TopCandidate],
    buildSignatureCache :: IORef (Maybe String), -- only for memoization
    artifactMap :: IORef (Map.HashMap (Path Abs File) AR.ArtifactTime),
    weakDefMap :: IORef (Map.HashMap DD.DefiniteDescription WT.WeakTerm),
    defMap :: IORef (Map.HashMap DD.DefiniteDescription ([BinderF TM.Term], TM.Term)),
    compAuxEnv :: IORef (Map.HashMap DD.DefiniteDescription (O.Opacity, [Ident], Comp)),
    preDeclEnv :: IORef (Map.HashMap EN.ExternalName Hint),
    weakDeclEnv :: IORef (Map.HashMap DN.DeclarationName ([WT.WeakTerm], F.ForeignCodType WT.WeakTerm)),
    compEnv :: IORef (Map.HashMap DD.DefiniteDescription (O.Opacity, [Ident], Comp)),
    clangDigest :: Ref T.Text
  }

type Ref a = IORef (Maybe a)

newRef :: IO (Ref a)
newRef =
  newIORef Nothing

newEnv :: IO Env
newEnv = do
  remarkList <- newIORef []
  globalRemarkList <- newIORef []
  localVarMap <- newIORef LVT.empty
  topCandidateEnv <- newIORef []
  buildSignatureCache <- newIORef Nothing
  artifactMap <- newIORef Map.empty
  weakDefMap <- newIORef Map.empty
  defMap <- newIORef Map.empty
  compAuxEnv <- newIORef Map.empty
  preDeclEnv <- newIORef Map.empty
  weakDeclEnv <- newIORef Map.empty
  compEnv <- newIORef Map.empty
  clangDigest <- newRef
  return Env {..}
