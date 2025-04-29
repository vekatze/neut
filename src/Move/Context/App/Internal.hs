module Move.Context.App.Internal
  ( Env (..),
    Ref,
    newEnv,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Rule.Binder
import Rule.DeclarationName qualified as DN
import Rule.DefiniteDescription qualified as DD
import Rule.ForeignCodType qualified as F
import Rule.Remark qualified as Remark
import Rule.Term qualified as TM
import Rule.WeakTerm qualified as WT

data Env = Env
  { remarkList :: IORef [Remark.Remark], -- per file
    globalRemarkList :: IORef [Remark.Remark],
    weakDefMap :: IORef (Map.HashMap DD.DefiniteDescription WT.WeakTerm),
    defMap :: IORef (Map.HashMap DD.DefiniteDescription ([BinderF TM.Term], TM.Term)),
    weakDeclEnv :: IORef (Map.HashMap DN.DeclarationName ([WT.WeakTerm], F.ForeignCodType WT.WeakTerm))
  }

type Ref a = IORef (Maybe a)

newEnv :: IO Env
newEnv = do
  remarkList <- newIORef []
  globalRemarkList <- newIORef []
  weakDefMap <- newIORef Map.empty
  defMap <- newIORef Map.empty
  weakDeclEnv <- newIORef Map.empty
  return Env {..}
