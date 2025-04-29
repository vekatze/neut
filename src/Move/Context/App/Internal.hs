module Move.Context.App.Internal
  ( Env (..),
    Ref,
    newEnv,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Rule.Binder
import Rule.DefiniteDescription qualified as DD
import Rule.Remark qualified as Remark
import Rule.Term qualified as TM
import Rule.WeakTerm qualified as WT

data Env = Env
  { globalRemarkList :: IORef [Remark.Remark],
    weakDefMap :: IORef (Map.HashMap DD.DefiniteDescription WT.WeakTerm),
    defMap :: IORef (Map.HashMap DD.DefiniteDescription ([BinderF TM.Term], TM.Term))
  }

type Ref a = IORef (Maybe a)

newEnv :: IO Env
newEnv = do
  globalRemarkList <- newIORef []
  weakDefMap <- newIORef Map.empty
  defMap <- newIORef Map.empty
  return Env {..}
