module Move.Context.App.Internal
  ( Env (..),
    Ref,
    newEnv,
  )
where

import Data.IORef
import Rule.Remark qualified as Remark

newtype Env = Env
  { globalRemarkList :: IORef [Remark.Remark]
  }

type Ref a = IORef (Maybe a)

newEnv :: IO Env
newEnv = do
  globalRemarkList <- newIORef []
  return Env {..}
