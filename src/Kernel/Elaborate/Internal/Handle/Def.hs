module Kernel.Elaborate.Internal.Handle.Def
  ( Handle,
    new,
    insert',
    get',
  )
where

import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Language.Common.Binder
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Opacity qualified as O
import Language.Term.Term qualified as TM
import Prelude hiding (lookup, read)

newtype Handle = Handle
  { defMapRef :: IORef (Map.HashMap DD.DefiniteDescription ([BinderF TM.Term], TM.Term))
  }

new :: IO Handle
new = do
  defMapRef <- newIORef Map.empty
  return $ Handle {..}

insert' :: Handle -> O.Opacity -> DD.DefiniteDescription -> [BinderF TM.Term] -> TM.Term -> IO ()
insert' h opacity name xts e =
  when (opacity == O.Clear) $
    modifyIORef' (defMapRef h) $
      Map.insert name (xts, e)

get' :: Handle -> IO (Map.HashMap DD.DefiniteDescription ([BinderF TM.Term], TM.Term))
get' h =
  readIORef (defMapRef h)
