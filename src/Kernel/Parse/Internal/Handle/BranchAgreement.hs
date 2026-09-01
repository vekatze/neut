module Kernel.Parse.Internal.Handle.BranchAgreement
  ( Handle,
    Obligation (..),
    new,
    insert,
    get,
  )
where

import Data.IORef
import Language.Common.DefiniteDescription qualified as DD
import Logger.Hint

data Obligation = Obligation
  { obligationHint :: Hint,
    thenName :: DD.DefiniteDescription,
    elseName :: DD.DefiniteDescription
  }

newtype Handle = Handle
  { obligationListRef :: IORef [Obligation]
  }

new :: IO Handle
new = do
  obligationListRef <- newIORef []
  return $ Handle {..}

insert :: Handle -> Obligation -> IO ()
insert h obligation =
  atomicModifyIORef' (obligationListRef h) $ \obligationList ->
    (obligation : obligationList, ())

get :: Handle -> IO [Obligation]
get h =
  readIORef (obligationListRef h)
