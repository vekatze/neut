module Kernel.Elaborate.Internal.Handle.Hole
  ( Handle,
    new,
    insertTypeSubst,
    getTypeSubst,
    setTypeSubst,
  )
where

import Data.IORef
import Kernel.Elaborate.TypeHoleSubst qualified as THS
import Language.Common.HoleID qualified as HID
import Language.Common.Ident
import Language.WeakTerm.WeakTerm qualified as WT

data Handle = Handle
  { typeHoleSubstRef :: IORef THS.TypeHoleSubst
  }

new :: IO Handle
new = do
  typeHoleSubstRef <- newIORef THS.empty
  return $ Handle {..}

insertTypeSubst :: Handle -> HID.HoleID -> [Ident] -> WT.WeakType -> IO ()
insertTypeSubst h holeID xs e =
  modifyIORef' (typeHoleSubstRef h) $ THS.insert holeID xs e

getTypeSubst :: Handle -> IO THS.TypeHoleSubst
getTypeSubst h =
  readIORef (typeHoleSubstRef h)

setTypeSubst :: Handle -> THS.TypeHoleSubst -> IO ()
setTypeSubst h =
  writeIORef (typeHoleSubstRef h)
