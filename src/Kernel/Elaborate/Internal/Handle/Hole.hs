module Kernel.Elaborate.Internal.Handle.Hole
  ( Handle,
    new,
    insert,
    lookup,
    insertTypeSubst,
    getTypeSubst,
    setTypeSubst,
  )
where

import Data.IORef
import Data.IntMap qualified as IntMap
import Kernel.Elaborate.TypeHoleSubst qualified as THS
import Language.Common.HoleID qualified as HID
import Language.Common.Ident
import Language.WeakTerm.WeakTerm qualified as WT
import Prelude hiding (lookup)

data Handle = Handle
  { holeEnvRef :: IORef (IntMap.IntMap (WT.WeakType, WT.WeakType)),
    typeHoleSubstRef :: IORef THS.TypeHoleSubst
  }

new :: IO Handle
new = do
  holeEnvRef <- newIORef IntMap.empty
  typeHoleSubstRef <- newIORef THS.empty
  return $ Handle {..}

insert :: Handle -> Int -> WT.WeakType -> WT.WeakType -> IO ()
insert h i e1 e2 =
  modifyIORef' (holeEnvRef h) $ IntMap.insert i (e1, e2)

lookup :: Handle -> Int -> IO (Maybe (WT.WeakType, WT.WeakType))
lookup h i =
  IntMap.lookup i <$> readIORef (holeEnvRef h)

insertTypeSubst :: Handle -> HID.HoleID -> [Ident] -> WT.WeakType -> IO ()
insertTypeSubst h holeID xs e =
  modifyIORef' (typeHoleSubstRef h) $ THS.insert holeID xs e

getTypeSubst :: Handle -> IO THS.TypeHoleSubst
getTypeSubst h =
  readIORef (typeHoleSubstRef h)

setTypeSubst :: Handle -> THS.TypeHoleSubst -> IO ()
setTypeSubst h =
  writeIORef (typeHoleSubstRef h)
