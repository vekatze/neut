module Kernel.Common.Handle.Global.Data
  ( Handle (..),
    new,
    insert,
    insertWeak,
    lookup,
    lookupWeak,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Language.Common.Binder
import Language.Common.DataInfo (DataInfo)
import Language.Common.DefiniteDescription qualified as DD
import Language.Term.Term qualified as TM
import Language.WeakTerm.WeakTerm qualified as WT
import Prelude hiding (lookup)

data Handle = Handle
  { _dataMapRef :: IORef (Map.HashMap DD.DefiniteDescription (DataInfo (BinderF TM.Type))),
    _weakDataMapRef :: IORef (Map.HashMap DD.DefiniteDescription (DataInfo (BinderF WT.WeakType)))
  }

new :: IO Handle
new = do
  _dataMapRef <- newIORef Map.empty
  _weakDataMapRef <- newIORef Map.empty
  return $ Handle {..}

insert :: Handle -> DD.DefiniteDescription -> DataInfo (BinderF TM.Type) -> IO ()
insert h dataName dataInfo =
  atomicModifyIORef' (_dataMapRef h) (\mp -> (Map.insert dataName dataInfo mp, ()))

insertWeak :: Handle -> DD.DefiniteDescription -> DataInfo (BinderF WT.WeakType) -> IO ()
insertWeak h dataName dataInfo =
  atomicModifyIORef' (_weakDataMapRef h) (\mp -> (Map.insert dataName dataInfo mp, ()))

lookup :: Handle -> DD.DefiniteDescription -> IO (Maybe (DataInfo (BinderF TM.Type)))
lookup h dataName = do
  dataMap <- readIORef (_dataMapRef h)
  return $ Map.lookup dataName dataMap

lookupWeak :: Handle -> DD.DefiniteDescription -> IO (Maybe (DataInfo (BinderF WT.WeakType)))
lookupWeak h dataName = do
  weakDataMap <- readIORef (_weakDataMapRef h)
  return $ Map.lookup dataName weakDataMap
