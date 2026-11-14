module Kernel.Common.Handle.Global.Data
  ( Handle (..),
    new,
    insertMany,
    insertWeakMany,
    lookup,
    lookupWeak,
    lookupOrCritical,
    lookupWeakOrCritical,
  )
where

import App.App (App)
import App.Run (raiseCritical)
import Control.Monad.IO.Class (liftIO)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Kernel.Common.HashMapRef (insertEntries)
import Language.Common.Binder
import Language.Common.DataInfo (DataInfo)
import Language.Common.DefiniteDescription qualified as DD
import Language.Term.Term qualified as TM
import Language.WeakTerm.WeakTerm qualified as WT
import Logger.Hint (Hint)
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

insertMany :: Handle -> [(DD.DefiniteDescription, DataInfo (BinderF TM.Type))] -> IO ()
insertMany h =
  insertEntries (_dataMapRef h)

insertWeakMany :: Handle -> [(DD.DefiniteDescription, DataInfo (BinderF WT.WeakType))] -> IO ()
insertWeakMany h =
  insertEntries (_weakDataMapRef h)

lookup :: Handle -> DD.DefiniteDescription -> IO (Maybe (DataInfo (BinderF TM.Type)))
lookup h dataName = do
  dataMap <- readIORef (_dataMapRef h)
  return $ Map.lookup dataName dataMap

lookupWeak :: Handle -> DD.DefiniteDescription -> IO (Maybe (DataInfo (BinderF WT.WeakType)))
lookupWeak h dataName = do
  weakDataMap <- readIORef (_weakDataMapRef h)
  return $ Map.lookup dataName weakDataMap

lookupOrCritical :: Handle -> Hint -> DD.DefiniteDescription -> App (DataInfo (BinderF TM.Type))
lookupOrCritical h m dataName = do
  dataInfoOrNone <- liftIO $ lookup h dataName
  case dataInfoOrNone of
    Just dataInfo ->
      return dataInfo
    Nothing ->
      raiseMissingDataInfo m dataName

lookupWeakOrCritical :: Handle -> Hint -> DD.DefiniteDescription -> App (DataInfo (BinderF WT.WeakType))
lookupWeakOrCritical h m dataName = do
  dataInfoOrNone <- liftIO $ lookupWeak h dataName
  case dataInfoOrNone of
    Just dataInfo ->
      return dataInfo
    Nothing ->
      raiseMissingDataInfo m dataName

raiseMissingDataInfo :: Hint -> DD.DefiniteDescription -> App a
raiseMissingDataInfo m dataName =
  raiseCritical m $ "Could not find constructor metadata for `" <> DD.reify dataName <> "`"
