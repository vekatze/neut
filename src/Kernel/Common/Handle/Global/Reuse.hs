module Kernel.Common.Handle.Global.Reuse
  ( Handle,
    Header,
    disabled,
    new,
    rememberCache,
    rememberHeader,
    rememberModule,
  )
where

import App.App (App)
import Control.Monad.IO.Class (liftIO)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Time (UTCTime)
import Kernel.Common.Cache qualified as Cache
import Kernel.Common.Module (Module)
import Language.RawTerm.RawStmt (RawImport, RawRequire)
import Path
import SyntaxTree.C (C)
import System.Directory (getFileSize, getModificationTime)

type Header =
  ([(RawImport, C)], [(RawRequire, C)])

data Stamp = Stamp UTCTime Integer
  deriving (Eq)

type Table a =
  IORef (Map.HashMap FilePath (Stamp, a))

data Tables = Tables
  { caches :: Table Cache.LowCache,
    headers :: Table Header,
    modules :: Table Module
  }

newtype Handle = Handle (Maybe Tables)

disabled :: Handle
disabled =
  Handle Nothing

new :: IO Handle
new = do
  caches <- newIORef Map.empty
  headers <- newIORef Map.empty
  modules <- newIORef Map.empty
  return $ Handle $ Just $ Tables {..}

rememberCache :: Handle -> Path Abs File -> IO (Either e Cache.LowCache) -> IO (Either e Cache.LowCache)
rememberCache (Handle tablesOrNone) path decode =
  case tablesOrNone of
    Nothing ->
      decode
    Just tables ->
      remember (caches tables) (toFilePath path) decode

rememberHeader :: Handle -> Path Abs File -> App Header -> App Header
rememberHeader (Handle tablesOrNone) path parse =
  case tablesOrNone of
    Nothing ->
      parse
    Just tables ->
      rememberApp (headers tables) (toFilePath path) parse

rememberModule :: Handle -> Path Abs File -> App Module -> App Module
rememberModule (Handle tablesOrNone) path parse =
  case tablesOrNone of
    Nothing ->
      parse
    Just tables ->
      rememberApp (modules tables) (toFilePath path) parse

remember :: Table a -> FilePath -> IO (Either e a) -> IO (Either e a)
remember table path produce = do
  stamp <- getStamp path
  found <- lookupStamped table path stamp
  case found of
    Just value ->
      return $ Right value
    Nothing -> do
      result <- produce
      case result of
        Right value ->
          insertStamped table path stamp value
        Left _ ->
          return ()
      return result

rememberApp :: Table a -> FilePath -> App a -> App a
rememberApp table path produce = do
  stamp <- liftIO $ getStamp path
  found <- liftIO $ lookupStamped table path stamp
  case found of
    Just value ->
      return value
    Nothing -> do
      value <- produce
      liftIO $ insertStamped table path stamp value
      return value

getStamp :: FilePath -> IO Stamp
getStamp path = do
  time <- getModificationTime path
  size <- getFileSize path
  return $ Stamp time size

lookupStamped :: Table a -> FilePath -> Stamp -> IO (Maybe a)
lookupStamped table path stamp = do
  entries <- readIORef table
  case Map.lookup path entries of
    Just (stamp', value)
      | stamp' == stamp ->
          return $ Just value
    _ ->
      return Nothing

insertStamped :: Table a -> FilePath -> Stamp -> a -> IO ()
insertStamped table path stamp value =
  atomicModifyIORef' table $ \entries ->
    (Map.insert path (stamp, value) entries, ())
