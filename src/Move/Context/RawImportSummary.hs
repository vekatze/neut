module Move.Context.RawImportSummary
  ( Handle,
    new,
    initialize,
    set,
    get,
  )
where

import Data.IORef
import Rule.RawImportSummary qualified as RIS
import Rule.RawProgram (RawImport)

newtype Handle = Handle
  { importEnvRef :: IORef (Maybe RIS.RawImportSummary)
  }

new :: IO Handle
new = do
  importEnvRef <- newIORef Nothing
  return $ Handle {..}

initialize :: Handle -> IO ()
initialize h =
  writeIORef (importEnvRef h) Nothing

set :: Handle -> RawImport -> IO ()
set h rawImport = do
  writeIORef (importEnvRef h) $ Just (RIS.fromRawImport rawImport)

get :: Handle -> IO (Maybe RIS.RawImportSummary)
get h =
  readIORef (importEnvRef h)
