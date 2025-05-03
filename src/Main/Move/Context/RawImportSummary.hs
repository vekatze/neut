module Main.Move.Context.RawImportSummary
  ( Handle,
    new,
    set,
    get,
  )
where

import Data.IORef
import Main.Rule.RawImportSummary qualified as RIS
import Main.Rule.RawProgram (RawImport)

newtype Handle = Handle
  { importEnvRef :: IORef (Maybe RIS.RawImportSummary)
  }

new :: IO Handle
new = do
  importEnvRef <- newIORef Nothing
  return $ Handle {..}

set :: Handle -> RawImport -> IO ()
set h rawImport = do
  writeIORef (importEnvRef h) $ Just (RIS.fromRawImport rawImport)

get :: Handle -> IO (Maybe RIS.RawImportSummary)
get h =
  readIORef (importEnvRef h)
