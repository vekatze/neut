module Kernel.Common.Handle.Local.RawImportSummary
  ( Handle (..),
    new,
    set,
    get,
  )
where

import Data.IORef
import Kernel.Common.RawImportSummary qualified as RIS
import Language.RawTerm.RawStmt (RawImport)

newtype Handle = Handle
  { _importEnvRef :: IORef (Maybe RIS.RawImportSummary)
  }

new :: IO Handle
new = do
  _importEnvRef <- newIORef Nothing
  return $ Handle {..}

set :: Handle -> RawImport -> IO ()
set h rawImport = do
  writeIORef (_importEnvRef h) $ Just (RIS.fromRawImport rawImport)

get :: Handle -> IO (Maybe RIS.RawImportSummary)
get h =
  readIORef (_importEnvRef h)
