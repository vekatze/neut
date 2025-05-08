module Kernel.Common.Move.Handle.Local.RawImportSummary
  ( new,
    set,
    get,
  )
where

import Data.IORef
import Kernel.Common.Rule.Handle.Local.RawImportSummary
import Kernel.Common.Rule.RawImportSummary qualified as RIS
import Language.RawTerm.Rule.RawStmt (RawImport)

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
