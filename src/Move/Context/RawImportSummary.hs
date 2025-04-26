module Move.Context.RawImportSummary
  ( Handle,
    new,
    initialize,
    set,
    get,
  )
where

import Control.Monad.Reader (asks)
import Data.IORef
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Rule.RawImportSummary qualified as RIS
import Rule.RawProgram (RawImport)

newtype Handle = Handle
  { importEnvRef :: IORef (Maybe RIS.RawImportSummary)
  }

new :: App Handle
new = do
  importEnvRef <- asks App.importEnv
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
