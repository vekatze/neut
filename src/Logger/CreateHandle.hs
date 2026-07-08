module Logger.CreateHandle (createHandle) where

import Console.Handle qualified as Console
import Data.IORef (newIORef)
import Data.Time
import Logger.Handle (Handle (..))

createHandle :: Console.Handle -> IO Handle
createHandle _consoleHandle = do
  _baseTime <- getCurrentTime
  _moduleDirRef <- newIORef ""
  return $ InternalHandle {_consoleHandle, _baseTime, _moduleDirRef}
