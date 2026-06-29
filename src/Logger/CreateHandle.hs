module Logger.CreateHandle (createHandle) where

import Console.Handle qualified as Console
import Data.IORef (newIORef)
import Data.Time
import Logger.Handle (Handle (..))

createHandle :: Console.Handle -> Bool -> IO Handle
createHandle _consoleHandle _enableDebugMode = do
  _baseTime <- getCurrentTime
  _moduleDirRef <- newIORef ""
  return $ InternalHandle {_consoleHandle, _enableDebugMode, _baseTime, _moduleDirRef}
