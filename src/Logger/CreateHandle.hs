module Logger.CreateHandle (createHandle) where

import Color.Handle qualified as Color
import Data.IORef (newIORef)
import Data.Time
import Logger.Handle (Handle (..))

createHandle :: Color.Handle -> Bool -> IO Handle
createHandle _colorHandle _enableDebugMode = do
  _baseTime <- getCurrentTime
  _moduleDirRef <- newIORef ""
  return $ InternalHandle {_colorHandle, _enableDebugMode, _baseTime, _moduleDirRef}
