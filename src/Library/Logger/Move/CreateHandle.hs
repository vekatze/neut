module Library.Logger.Move.CreateHandle (createHandle) where

import Data.Text qualified as T
import Data.Time
import Library.Color.Rule.Handle qualified as Color
import Library.Logger.Rule.Handle (Handle (..))

createHandle :: Color.Handle -> T.Text -> Bool -> IO Handle
createHandle _colorHandle _endOfEntry _enableDebugMode = do
  _baseTime <- getCurrentTime
  return $ InternalHandle {_colorHandle, _endOfEntry, _enableDebugMode, _baseTime}
