module Aux.Logger.Move.CreateHandle (createHandle) where

import Aux.Color.Rule.Handle qualified as Color
import Aux.Logger.Rule.Handle (Handle (..))
import Data.Text qualified as T
import Data.Time

createHandle :: Color.Handle -> T.Text -> Bool -> IO Handle
createHandle _colorHandle _endOfEntry _enableDebugMode = do
  _baseTime <- getCurrentTime
  return $ InternalHandle {_colorHandle, _endOfEntry, _enableDebugMode, _baseTime}
