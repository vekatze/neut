module Aux.Logger.Move.CreateHandle (createHandle) where

import Aux.Color.Rule.Handle qualified as Color
import Aux.Logger.Rule.Handle (Handle (..))
import Data.Time

createHandle :: Color.Handle -> Bool -> IO Handle
createHandle _colorHandle _enableDebugMode = do
  _baseTime <- getCurrentTime
  return $ InternalHandle {_colorHandle, _enableDebugMode, _baseTime}
