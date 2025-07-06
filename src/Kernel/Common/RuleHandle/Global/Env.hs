module Kernel.Common.RuleHandle.Global.Env
  ( Handle (..),
    getMainModule,
    getSilentMode,
  )
where

import Data.IORef
import Kernel.Common.BuildMode qualified as BM
import Kernel.Common.Module

data Handle = Handle
  { _buildModeRef :: IORef BM.BuildMode,
    _enableSilentMode :: Bool,
    _mainModule :: MainModule
  }

getMainModule :: Handle -> MainModule
getMainModule =
  _mainModule

getSilentMode :: Handle -> Bool
getSilentMode =
  _enableSilentMode
