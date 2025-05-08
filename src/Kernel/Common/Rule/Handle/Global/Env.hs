module Kernel.Common.Rule.Handle.Global.Env
  ( Handle (..),
    getMainModule,
    getSilentMode,
  )
where

import Data.IORef
import Kernel.Common.Rule.BuildMode qualified as BM
import Kernel.Common.Rule.Module

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
