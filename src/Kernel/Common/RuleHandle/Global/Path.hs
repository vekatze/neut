module Kernel.Common.RuleHandle.Global.Path
  ( Handle (..),
  )
where

import Data.IORef
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.Module
import Logger.Handle qualified as Logger

data Handle = Handle
  { _mainModule :: MainModule,
    _cacheRef :: IORef (Maybe String),
    _loggerHandle :: Logger.Handle,
    _platformHandle :: Platform.Handle
  }
