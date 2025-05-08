module Kernel.Common.Rule.Handle.Global.Path
  ( Handle (..),
  )
where

import Aux.Logger.Rule.Handle qualified as Logger
import Data.IORef
import Kernel.Common.Rule.Handle.Global.Platform qualified as Platform
import Kernel.Common.Rule.Module

data Handle = Handle
  { _mainModule :: MainModule,
    _cacheRef :: IORef (Maybe String),
    _loggerHandle :: Logger.Handle,
    _platformHandle :: Platform.Handle
  }
