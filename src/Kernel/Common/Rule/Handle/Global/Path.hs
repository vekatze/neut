module Kernel.Common.Rule.Handle.Global.Path
  ( Handle (..),
  )
where

import Data.IORef
import Kernel.Common.Rule.Handle.Global.Env qualified as Env
import Kernel.Common.Rule.Handle.Global.Platform qualified as Platform
import Logger.Rule.Handle qualified as Logger

data Handle = Handle
  { _cacheRef :: IORef (Maybe String),
    _loggerHandle :: Logger.Handle,
    _envHandle :: Env.Handle,
    _platformHandle :: Platform.Handle
  }
