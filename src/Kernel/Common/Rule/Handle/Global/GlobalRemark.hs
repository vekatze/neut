module Kernel.Common.Rule.Handle.Global.GlobalRemark
  ( Handle (..),
  )
where

import Data.IORef
import Logger.Rule.Log qualified as L

newtype Handle = Handle
  { _globalRemarkListRef :: IORef [L.Log]
  }
