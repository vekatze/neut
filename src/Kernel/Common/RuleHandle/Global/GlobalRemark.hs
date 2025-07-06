module Kernel.Common.RuleHandle.Global.GlobalRemark
  ( Handle (..),
  )
where

import Data.IORef
import Logger.Log qualified as L

newtype Handle = Handle
  { _globalRemarkListRef :: IORef [L.Log]
  }
