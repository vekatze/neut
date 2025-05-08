module Kernel.Common.Rule.Handle.Global.GlobalRemark
  ( Handle (..),
  )
where

import Aux.Logger.Rule.Log qualified as L
import Data.IORef

newtype Handle = Handle
  { _globalRemarkListRef :: IORef [L.Log]
  }
