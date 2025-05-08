module Kernel.Common.Rule.Handle.Local.TopCandidate
  ( Handle (..),
  )
where

import Data.IORef
import Kernel.Common.Rule.TopCandidate

newtype Handle = Handle
  { _topCandidateEnvRef :: IORef [TopCandidate]
  }
