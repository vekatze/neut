module Kernel.Common.RuleHandle.Local.TopCandidate
  ( Handle (..),
  )
where

import Data.IORef
import Kernel.Common.TopCandidate

newtype Handle = Handle
  { _topCandidateEnvRef :: IORef [TopCandidate]
  }
