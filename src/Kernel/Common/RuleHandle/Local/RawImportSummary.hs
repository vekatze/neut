module Kernel.Common.RuleHandle.Local.RawImportSummary
  ( Handle (..),
  )
where

import Data.IORef
import Kernel.Common.RawImportSummary qualified as RIS

newtype Handle = Handle
  { _importEnvRef :: IORef (Maybe RIS.RawImportSummary)
  }
