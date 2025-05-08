module Kernel.Common.Rule.Handle.Local.RawImportSummary
  ( Handle (..),
  )
where

import Data.IORef
import Kernel.Common.Rule.RawImportSummary qualified as RIS

newtype Handle = Handle
  { _importEnvRef :: IORef (Maybe RIS.RawImportSummary)
  }
