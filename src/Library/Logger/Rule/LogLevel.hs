module Library.Logger.Rule.LogLevel
  ( LogLevel (..),
  )
where

import Data.Binary
import GHC.Generics (Generic)

data LogLevel
  = Note
  | Warning
  | Error
  | Critical
  deriving (Show, Eq, Generic)

instance Binary LogLevel
