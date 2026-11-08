module Logger.LogLevel
  ( LogLevel (..),
  )
where

import Binary.Derive (deriveBinary)

data LogLevel
  = Note
  | Warning
  | Error
  | Critical
  deriving (Show, Eq)

deriveBinary [''LogLevel]
