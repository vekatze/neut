module Language.Term.TraceID
  ( TraceID (..),
    noTrace,
  )
where

import Data.Binary (Binary)
import GHC.Generics (Generic)

newtype TraceID = TraceID Int
  deriving (Eq, Ord, Show, Generic)

instance Binary TraceID

noTrace :: TraceID
noTrace =
  TraceID 0
