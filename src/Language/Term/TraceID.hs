module Language.Term.TraceID
  ( TraceID (..),
    noTrace,
  )
where

import Binary.Derive (deriveBinary)

newtype TraceID = TraceID Int
  deriving (Eq, Ord, Show)

noTrace :: TraceID
noTrace =
  TraceID 0

deriveBinary [''TraceID]
