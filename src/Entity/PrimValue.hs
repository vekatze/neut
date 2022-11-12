module Entity.PrimValue
  ( PrimValue (..),
  )
where

import GHC.Generics (Generic)

data PrimValue a
  = Int a Integer
  | Double a Double
  deriving (Show, Generic)
