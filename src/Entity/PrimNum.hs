module Entity.PrimNum
  ( PrimNum (..),
  )
where

import GHC.Generics (Generic)

data PrimNum a
  = Int a Integer
  | Double a Double
  deriving (Show, Generic)
