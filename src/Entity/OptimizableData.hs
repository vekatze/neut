module Entity.OptimizableData (OptimizableData (..)) where

data OptimizableData
  = Enum
  | Unitary -- for newtype-ish optimization
