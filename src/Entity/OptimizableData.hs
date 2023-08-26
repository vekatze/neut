module Entity.OptimizableData (OptimizableData (..)) where

data OptimizableData
  = Enum
  | Nat
  | Unitary -- for newtype-ish optimization
