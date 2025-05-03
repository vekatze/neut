module Main.Rule.OptimizableData (OptimizableData (..)) where

data OptimizableData
  = Enum
  | Unary -- for newtype-ish optimization
  deriving (Show)
