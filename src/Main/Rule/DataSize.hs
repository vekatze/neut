module Main.Rule.DataSize
  ( DataSize (..),
    reify,
  )
where

data DataSize
  = DataSize64
  deriving (Eq)

reify :: DataSize -> Int
reify dataSize =
  case dataSize of
    DataSize64 ->
      64
