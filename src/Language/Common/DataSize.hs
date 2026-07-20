module Language.Common.DataSize
  ( DataSize (..),
    reify,
    reifyBytes,
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

reifyBytes :: DataSize -> Int
reifyBytes dataSize =
  reify dataSize `div` 8
