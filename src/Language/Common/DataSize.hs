module Language.Common.DataSize
  ( DataSize (..),
    reify,
    reifyBytes,
  )
where

data DataSize
  = DataSize32
  | DataSize64
  deriving (Eq)

reify :: DataSize -> Int
reify dataSize =
  case dataSize of
    DataSize32 ->
      32
    DataSize64 ->
      64

reifyBytes :: DataSize -> Int
reifyBytes dataSize =
  reify dataSize `div` 8
