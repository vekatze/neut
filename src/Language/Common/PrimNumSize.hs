module Language.Common.PrimNumSize
  ( IntSize (..),
    FloatSize (..),
    dataSizeToIntSize,
    intToIntSize,
    intToFloatSize,
    floatSizeToIntSize,
  )
where

import Data.Binary
import GHC.Generics qualified as G
import Language.Common.DataSize qualified as DS

data IntSize
  = IntSize1
  | IntSize2
  | IntSize4
  | IntSize8
  | IntSize16
  | IntSize32
  | IntSize64
  deriving (Eq, Ord, Show, G.Generic)

instance Binary IntSize

data FloatSize
  = FloatSize16
  | FloatSize32
  | FloatSize64
  deriving (Eq, Ord, Show, G.Generic)

instance Binary FloatSize

dataSizeToIntSize :: DS.DataSize -> IntSize
dataSizeToIntSize dataSize =
  case dataSize of
    DS.DataSize32 ->
      IntSize32
    DS.DataSize64 ->
      IntSize64

intToIntSize :: Int -> Maybe IntSize
intToIntSize size =
  case size of
    1 ->
      return IntSize1
    2 ->
      return IntSize2
    4 ->
      return IntSize4
    8 ->
      return IntSize8
    16 ->
      return IntSize16
    32 ->
      return IntSize32
    64 ->
      return IntSize64
    _ ->
      Nothing

intToFloatSize :: Int -> Maybe FloatSize
intToFloatSize size =
  case size of
    16 ->
      return FloatSize16
    32 ->
      return FloatSize32
    64 ->
      return FloatSize64
    _ ->
      Nothing

floatSizeToIntSize :: FloatSize -> IntSize
floatSizeToIntSize floatSize =
  case floatSize of
    FloatSize16 ->
      IntSize16
    FloatSize32 ->
      IntSize32
    FloatSize64 ->
      IntSize64
