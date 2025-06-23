module Language.Common.Rule.PrimNumSize
  ( IntSize (..),
    FloatSize (..),
    dataSizeToIntSize,
    dataSizeToFloatSize,
    intToIntSize,
    intToFloatSize,
  )
where

import Data.Binary
import GHC.Generics qualified as G
import Language.Common.Rule.DataSize qualified as DS

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
    DS.DataSize64 ->
      IntSize64

dataSizeToFloatSize :: DS.DataSize -> FloatSize
dataSizeToFloatSize dataSize =
  case dataSize of
    DS.DataSize64 ->
      FloatSize64

intToIntSize :: DS.DataSize -> Int -> Maybe IntSize
intToIntSize dataSize size =
  if size > DS.reify dataSize
    then Nothing
    else do
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

intToFloatSize :: DS.DataSize -> Int -> Maybe FloatSize
intToFloatSize dataSize size =
  if size > DS.reify dataSize
    then Nothing
    else do
      case size of
        16 ->
          return FloatSize16
        32 ->
          return FloatSize32
        64 ->
          return FloatSize64
        _ ->
          Nothing
