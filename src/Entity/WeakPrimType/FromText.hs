module Entity.WeakPrimType.FromText (fromText) where

import Data.Text qualified as T
import Entity.DataSize qualified as DS
import Entity.PrimNumSize
import Entity.WeakPrimType qualified as PT
import Text.Read

fromText :: DS.DataSize -> T.Text -> Maybe PT.WeakPrimType
fromText dataSize name
  | Just intSize <- asLowInt dataSize name =
      Just $ PT.Int intSize
  | Just floatSize <- asLowFloat dataSize name =
      Just $ PT.Float floatSize
  | otherwise =
      Nothing

intTypeName :: T.Text
intTypeName = "int"

floatTypeName :: T.Text
floatTypeName = "float"

asLowInt :: DS.DataSize -> T.Text -> Maybe (PT.WeakSize IntSize)
asLowInt dataSize s = do
  rest <- T.stripPrefix intTypeName s
  if T.null rest
    then return $ PT.Implicit $ dataSizeToIntSize dataSize
    else do
      size <- readMaybe $ T.unpack rest
      PT.Explicit <$> asIntSize dataSize size

asLowFloat :: DS.DataSize -> T.Text -> Maybe (PT.WeakSize FloatSize)
asLowFloat dataSize s = do
  rest <- T.stripPrefix floatTypeName s
  if T.null rest
    then return $ PT.Implicit $ dataSizeToFloatSize dataSize
    else do
      size <- readMaybe $ T.unpack rest
      PT.Explicit <$> asFloatSize dataSize size

dataSizeToIntSize :: DS.DataSize -> IntSize
dataSizeToIntSize dataSize =
  IntSize $ DS.reify dataSize

dataSizeToFloatSize :: DS.DataSize -> FloatSize
dataSizeToFloatSize dataSize =
  case dataSize of
    DS.DataSize64 ->
      FloatSize64

asIntSize :: DS.DataSize -> Int -> Maybe IntSize
asIntSize dataSize size =
  if 1 <= size && size <= DS.reify dataSize
    then return $ IntSize size
    else Nothing

asFloatSize :: DS.DataSize -> Int -> Maybe FloatSize
asFloatSize dataSize size =
  if size > DS.reify dataSize
    then Nothing
    else case size of
      16 ->
        return FloatSize16
      32 ->
        return FloatSize32
      64 ->
        return FloatSize64
      _ ->
        Nothing
