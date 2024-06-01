module Entity.PrimType.FromWeakPrimType (fromWeakPrimType) where

import Data.Text qualified as T
import Entity.DataSize qualified as DS
import Entity.PrimNumSize
import Entity.PrimType qualified as PT
import Entity.WeakPrimType qualified as WPT

fromWeakPrimType :: DS.DataSize -> WPT.WeakPrimType -> Either T.Text PT.PrimType
fromWeakPrimType dataSize wpt =
  case wpt of
    WPT.Int sizeOrNone -> do
      case sizeOrNone of
        Just size -> do
          size' <- asIntSize dataSize size
          return $ PT.Int size'
        Nothing ->
          return $ PT.Int (dataSizeToIntSize dataSize)
    WPT.Float sizeOrNone -> do
      case sizeOrNone of
        Just size -> do
          size' <- asFloatSize dataSize size
          return $ PT.Float size'
        Nothing ->
          return $ PT.Float (dataSizeToFloatSize dataSize)

asIntSize :: DS.DataSize -> Int -> Either T.Text IntSize
asIntSize dataSize size =
  if 1 <= size && size <= DS.reify dataSize
    then return $ IntSize size
    else
      Left $
        "The size of an integer type must satisfy `1 <= SIZE <= "
          <> T.pack (show (DS.reify dataSize))
          <> "`, but found: "
          <> T.pack (show size)

asFloatSize :: DS.DataSize -> Int -> Either T.Text FloatSize
asFloatSize dataSize size =
  if size > DS.reify dataSize
    then
      Left $
        "The size of a float type must satisfy `1 <= SIZE <= "
          <> T.pack (show (DS.reify dataSize))
          <> "`, but found: "
          <> T.pack (show size)
    else case size of
      16 ->
        return FloatSize16
      32 ->
        return FloatSize32
      64 ->
        return FloatSize64
      _ ->
        Left $
          "The size of a float type must be one in [16, 32, 64], but found: "
            <> T.pack (show size)

dataSizeToIntSize :: DS.DataSize -> IntSize
dataSizeToIntSize dataSize =
  IntSize $ DS.reify dataSize

dataSizeToFloatSize :: DS.DataSize -> FloatSize
dataSizeToFloatSize dataSize =
  case dataSize of
    DS.DataSize64 ->
      FloatSize64
