module Entity.PrimType.FromText (fromDefiniteDescription, fromText) where

import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.DataSize qualified as DS
import Entity.DefiniteDescription qualified as DD
import Entity.LocalLocator qualified as LL
import Entity.PrimNumSize
import Entity.PrimType qualified as PT
import Entity.StrictGlobalLocator qualified as SGL
import Text.Read

fromDefiniteDescription :: DS.DataSize -> DD.DefiniteDescription -> Maybe PT.PrimType
fromDefiniteDescription dataSize dd = do
  let sgl = DD.globalLocator dd
  let ll = DD.localLocator dd
  if SGL.llvmGlobalLocator /= sgl
    then Nothing
    else fromText dataSize $ BN.reify $ LL.baseName ll

fromText :: DS.DataSize -> T.Text -> Maybe PT.PrimType
fromText dataSize name
  | Just intSize <- asLowInt dataSize name =
      Just $ PT.Int intSize
  | Just floatSize <- asLowFloat dataSize name =
      Just $ PT.Float floatSize
  | otherwise =
      Nothing

asLowInt :: DS.DataSize -> T.Text -> Maybe IntSize
asLowInt dataSize s =
  case T.uncons s of
    Nothing ->
      Nothing
    Just (c, rest)
      | c == 'i',
        Just n <- readMaybe $ T.unpack rest,
        Just size <- asIntSize dataSize n ->
          Just size
    _ ->
      Nothing

asLowFloat :: DS.DataSize -> T.Text -> Maybe FloatSize
asLowFloat dataSize s =
  case T.uncons s of
    Nothing ->
      Nothing
    Just (c, rest) ->
      case c of
        'f'
          | Just n <- readMaybe $ T.unpack rest,
            Just size <- asFloatSize dataSize n ->
              Just size
        _ ->
          Nothing

asIntSize :: DS.DataSize -> Int -> Maybe IntSize
asIntSize dataSize size =
  if 1 <= size && size <= DS.reify dataSize
    then Just $ IntSize size
    else Nothing

asFloatSize :: DS.DataSize -> Int -> Maybe FloatSize
asFloatSize dataSize size =
  if size > DS.reify dataSize
    then Nothing
    else case size of
      16 ->
        Just FloatSize16
      32 ->
        Just FloatSize32
      64 ->
        Just FloatSize64
      _ ->
        Nothing
