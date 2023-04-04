module Entity.PrimType.FromText (fromDefiniteDescription, fromText) where

import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.DefiniteDescription qualified as DD
import Entity.LocalLocator qualified as LL
import Entity.PrimNumSize
import Entity.PrimType qualified as PT
import Entity.StrictGlobalLocator qualified as SGL
import Text.Read

fromDefiniteDescription :: DD.DefiniteDescription -> Maybe PT.PrimType
fromDefiniteDescription dd = do
  let sgl = DD.globalLocator dd
  let ll = DD.localLocator dd
  if SGL.llvmGlobalLocator /= sgl || not (null (LL.sectionStack ll))
    then Nothing
    else fromText $ BN.reify $ LL.baseName ll

fromText :: T.Text -> Maybe PT.PrimType
fromText name
  | Just intSize <- asLowSignedInt name =
      Just $ PT.Int intSize
  | Just intSize <- asLowUnsignedInt name =
      Just $ PT.UInt intSize
  | Just floatSize <- asLowFloat name =
      Just $ PT.Float floatSize
  | otherwise =
      Nothing

asLowInt :: Char -> T.Text -> Maybe IntSize
asLowInt headChar s =
  case T.uncons s of
    Nothing ->
      Nothing
    Just (c, rest)
      | c == headChar,
        Just n <- readMaybe $ T.unpack rest,
        Just size <- asIntSize n ->
          Just size
    _ ->
      Nothing

asLowSignedInt :: T.Text -> Maybe IntSize
asLowSignedInt =
  asLowInt 'i'

asLowUnsignedInt :: T.Text -> Maybe IntSize
asLowUnsignedInt =
  asLowInt 'u'

asLowFloat :: T.Text -> Maybe FloatSize
asLowFloat s =
  case T.uncons s of
    Nothing ->
      Nothing
    Just (c, rest) ->
      case c of
        'f'
          | Just n <- readMaybe $ T.unpack rest,
            Just size <- asFloatSize n ->
              Just size
        _ ->
          Nothing

asIntSize :: Int -> Maybe IntSize
asIntSize size =
  if 1 <= size && size <= 64
    then Just $ IntSize size
    else Nothing

asFloatSize :: Int -> Maybe FloatSize
asFloatSize size =
  case size of
    16 ->
      Just FloatSize16
    32 ->
      Just FloatSize32
    64 ->
      Just FloatSize64
    _ ->
      Nothing
