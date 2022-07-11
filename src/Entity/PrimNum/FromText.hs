module Entity.PrimNum.FromText (fromDefiniteDescription, fromText) where

import qualified Data.Text as T
import qualified Entity.BaseName as BN
import qualified Entity.DefiniteDescription as DD
import qualified Entity.LocalLocator as LL
import Entity.PrimNum
import Entity.PrimNumSize
import qualified Entity.StrictGlobalLocator as SGL
import Text.Read

fromDefiniteDescription :: DD.DefiniteDescription -> Maybe PrimNum
fromDefiniteDescription dd = do
  let sgl = DD.globalLocator dd
  let ll = DD.localLocator dd
  if SGL.llvmGlobalLocator /= sgl || not (null (LL.sectionStack ll))
    then Nothing
    else fromText $ BN.reify $ LL.baseName ll

fromText :: T.Text -> Maybe PrimNum
fromText name
  | Just intSize <- asLowInt name =
    Just $ PrimNumInt intSize
  | Just floatSize <- asLowFloat name =
    Just $ PrimNumFloat floatSize
  | otherwise =
    Nothing

asLowInt :: T.Text -> Maybe IntSize
asLowInt s =
  case T.uncons s of
    Nothing ->
      Nothing
    Just (c, rest) ->
      case c of
        'i'
          | Just n <- readMaybe $ T.unpack rest,
            Just size <- asIntSize n ->
            Just size
        _ ->
          Nothing

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
