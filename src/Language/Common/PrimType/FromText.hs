module Language.Common.PrimType.FromText (fromDefiniteDescription, fromText) where

import Data.Text qualified as T
import Language.Common.Decimal (readDecimal)
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.PrimNumSize
import Language.Common.PrimType qualified as PT
import Language.Common.SlotSize

fromDefiniteDescription :: DD.DefiniteDescription -> Maybe PT.PrimType
fromDefiniteDescription dd = do
  let sgl = DD.globalLocator dd
  let ll = DD.localLocator dd
  if DD.llvmGlobalLocator /= sgl
    then Nothing
    else fromText ll

fromText :: T.Text -> Maybe PT.PrimType
fromText name
  | Just intSize <- asLowInt name =
      Just $ PT.Int intSize
  | Just floatSize <- asLowFloat name =
      Just $ PT.Float floatSize
  | name == textTypeName =
      Just PT.Text
  | name == blobTypeName =
      Just PT.Blob
  | name == runeTypeName =
      Just PT.Rune
  | otherwise =
      Nothing

asLowInt :: T.Text -> Maybe IntSize
asLowInt s =
  if s == intTypeName
    then Just slotIntSize
    else do
      case T.splitAt 3 s of
        ("", "") ->
          Nothing
        (c, rest)
          | c == intTypeName,
            Just n <- readDecimal rest,
            Just size <- intToIntSize n ->
              Just size
          | otherwise ->
              Nothing

floatTypeName :: T.Text
floatTypeName = "float"

asLowFloat :: T.Text -> Maybe FloatSize
asLowFloat s =
  if s == floatTypeName
    then Just slotFloatSize
    else do
      case T.splitAt 5 s of
        ("", "") ->
          Nothing
        (c, rest)
          | c == floatTypeName,
            Just n <- readDecimal rest,
            Just size <- intToFloatSize n ->
              Just size
          | otherwise ->
              Nothing

intTypeName :: T.Text
intTypeName = "int"

textTypeName :: T.Text
textTypeName = "text"

blobTypeName :: T.Text
blobTypeName = "blob"

runeTypeName :: T.Text
runeTypeName = "rune"
