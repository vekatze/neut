module Language.Common.PrimType.FromText (fromDefiniteDescription, fromText) where

import Data.Text qualified as T
import Language.Common.DataSize qualified as DS
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.PrimNumSize
import Language.Common.PrimType qualified as PT
import Text.Read

fromDefiniteDescription :: DS.DataSize -> DD.DefiniteDescription -> Maybe PT.PrimType
fromDefiniteDescription dataSize dd = do
  let sgl = DD.globalLocator dd
  let ll = DD.localLocator dd
  if DD.llvmGlobalLocator /= sgl
    then Nothing
    else fromText dataSize ll

fromText :: DS.DataSize -> T.Text -> Maybe PT.PrimType
fromText dataSize name
  | Just intSize <- asLowInt dataSize name =
      Just $ PT.Int intSize
  | Just floatSize <- asLowFloat dataSize name =
      Just $ PT.Float floatSize
  | otherwise =
      Nothing

intTypeName :: T.Text
intTypeName = "int"

asLowInt :: DS.DataSize -> T.Text -> Maybe IntSize
asLowInt dataSize s =
  if s == intTypeName
    then Just $ dataSizeToIntSize dataSize
    else do
      case T.splitAt 3 s of
        ("", "") ->
          Nothing
        (c, rest)
          | c == intTypeName,
            Just n <- readMaybe $ T.unpack rest,
            Just size <- intToIntSize dataSize n ->
              Just size
          | otherwise ->
              Nothing

floatTypeName :: T.Text
floatTypeName = "float"

asLowFloat :: DS.DataSize -> T.Text -> Maybe FloatSize
asLowFloat dataSize s =
  if s == floatTypeName
    then Just $ dataSizeToFloatSize dataSize
    else do
      case T.splitAt 5 s of
        ("", "") ->
          Nothing
        (c, rest)
          | c == floatTypeName,
            Just n <- readMaybe $ T.unpack rest,
            Just size <- intToFloatSize dataSize n ->
              Just size
          | otherwise ->
              Nothing
