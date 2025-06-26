module Language.Common.Rule.BasePrimType.FromText (fromText) where

import Data.Text qualified as T
import Language.Common.Rule.BasePrimType qualified as BPT
import Language.Common.Rule.DataSize qualified as DS
import Language.Common.Rule.PrimNumSize
import Text.Read

fromText :: DS.DataSize -> T.Text -> Maybe BPT.BasePrimType
fromText dataSize name
  | Just intSize <- asLowInt dataSize name =
      Just $ BPT.Int intSize
  | Just floatSize <- asLowFloat dataSize name =
      Just $ BPT.Float floatSize
  | otherwise =
      Nothing

intTypeName :: T.Text
intTypeName = "int"

floatTypeName :: T.Text
floatTypeName = "float"

asLowInt :: DS.DataSize -> T.Text -> Maybe (BPT.BasePrimTypeSize IntSize)
asLowInt dataSize s = do
  rest <- T.stripPrefix intTypeName s
  if T.null rest
    then return $ BPT.Implicit $ dataSizeToIntSize dataSize
    else do
      size <- readMaybe $ T.unpack rest
      BPT.Explicit <$> intToIntSize dataSize size

asLowFloat :: DS.DataSize -> T.Text -> Maybe (BPT.BasePrimTypeSize FloatSize)
asLowFloat dataSize s = do
  rest <- T.stripPrefix floatTypeName s
  if T.null rest
    then return $ BPT.Implicit $ dataSizeToFloatSize dataSize
    else do
      size <- readMaybe $ T.unpack rest
      BPT.Explicit <$> intToFloatSize dataSize size
