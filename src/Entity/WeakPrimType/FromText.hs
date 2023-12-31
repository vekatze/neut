module Entity.WeakPrimType.FromText (fromText) where

import Data.Text qualified as T
import Entity.WeakPrimType qualified as PT
import Text.Read

fromText :: T.Text -> Maybe PT.WeakPrimType
fromText name
  | Just intSize <- asLowInt name =
      Just $ PT.Int intSize
  | Just floatSize <- asLowFloat name =
      Just $ PT.Float floatSize
  | otherwise =
      Nothing

intTypeName :: T.Text
intTypeName = "int"

floatTypeName :: T.Text
floatTypeName = "float"

asLowInt :: T.Text -> Maybe (Maybe Int)
asLowInt s = do
  rest <- T.stripPrefix intTypeName s
  if T.null rest
    then return Nothing
    else do
      n <- readMaybe $ T.unpack rest
      return $ Just n

asLowFloat :: T.Text -> Maybe (Maybe Int)
asLowFloat s = do
  rest <- T.stripPrefix floatTypeName s
  if T.null rest
    then return Nothing
    else do
      n <- readMaybe $ T.unpack rest
      return $ Just n
