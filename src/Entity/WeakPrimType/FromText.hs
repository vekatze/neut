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

asLowInt :: T.Text -> Maybe (Maybe Int)
asLowInt s =
  if s == intTypeName
    then Just Nothing
    else do
      case T.splitAt 3 s of
        ("", "") ->
          Nothing
        (c, rest)
          | c == intTypeName,
            Just n <- readMaybe $ T.unpack rest ->
              return $ Just n
          | otherwise ->
              Nothing

floatTypeName :: T.Text
floatTypeName = "float"

asLowFloat :: T.Text -> Maybe (Maybe Int)
asLowFloat s =
  if s == floatTypeName
    then Just Nothing
    else do
      case T.splitAt 5 s of
        ("", "") ->
          Nothing
        (c, rest)
          | c == floatTypeName,
            Just n <- readMaybe $ T.unpack rest ->
              Just $ Just n
          | otherwise ->
              Nothing
