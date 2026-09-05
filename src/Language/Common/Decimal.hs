module Language.Common.Decimal (readDecimal) where

import Data.Char (isDigit)
import Data.Text qualified as T
import Text.Read (readMaybe)

readDecimal :: T.Text -> Maybe Integer
readDecimal text =
  case T.unpack text of
    [] ->
      Nothing
    "0" ->
      Just 0
    '0' : _ ->
      Nothing
    digits
      | all isDigit digits ->
          readMaybe digits
      | otherwise ->
          Nothing
