module Scene.Parse.Util (readIntBinaryMaybe) where

import Data.Text qualified as T

_isBinaryIntChar :: Char -> Bool
_isBinaryIntChar c =
  _isStrictBinaryIntChar c || c == '_'

_isStrictBinaryIntChar :: Char -> Bool
_isStrictBinaryIntChar c =
  c == '1' || c == '0'

readIntBinaryMaybe :: T.Text -> Maybe Integer
readIntBinaryMaybe t = do
  suffix <- T.stripPrefix "0b" t
  interpretAsInteger 1 0 suffix

interpretAsInteger :: Integer -> Integer -> T.Text -> Maybe Integer
interpretAsInteger base acc t = do
  case T.unsnoc t of
    Nothing ->
      Just acc
    Just (t', b) -> do
      case b of
        '0' ->
          interpretAsInteger (2 * base) acc t'
        '1' ->
          interpretAsInteger (2 * base) (base + acc) t'
        '_' ->
          interpretAsInteger base acc t'
        _ ->
          Nothing
