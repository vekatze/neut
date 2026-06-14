module Language.Common.Text.Util
  ( parseBytes,
    parseText,
  )
where

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Char
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Numeric (readHex)

isUpperHexDigit :: Char -> Bool
isUpperHexDigit c =
  isDigit c || (fromIntegral (ord c - ord 'A') :: Word) <= 5

readUnicodeScalarValueMaybe :: T.Text -> Either T.Text Char
readUnicodeScalarValueMaybe t =
  case readHex (T.unpack t) of
    [(value, _)]
      | 0 <= value && value <= 0x10FFFF ->
          return $ chr value
      | otherwise ->
          Left $ "The value `" <> t <> "` is outside the Unicode codespace"
    _ ->
      Left $ "Could not parse `" <> t <> "` as a hexadecimal integer"

readChar :: Char -> T.Text -> Either T.Text T.Text
readChar c t = do
  case T.uncons t of
    Just (c', rest)
      | c == c' ->
          Right rest
      | otherwise ->
          Left $ "Expected `" <> T.singleton c <> "`, but got: `" <> T.singleton c' <> "`"
    Nothing ->
      Left $ "Expected `" <> T.singleton c <> "`, but got: end of text"

readByteEscape :: T.Text -> Either T.Text (Integer, T.Text)
readByteEscape t = do
  rest <- readChar '{' t
  let (byteText, rest') = T.span isUpperHexDigit rest
  rest'' <- readChar '}' rest'
  case readHex (T.unpack byteText) :: [(Integer, String)] of
    [(value, _)]
      | 0 <= value && value <= 0xFF ->
          return (value, rest'')
      | otherwise ->
          Left $ "The value `" <> byteText <> "` is outside the byte range"
    _ ->
      Left $ "Could not parse `" <> byteText <> "` as a hexadecimal byte"

readEscapeBytes :: T.Text -> Either T.Text (Builder.Builder, T.Text)
readEscapeBytes t = do
  case T.uncons t of
    Nothing ->
      Right (Builder.word8 0x5C, "")
    Just (c, rest) -> do
      case c of
        '0' ->
          Right (Builder.word8 0x00, rest)
        't' ->
          Right (Builder.word8 0x09, rest)
        'n' ->
          Right (Builder.word8 0x0A, rest)
        'r' ->
          Right (Builder.word8 0x0D, rest)
        '`' ->
          Right (Builder.word8 0x60, rest)
        '"' ->
          Right (Builder.word8 0x22, rest)
        '\\' ->
          Right (Builder.word8 0x5C, rest)
        'x' -> do
          (byte, rest') <- readByteEscape rest
          Right (Builder.word8 $ fromIntegral byte, rest')
        'u' -> do
          rest' <- readChar '{' rest
          let (scalarValueText, rest'') = T.span isUpperHexDigit rest'
          rest''' <- readChar '}' rest''
          ch <- readUnicodeScalarValueMaybe scalarValueText
          Right (Builder.byteString $ TE.encodeUtf8 $ T.singleton ch, rest''')
        _ ->
          Left $ "Unknown escape sequence: \\" <> T.singleton c

parseBytes' :: Builder.Builder -> T.Text -> Either T.Text Builder.Builder
parseBytes' builder t = do
  case T.uncons t of
    Nothing ->
      return builder
    Just ('\\', rest) -> do
      (escaped, rest') <- readEscapeBytes rest
      parseBytes' (builder <> escaped) rest'
    Just (c, rest) -> do
      let encoded = Builder.byteString $ TE.encodeUtf8 $ T.singleton c
      parseBytes' (builder <> encoded) rest

parseBytes :: T.Text -> Either T.Text BS.ByteString
parseBytes t = do
  builder <- parseBytes' mempty t
  return $ LBS.toStrict $ Builder.toLazyByteString builder

parseText :: T.Text -> Either T.Text T.Text
parseText t = do
  bytes <- parseBytes t
  case TE.decodeUtf8' bytes of
    Right text ->
      return text
    Left err ->
      Left $ T.pack $ show err
