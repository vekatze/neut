module Language.Common.Text.Util
  ( decodeUtf8Bytes,
    parseBytes,
    parseText,
  )
where

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Char
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import Numeric (readHex, showHex)

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

isContinuationByte :: Word8 -> Bool
isContinuationByte w =
  0x80 <= w && w <= 0xBF

formatByte :: Word8 -> T.Text
formatByte w = do
  let hex = T.pack $ map toUpper $ showHexByte w
  "0x" <> T.replicate (2 - T.length hex) "0" <> hex

showHexByte :: Word8 -> String
showHexByte w =
  case showHex (fromIntegral w :: Int) "" of
    [] ->
      "0"
    hex ->
      hex

readContinuationByte :: Int -> [Word8] -> Either T.Text (Word8, [Word8])
readContinuationByte offset ws = do
  case ws of
    [] ->
      Left $ "Invalid UTF-8 byte sequence at byte offset " <> T.pack (show offset) <> ": unexpected end of string literal"
    w : rest
      | isContinuationByte w ->
          return (w, rest)
      | otherwise ->
          Left $
            "Invalid UTF-8 byte sequence at byte offset "
              <> T.pack (show offset)
              <> ": expected a continuation byte, but got "
              <> formatByte w

readBoundedContinuationByte :: Int -> Word8 -> Word8 -> [Word8] -> Either T.Text [Word8]
readBoundedContinuationByte offset lower upper ws = do
  (w, rest) <- readContinuationByte offset ws
  if lower <= w && w <= upper
    then return rest
    else
      Left $
        "Invalid UTF-8 byte sequence at byte offset "
          <> T.pack (show offset)
          <> ": expected a byte in "
          <> formatByte lower
          <> ".."
          <> formatByte upper
          <> ", but got "
          <> formatByte w

consumeContinuationBytes :: Int -> Int -> [Word8] -> Either T.Text [Word8]
consumeContinuationBytes offset count ws = do
  if count <= 0
    then return ws
    else do
      (_, rest) <- readContinuationByte offset ws
      consumeContinuationBytes (offset + 1) (count - 1) rest

validateUtf8Bytes' :: Int -> [Word8] -> Either T.Text ()
validateUtf8Bytes' offset ws = do
  case ws of
    [] ->
      return ()
    w : rest
      | w <= 0x7F ->
          validateUtf8Bytes' (offset + 1) rest
      | 0xC2 <= w && w <= 0xDF -> do
          rest' <- consumeContinuationBytes (offset + 1) 1 rest
          validateUtf8Bytes' (offset + 2) rest'
      | w == 0xE0 -> do
          rest' <- readBoundedContinuationByte (offset + 1) 0xA0 0xBF rest
          rest'' <- consumeContinuationBytes (offset + 2) 1 rest'
          validateUtf8Bytes' (offset + 3) rest''
      | 0xE1 <= w && w <= 0xEC -> do
          rest' <- consumeContinuationBytes (offset + 1) 2 rest
          validateUtf8Bytes' (offset + 3) rest'
      | w == 0xED -> do
          rest' <- readBoundedContinuationByte (offset + 1) 0x80 0x9F rest
          rest'' <- consumeContinuationBytes (offset + 2) 1 rest'
          validateUtf8Bytes' (offset + 3) rest''
      | 0xEE <= w && w <= 0xEF -> do
          rest' <- consumeContinuationBytes (offset + 1) 2 rest
          validateUtf8Bytes' (offset + 3) rest'
      | w == 0xF0 -> do
          rest' <- readBoundedContinuationByte (offset + 1) 0x90 0xBF rest
          rest'' <- consumeContinuationBytes (offset + 2) 2 rest'
          validateUtf8Bytes' (offset + 4) rest''
      | 0xF1 <= w && w <= 0xF3 -> do
          rest' <- consumeContinuationBytes (offset + 1) 3 rest
          validateUtf8Bytes' (offset + 4) rest'
      | w == 0xF4 -> do
          rest' <- readBoundedContinuationByte (offset + 1) 0x80 0x8F rest
          rest'' <- consumeContinuationBytes (offset + 2) 2 rest'
          validateUtf8Bytes' (offset + 4) rest''
      | otherwise ->
          Left $
            "Invalid UTF-8 byte sequence at byte offset "
              <> T.pack (show offset)
              <> ": invalid leading byte "
              <> formatByte w

decodeUtf8Bytes :: BS.ByteString -> Either T.Text T.Text
decodeUtf8Bytes bytes = do
  validateUtf8Bytes' 0 $ BS.unpack bytes
  return $ TE.decodeUtf8 bytes

parseText :: T.Text -> Either T.Text T.Text
parseText t = do
  bytes <- parseBytes t
  decodeUtf8Bytes bytes
