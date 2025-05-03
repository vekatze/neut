module Main.Rule.Text.Util (parseText) where

import Data.Char
import Data.List qualified as List
import Data.Text qualified as T
import Numeric (readHex)

isLowerHexDigit :: Char -> Bool
isLowerHexDigit c =
  isDigit c || (fromIntegral (ord c - ord 'a') :: Word) <= 5

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

unquoteText :: T.Text -> Either T.Text [T.Text]
unquoteText t = do
  case T.uncons t of
    Nothing ->
      Right ["\\"]
    Just (c, rest) -> do
      case c of
        '0' ->
          Right ["\0", rest]
        't' ->
          Right ["\t", rest]
        'n' ->
          Right ["\n", rest]
        'r' ->
          Right ["\r", rest]
        '`' ->
          Right ["`", rest]
        '"' ->
          Right ["\"", rest]
        'u' -> do
          rest' <- readChar '{' rest
          let (scalarValueText, rest'') = T.span isLowerHexDigit rest'
          rest''' <- readChar '}' rest''
          ch <- readUnicodeScalarValueMaybe scalarValueText
          Right [T.singleton ch, rest''']
        _ ->
          Left $ "Unknown escape sequence: \\" <> T.singleton c

parseTextFragment :: T.Text -> Either T.Text [T.Text]
parseTextFragment t = do
  case T.splitOn "\\" t of
    [] ->
      -- unreachable
      Left "Got an empty list from `Data.Text.splitOn`"
    t' : rest -> do
      fragments <- concat <$> mapM unquoteText rest
      Right $ t' : fragments

parseText :: T.Text -> Either T.Text T.Text
parseText t = do
  let ts = T.splitOn "\\\\" t
  ts' <- mapM parseTextFragment ts
  return $ T.concat $ List.intercalate ["\\"] ts'
