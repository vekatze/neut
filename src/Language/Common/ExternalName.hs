module Language.Common.ExternalName
  ( ExternalName (..),
    calloc,
    malloc,
    realloc,
    free,
    memcpy,
    toBuilder,
  )
where

import Data.Binary
import Data.ByteString.Builder
import Data.Char (isControl, ord)
import Data.Hashable
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics
import Numeric (showHex)

newtype ExternalName = ExternalName {reify :: T.Text}
  deriving (Generic, Show, Eq, Ord)

instance Binary ExternalName

instance Hashable ExternalName

calloc :: ExternalName
calloc =
  ExternalName "calloc"

malloc :: ExternalName
malloc =
  ExternalName "malloc"

realloc :: ExternalName
realloc =
  ExternalName "realloc"

free :: ExternalName
free =
  ExternalName "free"

memcpy :: ExternalName
memcpy =
  ExternalName "llvm.memcpy.p0.p0.i64"

toBuilder :: ExternalName -> Builder
toBuilder (ExternalName rawTxt) =
  TE.encodeUtf8Builder $
    if isPlainIdentifier rawTxt
      then rawTxt
      else "\"" <> T.concatMap escapeChar rawTxt <> "\""

-- an LLVM identifier that needs no quotation: [-a-zA-Z$._][-a-zA-Z$._0-9]*
isPlainIdentifier :: T.Text -> Bool
isPlainIdentifier text =
  case T.uncons text of
    Nothing ->
      False
    Just (c, rest) ->
      isIdentifierHead c && T.all isIdentifierTail rest

isIdentifierHead :: Char -> Bool
isIdentifierHead c =
  isAsciiLetter c || c `elem` ("-$._" :: String)

isIdentifierTail :: Char -> Bool
isIdentifierTail c =
  isIdentifierHead c || isAsciiDigit c

isAsciiLetter :: Char -> Bool
isAsciiLetter c =
  ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

isAsciiDigit :: Char -> Bool
isAsciiDigit c =
  '0' <= c && c <= '9'

escapeChar :: Char -> T.Text
escapeChar c
  | c == '"' || c == '\\' || isControl c =
      T.pack $ '\\' : pad (map toUpperHex (showHex (ord c) ""))
  | otherwise =
      T.singleton c

pad :: String -> String
pad hex =
  replicate (2 - length hex) '0' ++ hex

toUpperHex :: Char -> Char
toUpperHex c =
  case c of
    'a' -> 'A'
    'b' -> 'B'
    'c' -> 'C'
    'd' -> 'D'
    'e' -> 'E'
    'f' -> 'F'
    _ -> c
