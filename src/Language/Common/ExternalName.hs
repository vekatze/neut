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
  TE.encodeUtf8Builder $ "\"" <> T.concatMap escapeChar rawTxt <> "\""

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
