module Language.Common.ExternalName
  ( ExternalName (..),
    malloc,
    realloc,
    free,
    memcpy,
    toBuilder,
  )
where

import Data.Binary
import Data.ByteString.Builder
import Data.Hashable
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics

newtype ExternalName = ExternalName {reify :: T.Text}
  deriving (Generic, Show, Eq, Ord)

instance Binary ExternalName

instance Hashable ExternalName

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
  TE.encodeUtf8Builder rawTxt
