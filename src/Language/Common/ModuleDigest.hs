module Language.Common.ModuleDigest
  ( ModuleDigest (..),
    reify,
    fromByteString,
  )
where

import Binary.Derive (deriveBinary)
import Data.ByteString qualified as B
import Data.Hashable
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics
import Language.Common.Digest

newtype ModuleDigest
  = ModuleDigest T.Text
  deriving (Show, Ord, Eq, Generic)

instance Hashable ModuleDigest

reify :: ModuleDigest -> T.Text
reify (ModuleDigest digest) =
  digest

fromByteString :: B.ByteString -> ModuleDigest
fromByteString fileByteString =
  ModuleDigest $ TE.decodeUtf8 $ hashAndEncode fileByteString

deriveBinary [''ModuleDigest]
