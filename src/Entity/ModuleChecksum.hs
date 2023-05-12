module Entity.ModuleChecksum where

import Data.Binary
import Data.ByteString qualified as B
import Data.Hashable
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Entity.Checksum
import GHC.Generics

newtype ModuleChecksum
  = ModuleChecksum T.Text
  deriving (Show, Ord, Eq, Generic)

instance Binary ModuleChecksum

instance Hashable ModuleChecksum

reify :: ModuleChecksum -> T.Text
reify (ModuleChecksum checksum) =
  checksum

fromByteString :: B.ByteString -> ModuleChecksum
fromByteString fileByteString =
  ModuleChecksum $ TE.decodeUtf8 $ hashAndEncode fileByteString
