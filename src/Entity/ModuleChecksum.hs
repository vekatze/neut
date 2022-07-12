module Entity.ModuleChecksum where

import Crypto.Hash.SHA256 as SHA256
import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as Base64
import Data.Hashable
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
  ModuleChecksum $ TE.decodeUtf8 $ Base64.encode $ SHA256.hash fileByteString
