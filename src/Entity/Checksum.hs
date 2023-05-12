module Entity.Checksum (hashAndEncode) where

import Crypto.Hash.SHA256 as SHA256
import Data.ByteString qualified as B
import Data.ByteString.Base64.URL qualified as Base64

hashAndEncode :: B.ByteString -> B.ByteString
hashAndEncode =
  Base64.encode . SHA256.hash
