module Rule.Digest (hashAndEncode) where

import Crypto.Hash.SHA256 as SHA256
import Data.ByteString qualified as B
import Data.ByteString.Base64.URL qualified as Base64

hashAndEncode :: B.ByteString -> B.ByteString
hashAndEncode =
  Base64.encodeUnpadded . SHA256.hash
