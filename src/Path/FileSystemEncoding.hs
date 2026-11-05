module Path.FileSystemEncoding
  ( encodeFileSystemPath,
    decodeFileSystemPath,
  )
where

import Data.ByteString qualified as B
import GHC.Foreign qualified as GF
import GHC.IO.Encoding (getFileSystemEncoding)

encodeFileSystemPath :: FilePath -> IO B.ByteString
encodeFileSystemPath path = do
  encoding <- getFileSystemEncoding
  GF.withCStringLen encoding path B.packCStringLen

decodeFileSystemPath :: B.ByteString -> IO FilePath
decodeFileSystemPath bytes = do
  encoding <- getFileSystemEncoding
  B.useAsCStringLen bytes $ GF.peekCStringLen encoding
