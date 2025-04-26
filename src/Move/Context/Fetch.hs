module Move.Context.Fetch
  ( withTempFile,
    getHandleContents,
  )
where

import Data.ByteString qualified as B
import Move.Context.App
import Path
import Path.IO (withSystemTempFile)
import System.IO

withTempFile :: (Path Abs File -> Handle -> App a) -> App a
withTempFile =
  withSystemTempFile "fetch"

getHandleContents :: Handle -> IO B.ByteString
getHandleContents =
  B.hGetContents
