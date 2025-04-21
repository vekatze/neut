module Move.Context.Fetch
  ( withTempFile,
    getHandleContents,
  )
where

import Move.Context.App
import Control.Monad.IO.Class
import Data.ByteString qualified as B
import Path
import Path.IO (withSystemTempFile)
import System.IO

withTempFile :: (Path Abs File -> Handle -> App a) -> App a
withTempFile =
  withSystemTempFile "fetch"

getHandleContents :: Handle -> App B.ByteString
getHandleContents =
  liftIO . B.hGetContents
