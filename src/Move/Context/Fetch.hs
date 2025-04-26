module Move.Context.Fetch
  ( getHandleContents,
  )
where

import Data.ByteString qualified as B
import System.IO

getHandleContents :: Handle -> IO B.ByteString
getHandleContents =
  B.hGetContents
