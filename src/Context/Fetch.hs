module Context.Fetch
  ( withTempFile,
    writeModule,
    getHandleContents,
  )
where

import Context.App
import Control.Monad.IO.Class
import Data.ByteString qualified as B
import Data.Text.IO qualified as TIO
import Entity.Module qualified as M
import Path
import Path.IO (withSystemTempFile)
import System.IO

withTempFile :: (Path Abs File -> Handle -> App a) -> App a
withTempFile =
  withSystemTempFile "fetch"

writeModule :: M.Module -> App ()
writeModule targetModule =
  liftIO $ TIO.writeFile (toFilePath $ M.moduleLocation targetModule) $ M.ppModule targetModule

getHandleContents :: Handle -> App B.ByteString
getHandleContents =
  liftIO . B.hGetContents
