module Context.Path where

-- import qualified Context.Throw as Throw
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Time
import Path

class Monad m => Context m where
  getLibraryDirPath :: m (Path Abs Dir)
  getCurrentDir :: m (Path Abs Dir)
  ensureNotInLibDir :: m ()
  resolveDir :: Path Abs Dir -> FilePath -> m (Path Abs Dir)
  resolveFile :: Path Abs Dir -> FilePath -> m (Path Abs File)
  doesDirExist :: Path Abs Dir -> m Bool
  doesFileExist :: Path Abs File -> m Bool
  getModificationTime :: Path Abs File -> m UTCTime
  ensureDir :: Path Abs Dir -> m ()
  stripPrefix :: Path b Dir -> Path b t -> m (Path Rel t)
  writeByteString :: Path Abs File -> L.ByteString -> m ()
  writeText :: Path Abs File -> T.Text -> m ()
  parseRelFile :: FilePath -> m (Path Rel File)
  removeDirRecur :: Path Abs Dir -> m ()

-- newtype Config = Config
--   { throwCtx :: Throw.Context
--   }
