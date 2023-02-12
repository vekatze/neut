module Context.Case.Main.Cache
  ( saveCache,
    loadCache,
    Context,
  )
where

import Control.Monad.IO.Class
import Data.Binary
import Data.Set qualified as S
import Entity.Source qualified as Source
import Entity.Stmt
import Path
import Path.IO

class (Source.Context m, MonadIO m) => Context m

saveCache :: Context m => Program -> m ()
saveCache (source, stmtList) = do
  cachePath <- Source.getSourceCachePath source
  ensureDir $ parent cachePath
  liftIO $ encodeFile (toFilePath cachePath) $ Cache stmtList

loadCache :: Context m => Source.Source -> PathSet -> m (Maybe Cache)
loadCache source hasCacheSet = do
  cachePath <- Source.getSourceCachePath source
  hasCache <- doesFileExist cachePath
  if not hasCache
    then return Nothing
    else do
      if S.notMember (Source.sourceFilePath source) hasCacheSet
        then return Nothing
        else do
          dataOrErr <- liftIO $ decodeFileOrFail (toFilePath cachePath)
          case dataOrErr of
            Left _ -> do
              removeFile cachePath
              return Nothing
            Right content ->
              return $ Just content
