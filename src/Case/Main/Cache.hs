module Case.Main.Cache
  ( saveCache,
    loadCache,
    Context,
  )
where

import Control.Monad.IO.Class
import Data.Binary
import qualified Data.Set as S
import Entity.EnumInfo
import qualified Entity.Source as Source
import Entity.Stmt
import Path
import Path.IO

class (Source.Context m, MonadIO m) => Context m

saveCache :: Context m => Program -> [EnumInfo] -> m ()
saveCache (source, stmtList) enumInfoList = do
  cachePath <- Source.getSourceCachePath source
  ensureDir $ parent cachePath
  liftIO $ encodeFile (toFilePath cachePath) (stmtList, enumInfoList)

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
