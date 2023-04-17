module Context.Cache
  ( saveCache,
    loadCache,
    whenCompilationNecessary,
  )
where

import Context.App
import Context.App.Internal
import Context.Path qualified as Path
import Control.Monad
import Control.Monad.IO.Class
import Data.Binary
import Data.Set qualified as S
import Entity.Cache qualified as Cache
import Entity.OutputKind qualified as OK
import Entity.Source qualified as Source
import Entity.Stmt
import Path
import Path.IO

saveCache :: Source.Source -> Cache.Cache -> App ()
saveCache source cache = do
  cachePath <- Path.getSourceCachePath source
  ensureDir $ parent cachePath
  liftIO $ encodeFile (toFilePath cachePath) cache

loadCache :: Source.Source -> PathSet -> App (Maybe Cache.Cache)
loadCache source hasCacheSet = do
  cachePath <- Path.getSourceCachePath source
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

whenCompilationNecessary :: [OK.OutputKind] -> Source.Source -> App () -> App ()
whenCompilationNecessary outputKindList source comp = do
  hasLLVMSet <- readRef' hasLLVMSet
  hasObjectSet <- readRef' hasObjectSet
  unless (Source.isCompilationSkippable hasLLVMSet hasObjectSet outputKindList source) $ do
    comp
