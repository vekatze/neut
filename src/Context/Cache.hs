module Context.Cache
  ( saveCache,
    loadCache,
    whenCompilationNecessary,
  )
where

import Context.App
import Context.Env qualified as Env
import Context.Path qualified as Path
import Control.Monad
import Control.Monad.IO.Class
import Data.Binary
import Data.HashMap.Strict qualified as Map
import Entity.Cache qualified as Cache
import Entity.OutputKind qualified as OK
import Entity.Source qualified as Source
import Path
import Path.IO

saveCache :: Source.Source -> Cache.Cache -> App ()
saveCache source cache = do
  cachePath <- Path.getSourceCachePath source
  ensureDir $ parent cachePath
  liftIO $ encodeFile (toFilePath cachePath) cache

loadCache :: Source.Source -> App (Maybe Cache.Cache)
loadCache source = do
  cachePath <- Path.getSourceCachePath source
  hasCache <- doesFileExist cachePath
  if not hasCache
    then return Nothing
    else do
      cacheTimeMap <- Env.getCacheTimeMap
      if not $ Map.member (Source.sourceFilePath source) cacheTimeMap
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
  llvmTimeMap <- Env.getLLVMTimeMap
  objectTimeMap <- Env.getObjectTimeMap
  unless (Source.isCompilationSkippable llvmTimeMap objectTimeMap outputKindList source) $ do
    comp
