module Context.Cache
  ( saveCache,
    loadCache,
    whenCompilationNecessary,
  )
where

import Context.App
import Context.Env qualified as Env
import Context.Path qualified as Path
import Control.Monad.IO.Class
import Data.Binary
import Entity.Artifact qualified as A
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
      artifactTime <- Env.lookupArtifactTime (Source.sourceFilePath source)
      case A.cacheTime artifactTime of
        Nothing ->
          return Nothing
        Just _ -> do
          dataOrErr <- liftIO $ decodeFileOrFail (toFilePath cachePath)
          case dataOrErr of
            Left _ -> do
              removeFile cachePath
              return Nothing
            Right content ->
              return $ Just content

whenCompilationNecessary :: [OK.OutputKind] -> Source.Source -> App a -> App (Maybe a)
whenCompilationNecessary outputKindList source comp = do
  artifactTime <- Env.lookupArtifactTime (Source.sourceFilePath source)
  if Source.isCompilationSkippable artifactTime outputKindList source
    then return Nothing
    else Just <$> comp
