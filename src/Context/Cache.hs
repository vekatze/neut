module Context.Cache
  ( saveCache,
    saveCompletionCache,
    saveLocationCache,
    loadCache,
    loadCacheOptimistically,
    loadCompletionCacheOptimistically,
    loadLocationCache,
    whenCompilationNecessary,
    isEntryPointCompilationSkippable,
    needsCompilation,
    invalidate,
  )
where

import Context.App
import Context.Env qualified as Env
import Context.Path (getSourceLocationCachePath)
import Context.Path qualified as Path
import Control.Monad.IO.Class
import Data.Binary
import Entity.Artifact qualified as A
import Entity.Cache qualified as Cache
import Entity.Module
import Entity.OutputKind qualified as OK
import Entity.Source qualified as Source
import Entity.Target
import Path
import Path.IO

saveCache :: Target -> Source.Source -> Cache.Cache -> App ()
saveCache t source cache = do
  cachePath <- Path.getSourceCachePath t source
  ensureDir $ parent cachePath
  liftIO $ encodeFile (toFilePath cachePath) $ Cache.compress cache

saveCompletionCache :: Target -> Source.Source -> Cache.CompletionCache -> App ()
saveCompletionCache t source cache = do
  cachePath <- Path.getSourceCompletionCachePath t source
  ensureDir $ parent cachePath
  liftIO $ encodeFile (toFilePath cachePath) cache

saveLocationCache :: Target -> Source.Source -> Cache.LocationCache -> App ()
saveLocationCache t source cache = do
  cachePath <- Path.getSourceLocationCachePath t source
  ensureDir $ parent cachePath
  liftIO $ encodeFile (toFilePath cachePath) cache

loadCache :: Target -> Source.Source -> App (Maybe Cache.Cache)
loadCache t source = do
  cachePath <- Path.getSourceCachePath t source
  hasCache <- doesFileExist cachePath
  if not hasCache
    then return Nothing
    else do
      artifactTime <- Env.lookupArtifactTime (Source.sourceFilePath source)
      case A.cacheTime artifactTime of
        Nothing ->
          return Nothing
        _ -> do
          dataOrErr <- liftIO $ decodeFileOrFail (toFilePath cachePath)
          case dataOrErr of
            Left _ -> do
              removeFile cachePath
              return Nothing
            Right content ->
              return $ Just $ Cache.extend content

loadCacheOptimistically :: Path Abs File -> App (Maybe Cache.Cache)
loadCacheOptimistically cachePath = do
  hasCache <- doesFileExist cachePath
  if not hasCache
    then return Nothing
    else do
      dataOrErr <- liftIO $ decodeFileOrFail (toFilePath cachePath)
      case dataOrErr of
        Left _ -> do
          removeFile cachePath
          return Nothing
        Right content ->
          return $ Just $ Cache.extend content

loadCompletionCacheOptimistically :: Path Abs File -> App (Maybe Cache.CompletionCache)
loadCompletionCacheOptimistically cachePath = do
  hasCache <- doesFileExist cachePath
  if not hasCache
    then return Nothing
    else do
      dataOrErr <- liftIO $ decodeFileOrFail (toFilePath cachePath)
      case dataOrErr of
        Left _ -> do
          removeFile cachePath
          return Nothing
        Right content ->
          return $ Just content

loadLocationCache :: Target -> Source.Source -> App (Maybe Cache.LocationCache)
loadLocationCache t source = do
  cachePath <- getSourceLocationCachePath t source
  hasCache <- doesFileExist cachePath
  if not hasCache
    then return Nothing
    else do
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
  if Source.isCompilationSkippable artifactTime outputKindList
    then return Nothing
    else Just <$> comp

needsCompilation :: [OK.OutputKind] -> Source.Source -> App Bool
needsCompilation outputKindList source = do
  artifactTime <- Env.lookupArtifactTime (Source.sourceFilePath source)
  return $ not $ Source.isCompilationSkippable artifactTime outputKindList

isEntryPointCompilationSkippable :: Module -> MainTarget -> [OK.OutputKind] -> App Bool
isEntryPointCompilationSkippable baseModule target outputKindList = do
  case outputKindList of
    [] ->
      return True
    kind : rest -> do
      (_, outputPath) <- Path.getOutputPathForEntryPoint baseModule kind target
      b <- Path.doesFileExist outputPath
      if b
        then isEntryPointCompilationSkippable baseModule target rest
        else return False

invalidate :: Target -> Source.Source -> App ()
invalidate t source = do
  cachePath <- Path.getSourceCachePath t source
  hasCache <- doesFileExist cachePath
  if not hasCache
    then return ()
    else removeFile cachePath
