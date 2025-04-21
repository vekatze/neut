module Move.Context.Cache
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

import Control.Monad.IO.Class
import Data.Binary
import Move.Context.App
import Move.Context.EIO (EIO, toApp)
import Move.Context.Env qualified as Env
import Move.Context.Path (getSourceLocationCachePath)
import Move.Context.Path qualified as Path
import Path
import Path.IO
import Rule.Artifact qualified as A
import Rule.Cache qualified as Cache
import Rule.Module
import Rule.OutputKind qualified as OK
import Rule.Source qualified as Source
import Rule.Target

saveCache :: Path.Handle -> Target -> Source.Source -> Cache.Cache -> EIO ()
saveCache h t source cache = do
  cachePath <- Path.getSourceCachePath h t source
  ensureDir $ parent cachePath
  liftIO $ encodeFile (toFilePath cachePath) $ Cache.compress cache

saveCompletionCache :: Path.Handle -> Target -> Source.Source -> Cache.CompletionCache -> EIO ()
saveCompletionCache h t source cache = do
  cachePath <- Path.getSourceCompletionCachePath h t source
  ensureDir $ parent cachePath
  liftIO $ encodeFile (toFilePath cachePath) cache

saveLocationCache :: Path.Handle -> Target -> Source.Source -> Cache.LocationCache -> EIO ()
saveLocationCache h t source cache = do
  cachePath <- Path.getSourceLocationCachePath h t source
  ensureDir $ parent cachePath
  liftIO $ encodeFile (toFilePath cachePath) cache

loadCache :: Target -> Source.Source -> App (Maybe Cache.Cache)
loadCache t source = do
  h <- Path.new
  cachePath <- toApp $ Path.getSourceCachePath h t source
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
  h <- Path.new
  cachePath <- toApp $ getSourceLocationCachePath h t source
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

isEntryPointCompilationSkippable :: MainModule -> MainTarget -> [OK.OutputKind] -> App Bool
isEntryPointCompilationSkippable mainModule target outputKindList = do
  case outputKindList of
    [] ->
      return True
    kind : rest -> do
      h <- Path.new
      (_, outputPath) <- toApp $ Path.getOutputPathForEntryPoint h (extractModule mainModule) kind target
      b <- doesFileExist outputPath
      if b
        then isEntryPointCompilationSkippable mainModule target rest
        else return False

invalidate :: Target -> Source.Source -> App ()
invalidate t source = do
  h <- Path.new
  cachePath <- toApp $ Path.getSourceCachePath h t source
  hasCache <- doesFileExist cachePath
  if not hasCache
    then return ()
    else removeFile cachePath
