module Kernel.Move.Context.Cache
  ( Handle,
    new,
    saveCache,
    saveCompletionCache,
    saveLocationCache,
    loadCache,
    loadCompletionCacheOptimistically,
    loadLocationCache,
    isEntryPointCompilationSkippable,
    needsCompilation,
    invalidate,
  )
where

import Control.Monad.IO.Class
import Data.Binary
import Error.Rule.EIO (EIO)
import Kernel.Move.Context.Artifact qualified as Artifact
import Kernel.Move.Context.Path (getSourceLocationCachePath)
import Kernel.Move.Context.Path qualified as Path
import Kernel.Move.Scene.Init.Base qualified as Base
import Kernel.Common.Rule.Artifact qualified as A
import Kernel.Common.Rule.Cache qualified as Cache
import Kernel.Common.Rule.Module
import Kernel.Common.Rule.OutputKind qualified as OK
import Kernel.Common.Rule.Source qualified as Source
import Kernel.Common.Rule.Target
import Path
import Path.IO

data Handle = Handle
  { pathHandle :: Path.Handle,
    artifactHandle :: Artifact.Handle
  }

new :: Base.Handle -> Handle
new (Base.Handle {..}) =
  Handle {..}

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

loadCache :: Handle -> Target -> Source.Source -> EIO (Maybe Cache.Cache)
loadCache h t source = do
  cachePath <- Path.getSourceCachePath (pathHandle h) t source
  hasCache <- doesFileExist cachePath
  if not hasCache
    then return Nothing
    else do
      artifactTime <- Artifact.lookup (artifactHandle h) (Source.sourceFilePath source)
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

loadCompletionCacheOptimistically :: Path Abs File -> EIO (Maybe Cache.CompletionCache)
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

loadLocationCache :: Path.Handle -> Target -> Source.Source -> EIO (Maybe Cache.LocationCache)
loadLocationCache h t source = do
  cachePath <- getSourceLocationCachePath h t source
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

needsCompilation :: Handle -> [OK.OutputKind] -> Source.Source -> EIO Bool
needsCompilation h outputKindList source = do
  artifactTime <- Artifact.lookup (artifactHandle h) (Source.sourceFilePath source)
  return $ not $ Source.isCompilationSkippable artifactTime outputKindList

isEntryPointCompilationSkippable :: Path.Handle -> MainModule -> MainTarget -> [OK.OutputKind] -> EIO Bool
isEntryPointCompilationSkippable h mainModule target outputKindList = do
  case outputKindList of
    [] ->
      return True
    kind : rest -> do
      (_, outputPath) <- Path.getOutputPathForEntryPoint h (extractModule mainModule) kind target
      b <- doesFileExist outputPath
      if b
        then isEntryPointCompilationSkippable h mainModule target rest
        else return False

invalidate :: Path.Handle -> Target -> Source.Source -> EIO ()
invalidate h t source = do
  cachePath <- Path.getSourceCachePath h t source
  hasCache <- doesFileExist cachePath
  if not hasCache
    then return ()
    else removeFile cachePath
