module Kernel.Common.ManageCache
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

import App.App (App)
import Control.Monad.IO.Class
import Data.Binary
import Data.Maybe
import Kernel.Common.Artifact qualified as A
import Kernel.Common.Cache qualified as Cache
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Artifact qualified as Artifact
import Kernel.Common.Handle.Global.Path qualified as Path
import Kernel.Common.OutputKind qualified as OK
import Kernel.Common.Source qualified as Source
import Kernel.Common.Target
import Path
import Path.IO

data Handle = Handle
  { pathHandle :: Path.Handle,
    artifactHandle :: Artifact.Handle
  }

new :: Global.Handle -> Handle
new (Global.Handle {..}) =
  Handle {..}

saveCache :: Path.Handle -> Target -> Source.Source -> Cache.Cache -> App ()
saveCache h t source cache = do
  cachePath <- Path.getSourceCachePath h t source
  ensureDir $ parent cachePath
  liftIO $ encodeFile (toFilePath cachePath) $ Cache.compress cache

saveCompletionCache :: Path.Handle -> Target -> Source.Source -> Cache.CompletionCache -> App ()
saveCompletionCache h t source cache = do
  cachePath <- Path.getSourceCompletionCachePath h t source
  ensureDir $ parent cachePath
  liftIO $ encodeFile (toFilePath cachePath) cache

saveLocationCache :: Path.Handle -> Target -> Source.Source -> Cache.LocationCache -> App ()
saveLocationCache h t source cache = do
  cachePath <- Path.getSourceLocationCachePath h t source
  ensureDir $ parent cachePath
  liftIO $ encodeFile (toFilePath cachePath) cache

loadCache :: Handle -> Target -> Source.Source -> App (Maybe Cache.Cache)
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

loadLocationCache :: Path.Handle -> Target -> Source.Source -> App (Maybe Cache.LocationCache)
loadLocationCache h t source = do
  cachePath <- Path.getSourceLocationCachePath h t source
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

needsCompilation :: Handle -> [OK.OutputKind] -> Source.Source -> App Bool
needsCompilation h outputKindList source = do
  artifactTime <- Artifact.lookup (artifactHandle h) (Source.sourceFilePath source)
  return $ not $ isCompilationSkippable artifactTime outputKindList

isEntryPointCompilationSkippable :: Path.Handle -> MainTarget -> [OK.OutputKind] -> App Bool
isEntryPointCompilationSkippable h target outputKindList = do
  case outputKindList of
    [] ->
      return True
    kind : rest -> do
      (_, outputPath) <- Path.getOutputPathForEntryPoint h kind target
      b <- doesFileExist outputPath
      if b
        then isEntryPointCompilationSkippable h target rest
        else return False

invalidate :: Path.Handle -> Target -> Source.Source -> App ()
invalidate h t source = do
  cachePath <- Path.getSourceCachePath h t source
  hasCache <- doesFileExist cachePath
  if not hasCache
    then return ()
    else removeFile cachePath

isCompilationSkippable ::
  A.ArtifactTime ->
  [OK.OutputKind] ->
  Bool
isCompilationSkippable artifactTime outputKindList =
  case outputKindList of
    [] ->
      True
    kind : rest -> do
      case kind of
        OK.LLVM -> do
          let b1 = isJust $ A.llvmTime artifactTime
          let b2 = isCompilationSkippable artifactTime rest
          b1 && b2
        OK.Object -> do
          let b1 = isJust $ A.objectTime artifactTime
          let b2 = isCompilationSkippable artifactTime rest
          b1 && b2
