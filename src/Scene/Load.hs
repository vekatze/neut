module Scene.Load (load) where

import Context.App
import Context.Cache qualified as Cache
import Context.Debug (report)
import Context.Parse (readTextFile)
import Data.Text qualified as T
import Entity.Cache qualified as Cache
import Entity.Source qualified as Source
import Entity.Target
import UnliftIO (pooledForConcurrently)

load :: Target -> [Source.Source] -> App [(Source.Source, Either Cache.Cache T.Text)]
load target dependenceSeq = do
  report "Loading source files and caches"
  pooledForConcurrently dependenceSeq $ \source -> do
    cacheOrContent <- _load target source
    return (source, cacheOrContent)

_load :: Target -> Source.Source -> App (Either Cache.Cache T.Text)
_load t source = do
  mCache <- Cache.loadCache t source
  case mCache of
    Just cache -> do
      return $ Left cache
    Nothing -> do
      fmap Right $ readTextFile $ Source.sourceFilePath source
