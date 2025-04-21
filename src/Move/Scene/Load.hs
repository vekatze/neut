module Move.Scene.Load (load) where

import Data.Text qualified as T
import Move.Context.App
import Move.Context.Cache qualified as Cache
import Move.Context.Debug (report)
import Move.Context.Parse (readTextFile)
import Rule.Cache qualified as Cache
import Rule.Source qualified as Source
import Rule.Target
import UnliftIO (MonadIO (liftIO), pooledForConcurrently)

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
      fmap Right $ liftIO . readTextFile $ Source.sourceFilePath source
