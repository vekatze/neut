module Main.Move.Scene.Load
  ( Handle,
    load,
    new,
  )
where

import Data.Text qualified as T
import Logger.Move.Debug qualified as Logger
import Logger.Rule.Handle qualified as Logger
import Main.Move.Context.Cache qualified as Cache
import Main.Move.Context.EIO (EIO, forP)
import Main.Move.Context.Parse (readTextFile)
import Main.Move.Scene.Init.Base qualified as Base
import Main.Rule.Cache qualified as Cache
import Main.Rule.Source qualified as Source
import Main.Rule.Target
import UnliftIO (MonadIO (liftIO))

data Handle
  = Handle
  { loggerHandle :: Logger.Handle,
    cacheHandle :: Cache.Handle
  }

new :: Base.Handle -> Handle
new baseHandle@(Base.Handle {..}) = do
  let cacheHandle = Cache.new baseHandle
  Handle {..}

load :: Handle -> Target -> [Source.Source] -> EIO [(Source.Source, Either Cache.Cache T.Text)]
load h target dependenceSeq = do
  liftIO $ Logger.report (loggerHandle h) "Loading source files and caches"
  forP dependenceSeq $ \source -> do
    cacheOrContent <- _load (cacheHandle h) target source
    return (source, cacheOrContent)

_load :: Cache.Handle -> Target -> Source.Source -> EIO (Either Cache.Cache T.Text)
_load h t source = do
  mCache <- Cache.loadCache h t source
  case mCache of
    Just cache -> do
      return $ Left cache
    Nothing -> do
      fmap Right $ liftIO . readTextFile $ Source.sourceFilePath source
