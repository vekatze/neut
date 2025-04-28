module Move.Scene.Load
  ( Handle,
    load,
    new,
  )
where

import Data.Text qualified as T
import Move.Context.App
import Move.Context.Cache qualified as Cache
import Move.Context.Color qualified as Color
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (EIO, forP)
import Move.Context.Env qualified as Env
import Move.Context.Parse (readTextFile)
import Rule.Cache qualified as Cache
import Rule.Source qualified as Source
import Rule.Target
import UnliftIO (MonadIO (liftIO))

data Handle
  = Handle
  { debugHandle :: Debug.Handle,
    cacheHandle :: Cache.Handle
  }

new :: Env.Handle -> Color.Handle -> App Handle
new envHandle colorHandle = do
  debugHandle <- Debug.new colorHandle
  cacheHandle <- Cache.new envHandle colorHandle debugHandle
  return $ Handle {..}

load :: Handle -> Target -> [Source.Source] -> EIO [(Source.Source, Either Cache.Cache T.Text)]
load h target dependenceSeq = do
  Debug.report (debugHandle h) "Loading source files and caches"
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
