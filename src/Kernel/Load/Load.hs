module Kernel.Load.Load
  ( Handle,
    load,
    new,
  )
where

import App.App (App)
import App.Run (forP)
import Data.Text qualified as T
import Kernel.Common.Cache qualified as Cache
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.ManageCache qualified as Cache
import Kernel.Common.Source qualified as Source
import Kernel.Common.Target
import Logger.Debug qualified as Logger
import Logger.Handle qualified as Logger
import Path.Read (readTextFromPath)
import UnliftIO (MonadIO (liftIO))

data Handle = Handle
  { loggerHandle :: Logger.Handle,
    cacheHandle :: Cache.Handle
  }

new :: Global.Handle -> Handle
new globalHandle@(Global.Handle {..}) = do
  let cacheHandle = Cache.new globalHandle
  Handle {..}

load :: Handle -> Target -> [Source.Source] -> App [(Source.Source, Either Cache.Cache T.Text)]
load h target dependenceSeq = do
  liftIO $ Logger.report (loggerHandle h) "Loading source files and caches"
  forP dependenceSeq $ \source -> do
    cacheOrContent <- _load (cacheHandle h) target source
    return (source, cacheOrContent)

_load :: Cache.Handle -> Target -> Source.Source -> App (Either Cache.Cache T.Text)
_load h t source = do
  mCache <- Cache.loadCache h t source
  case mCache of
    Just cache -> do
      return $ Left cache
    Nothing -> do
      fmap Right $ readTextFromPath $ Source.sourceFilePath source
