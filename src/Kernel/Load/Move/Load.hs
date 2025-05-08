module Kernel.Load.Move.Load
  ( Handle,
    load,
    new,
  )
where

import Aux.Error.Move.Run (forP)
import Aux.Error.Rule.EIO (EIO)
import Aux.Logger.Move.Debug qualified as Logger
import Aux.Logger.Rule.Handle qualified as Logger
import Aux.Path.Move.Read (readText)
import Data.Text qualified as T
import Kernel.Common.Move.CreateGlobalHandle qualified as Global
import Kernel.Common.Move.ManageCache qualified as Cache
import Kernel.Common.Rule.Cache qualified as Cache
import Kernel.Common.Rule.Source qualified as Source
import Kernel.Common.Rule.Target
import UnliftIO (MonadIO (liftIO))

data Handle = Handle
  { loggerHandle :: Logger.Handle,
    cacheHandle :: Cache.Handle
  }

new :: Global.Handle -> Handle
new globalHandle@(Global.Handle {..}) = do
  let cacheHandle = Cache.new globalHandle
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
      fmap Right $ liftIO . readText $ Source.sourceFilePath source
