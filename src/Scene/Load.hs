module Scene.Load (load) where

import Context.App
import Context.Cache qualified as Cache
import Context.Parse (readTextFile)
import Data.Text qualified as T
import Entity.Cache qualified as Cache
import Entity.Source qualified as Source
import Entity.Target

load :: Target -> Source.Source -> App (Either Cache.Cache T.Text)
load t source = do
  mCache <- Cache.loadCache t source
  case mCache of
    Just cache -> do
      return $ Left cache
    Nothing -> do
      fmap Right $ readTextFile $ Source.sourceFilePath source
