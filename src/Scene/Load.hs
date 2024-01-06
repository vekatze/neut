module Scene.Load (load) where

import Context.App
import Context.Cache qualified as Cache
import Context.Parse (readTextFile)
import Context.Path qualified as Path
import Data.Text qualified as T
import Entity.Cache qualified as Cache
import Entity.Source qualified as Source

load :: Source.Source -> App (Either Cache.Cache T.Text)
load source = do
  Path.writeBuildSignature (Source.sourceModule source)
  mCache <- Cache.loadCache source
  case mCache of
    Just cache -> do
      return $ Left cache
    Nothing -> do
      fmap Right $ readTextFile $ Source.sourceFilePath source
