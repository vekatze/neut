module Scene.LSP.FindDefinition (findDefinition) where

import Context.App
import Context.Cache qualified as Cache
import Entity.Cache qualified as Cache
import Entity.Hint qualified as H
import Entity.LocationTree qualified as LT
import Scene.Source.Reflect qualified as Source
import Scene.Unravel

findDefinition :: FilePath -> (Int, Int) -> App (Maybe (FilePath, (Int, Int)))
findDefinition fp (line, col) = do
  mSrc <- Source.reflect fp
  case mSrc of
    Just src -> do
      _ <- unravel' src
      mCache <- Cache.loadCache src
      case mCache of
        Just cache -> do
          let locationTree = Cache.locationTree cache
          case LT.find line col locationTree of
            Just m -> do
              let defPath = H.metaFileName m
              let (defLine, defCol) = H.metaLocation m
              return $ Just (defPath, (defLine, defCol))
            Nothing -> do
              return Nothing
        Nothing ->
          return Nothing
    Nothing ->
      return Nothing
