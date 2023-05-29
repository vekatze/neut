module Scene.LSP.FindDefinition (findDefinition) where

import Context.App
import Context.Cache qualified as Cache
import Control.Monad.IO.Class
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Entity.Cache qualified as Cache
import Entity.Hint qualified as H
import Entity.LocationTree qualified as LT
import Scene.Source.Reflect qualified as Source
import Scene.Unravel
import System.IO

findDefinition :: FilePath -> (Int, Int) -> App (Maybe (FilePath, (Int, Int)))
findDefinition fp (line, col) = do
  mSrc <- Source.reflect fp
  case mSrc of
    Just src -> do
      printLog "constructed src"
      _ <- unravel' src
      mCache <- Cache.loadCache src
      case mCache of
        Just cache -> do
          printLog "found cache"
          let locationTree = Cache.locationTree cache
          case LT.find line col locationTree of
            Just m -> do
              printLog "found definition"
              let defPath = H.metaFileName m
              let (defLine, defCol) = H.metaLocation m
              return $ Just (defPath, (defLine, defCol))
            Nothing -> do
              return Nothing
        Nothing ->
          return Nothing
    Nothing ->
      return Nothing

printLog :: T.Text -> App ()
printLog text =
  liftIO $ TIO.hPutStrLn stderr text
