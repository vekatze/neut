module Entity.Locator.Reflect (fromText) where

import qualified Context.Throw as Throw
import Data.List
import qualified Data.Text as T
import Entity.Global
import Entity.Hint
import Entity.Locator
import Entity.Module
import Entity.Module.Locator
import Entity.ModuleAlias
import Path

fromText :: Throw.Context -> Hint -> Module -> T.Text -> IO Locator
fromText ctx m currentModule sectionString = do
  case getHeadMiddleLast $ T.splitOn "." sectionString of
    Just (nextModuleName, dirNameList, fileNameWithoutExtension) -> do
      nextModule <- getNextModule ctx m currentModule $ ModuleAlias nextModuleName
      let fileName = fileNameWithoutExtension <> nsSep <> sourceFileExtension
      path <- parseRelFile $ T.unpack $ T.intercalate "/" dirNameList <> "/" <> fileName
      return $
        Locator
          { sourceLocatorModule = nextModule,
            sourceLocatorFilePath = path
          }
    Nothing ->
      Throw.raiseError ctx m "found a malformed module signature"

getHeadMiddleLast :: [a] -> Maybe (a, [a], a)
getHeadMiddleLast xs = do
  (y, ys) <- uncons xs
  (zs, z) <- unsnoc ys
  return (y, zs, z)

unsnoc :: [a] -> Maybe ([a], a)
unsnoc =
  foldr go Nothing
  where
    go x acc =
      case acc of
        Nothing ->
          Just ([], x)
        Just (ys, y) ->
          Just (x : ys, y)
