module Entity.SourceLocator.Reflect (fromText) where

import Context.Throw
import Data.List
import qualified Data.Text as T
import Entity.Global
import Entity.Hint
import Entity.Module
import Entity.Module.Locator
import Entity.ModuleAlias
import Entity.SourceLocator

fromText :: Context -> Hint -> Module -> T.Text -> IO SourceLocator
fromText ctx m currentModule sectionString = do
  case getHeadMiddleLast $ T.splitOn "." sectionString of
    Just (nextModuleName, dirNameList, fileNameWithoutExtension) -> do
      nextModule <- getNextModule ctx m currentModule $ ModuleAlias nextModuleName
      let fileName = fileNameWithoutExtension <> nsSep <> sourceFileExtension
      return $
        SourceLocator
          { sourceLocatorModule = nextModule,
            sourceLocatorDirNameList = map (DirName . T.unpack) dirNameList,
            sourceLocatorFileName = FileName . T.unpack $ fileName
          }
    Nothing ->
      raiseError ctx m "found a malformed module signature"

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

-- sourceSignatureは、(ModuleAlias, [DirPath], FileName) になってるのか。
-- sectionToPath :: [T.Text] -> FilePath
-- sectionToPath sectionPath =
--   T.unpack $ T.intercalate (T.singleton FP.pathSeparator) sectionPath <> "." <> sourceFileExtension
