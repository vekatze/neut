module Entity.Ens.Reflect (fromFilePath) where

import qualified Context.Throw as Throw
import Control.Comonad.Cofree
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import Entity.Ens
import Path
import Scene.Parse.Core
import Text.Megaparsec hiding (parse)

fromFilePath :: (Throw.Context m, Context m) => Path Abs File -> m Ens
fromFilePath =
  run parseFile

parseFile :: Context m => Parser m Ens
parseFile = do
  m <- getCurrentHint
  keyValuePairList <- many parseKeyValuePair
  eof
  return $ m :< EnsDictionary (M.fromList keyValuePairList)

parseKeyValuePair :: Context m => Parser m (T.Text, Ens)
parseKeyValuePair = do
  k <- symbol
  keyword "="
  v <- parseEns
  return (k, v)

parseEns :: Context m => Parser m Ens
parseEns = do
  m <- getCurrentHint
  v <- do
    choice
      [ EnsDictionary <$> parseDictionary,
        EnsList <$> parseList,
        EnsString <$> string,
        EnsInt64 <$> try (fromInteger <$> integer),
        EnsFloat64 <$> try float,
        EnsBool <$> bool
      ]
  return $ m :< v

parseDictionary :: Context m => Parser m (M.HashMap T.Text Ens)
parseDictionary = do
  keyword "{"
  M.fromList <$> manyTill parseKeyValuePair (keyword "}")

parseList :: Context m => Parser m [Ens]
parseList = do
  keyword "["
  vs <- many parseEns
  keyword "]"
  return vs
