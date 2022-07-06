module Entity.Ens.Reflect (fromFilePath) where

import qualified Context.Throw as Throw
import Control.Comonad.Cofree
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import Entity.Ens
import Path
import Scene.Parse.Core
import Text.Megaparsec hiding (parse)

fromFilePath :: Throw.Context -> Path Abs File -> IO Ens
fromFilePath ctx =
  run ctx parseFile

parseFile :: Parser Ens
parseFile = do
  m <- currentHint
  keyValuePairList <- many parseKeyValuePair
  eof
  return $ m :< EnsDictionary (M.fromList keyValuePairList)

parseKeyValuePair :: Parser (T.Text, Ens)
parseKeyValuePair = do
  k <- symbol
  keyword "="
  v <- parseEns
  return (k, v)

parseEns :: Parser Ens
parseEns = do
  m <- currentHint
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

parseDictionary :: Parser (M.HashMap T.Text Ens)
parseDictionary = do
  keyword "{"
  M.fromList <$> manyTill parseKeyValuePair (keyword "}")

parseList :: Parser [Ens]
parseList = do
  keyword "["
  vs <- many parseEns
  keyword "]"
  return vs
