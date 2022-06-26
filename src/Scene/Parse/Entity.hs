module Scene.Parse.Entity
  ( parse,
  )
where

import Control.Comonad.Cofree
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import Entity.Entity
import Path
import Scene.Parse.Core
import Text.Megaparsec hiding (parse)

parse :: Path Abs File -> IO Entity
parse =
  run parseFile

parseFile :: Parser Entity
parseFile = do
  m <- currentHint
  keyValuePairList <- many parseKeyValuePair
  eof
  return $ m :< EntityDictionary (M.fromList keyValuePairList)

parseKeyValuePair :: Parser (T.Text, Entity)
parseKeyValuePair = do
  k <- symbol
  keyword "="
  v <- parseEntity
  return (k, v)

parseEntity :: Parser Entity
parseEntity = do
  m <- currentHint
  v <- do
    choice
      [ EntityDictionary <$> parseDictionary,
        EntityList <$> parseList,
        EntityString <$> string,
        EntityInt64 <$> try (fromInteger <$> integer),
        EntityFloat64 <$> try float,
        EntityBool <$> bool
      ]
  return $ m :< v

parseDictionary :: Parser (M.HashMap T.Text Entity)
parseDictionary = do
  keyword "{"
  M.fromList <$> manyTill parseKeyValuePair (keyword "}")

parseList :: Parser [Entity]
parseList = do
  keyword "["
  vs <- many parseEntity
  keyword "]"
  return vs
