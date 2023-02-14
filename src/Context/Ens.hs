module Context.Ens (fromFilePath) where

import Context.App
import Control.Comonad.Cofree
import Data.HashMap.Strict qualified as M
import Data.Text qualified as T
import Data.Void
import Entity.Ens qualified as E
import Path
import Scene.Parse.Core
import Text.Megaparsec hiding (parse)

type Parser m = ParsecT Void T.Text m

fromFilePath :: Path Abs File -> App E.Ens
fromFilePath =
  run parseFile

parseFile :: Parser m E.Ens
parseFile = do
  m <- getCurrentHint
  keyValuePairList <- many parseKeyValuePair
  eof
  return $ m :< E.Dictionary (M.fromList keyValuePairList)

parseKeyValuePair :: Parser m (T.Text, E.Ens)
parseKeyValuePair = do
  k <- symbol
  keyword "="
  v <- parseEns
  return (k, v)

parseEns :: Parser m E.Ens
parseEns = do
  m <- getCurrentHint
  v <- do
    choice
      [ E.Dictionary <$> parseDictionary,
        E.List <$> parseList,
        E.String <$> string,
        E.Int64 <$> try (fromInteger <$> integer),
        E.Float64 <$> try float,
        E.Bool <$> bool
      ]
  return $ m :< v

parseDictionary :: Parser m (M.HashMap T.Text E.Ens)
parseDictionary = do
  keyword "{"
  M.fromList <$> manyTill parseKeyValuePair (keyword "}")

parseList :: Parser m [E.Ens]
parseList = do
  keyword "["
  vs <- many parseEns
  keyword "]"
  return vs
