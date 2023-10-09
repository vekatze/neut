module Scene.Ens.Reflect (fromFilePath) where

import Context.App
import Control.Comonad.Cofree
import Data.HashMap.Strict qualified as M
import Data.Text qualified as T
import Entity.Ens qualified as E
import Path
import Scene.Parse.Core
import Text.Megaparsec hiding (parse)

fromFilePath :: Path Abs File -> App E.Ens
fromFilePath =
  run parseEns

parseKeyValuePair :: Parser (T.Text, E.Ens)
parseKeyValuePair = do
  k <- symbol
  v <- parseEns
  return (k, v)

parseEns :: Parser E.Ens
parseEns = do
  m <- getCurrentHint
  v <- do
    choice
      [ E.Dictionary <$> parseDictionary,
        E.List <$> parseList,
        E.String <$> string,
        E.Int <$> try (fromInteger <$> integer),
        E.Float <$> try float,
        E.Bool <$> bool
      ]
  return $ m :< v

parseDictionary :: Parser (M.HashMap T.Text E.Ens)
parseDictionary = do
  delimiter "{"
  M.fromList <$> manyTill parseKeyValuePair (delimiter "}")

parseList :: Parser [E.Ens]
parseList = do
  delimiter "["
  vs <- many parseEns
  delimiter "]"
  return vs
