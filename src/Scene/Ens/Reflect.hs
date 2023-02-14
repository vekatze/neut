module Scene.Ens.Reflect (fromFilePath) where

import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Data.HashMap.Strict qualified as M
import Data.Text qualified as T
import Entity.Ens qualified as E
import Path
import Scene.Parse.Core
import Text.Megaparsec hiding (parse)

fromFilePath :: (Throw.Context m, Context m) => Path Abs File -> m E.Ens
fromFilePath =
  run parseFile

parseFile :: Context m => Parser m E.Ens
parseFile = do
  m <- getCurrentHint
  keyValuePairList <- many parseKeyValuePair
  eof
  return $ m :< E.Dictionary (M.fromList keyValuePairList)

parseKeyValuePair :: Context m => Parser m (T.Text, E.Ens)
parseKeyValuePair = do
  k <- symbol
  keyword "="
  v <- parseEns
  return (k, v)

parseEns :: Context m => Parser m E.Ens
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

parseDictionary :: Context m => Parser m (M.HashMap T.Text E.Ens)
parseDictionary = do
  keyword "{"
  M.fromList <$> manyTill parseKeyValuePair (keyword "}")

parseList :: Context m => Parser m [E.Ens]
parseList = do
  keyword "["
  vs <- many parseEns
  keyword "]"
  return vs
