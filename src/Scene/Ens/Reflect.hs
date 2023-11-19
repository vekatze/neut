module Scene.Ens.Reflect (fromFilePath) where

import Context.App
import Context.Parse
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Control.Monad.Trans
import Data.HashMap.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.Ens qualified as E
import Entity.Hint
import Path
import Scene.Parse.Core
import Text.Megaparsec hiding (parse)

fromFilePath :: Path Abs File -> App E.Ens
fromFilePath path = do
  fileContent <- readSourceFile path
  run parseEns path fileContent

parseKeyValuePair :: Parser ((Hint, T.Text), E.Ens)
parseKeyValuePair = do
  m <- getCurrentHint
  k <- symbol
  v <- parseEns
  return ((m, k), v)

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
  (mks, vs) <- unzip <$> manyTill parseKeyValuePair (delimiter "}")
  lift $ ensureKeyLinearity mks S.empty
  return $ M.fromList $ zip (map snd mks) vs

parseList :: Parser [E.Ens]
parseList = do
  delimiter "["
  vs <- many parseEns
  delimiter "]"
  return vs

ensureKeyLinearity :: [(Hint, T.Text)] -> S.Set T.Text -> App ()
ensureKeyLinearity mks foundKeySet =
  case mks of
    [] ->
      return ()
    (m, k) : rest
      | S.member k foundKeySet ->
          Throw.raiseError m $ "found a duplicated key: `" <> k <> "`"
      | otherwise ->
          ensureKeyLinearity rest (S.insert k foundKeySet)
