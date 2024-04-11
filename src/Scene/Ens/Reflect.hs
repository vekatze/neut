module Scene.Ens.Reflect
  ( fromFilePath,
    fromFilePath',
  )
where

import Context.App
import Context.Parse
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Control.Monad.Trans
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.C
import Entity.Ens qualified as E
import Entity.Hint
import Path
import Scene.Parse.Core
import Text.Megaparsec hiding (parse)

fromFilePath :: Path Abs File -> App (C, (E.Ens, C))
fromFilePath path = do
  fileContent <- readTextFile path
  fromFilePath' path fileContent

fromFilePath' :: Path Abs File -> T.Text -> App (C, (E.Ens, C))
fromFilePath' path content = do
  parseFile True parseEns path content

parseEns :: Parser (E.Ens, C)
parseEns = do
  m <- getCurrentHint
  choice
    [ parseDictionary m,
      parseList m,
      parseString m,
      try $ parseBool m,
      try $ parseInt m,
      parseFloat m
    ]

parseInt :: Hint -> Parser (E.Ens, C)
parseInt m = do
  (x, c) <- integer
  return (m :< E.Int (fromInteger x), c)

parseFloat :: Hint -> Parser (E.Ens, C)
parseFloat m = do
  (x, c) <- float
  return (m :< E.Float x, c)

parseBool :: Hint -> Parser (E.Ens, C)
parseBool m = do
  (x, c) <- bool
  return (m :< E.Bool x, c)

parseString :: Hint -> Parser (E.Ens, C)
parseString m = do
  (x, c) <- string
  return (m :< E.String x, c)

parseList :: Hint -> Parser (E.Ens, C)
parseList m = do
  (ensSeries, c) <- seriesBracket parseEns
  return (m :< E.List ensSeries, c)

parseDictionary :: Hint -> Parser (E.Ens, C)
parseDictionary m = do
  c1 <- delimiter "{"
  (ms, kvs) <- unzip <$> many parseKeyValuePair
  c2 <- delimiter "}"
  lift $ ensureKeyLinearity (zip ms (map fst kvs)) S.empty
  return (m :< E.Dictionary c1 kvs, c2)

parseKeyValuePair :: Parser (Hint, (T.Text, (C, (E.Ens, C))))
parseKeyValuePair = do
  m <- getCurrentHint
  (k, cLead) <- symbol
  (v, cTrail) <- parseEns
  return (m, (k, (cLead, (v, cTrail))))

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
