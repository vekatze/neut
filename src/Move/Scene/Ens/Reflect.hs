module Move.Scene.Ens.Reflect
  ( fromFilePath,
    fromFilePath',
  )
where

import Control.Comonad.Cofree
import Control.Monad.Trans
import Data.Set qualified as S
import Data.Text qualified as T
import Move.Context.App
import Move.Context.Parse
import Move.Context.Throw qualified as Throw
import Move.Scene.Parse.Core
import Path
import Rule.C
import Rule.Ens qualified as E
import Rule.Hint
import Rule.Syntax.Series qualified as SE
import Text.Megaparsec hiding (parse)

fromFilePath :: Path Abs File -> App (C, (E.Ens, C))
fromFilePath path = do
  fileContent <- liftIO $ readTextFile path
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
  (kvs, c) <- seriesBrace parseKeyValuePair
  let kvs' = SE.joinC kvs
  lift $ ensureKeyLinearity (SE.extract kvs') S.empty
  return (m :< E.Dictionary (fmap snd kvs'), c)

parseKeyValuePair :: Parser ((C, (Hint, (T.Text, E.Ens))), C)
parseKeyValuePair = do
  m <- getCurrentHint
  (k, cLead) <- symbol
  (v, cTrail) <- parseEns
  return ((cLead, (m, (k, v))), cTrail)

ensureKeyLinearity :: [(Hint, (T.Text, a))] -> S.Set T.Text -> App ()
ensureKeyLinearity mks foundKeySet =
  case mks of
    [] ->
      return ()
    (m, (k, _)) : rest
      | S.member k foundKeySet ->
          Throw.raiseError m $ "Found a duplicate key: `" <> k <> "`"
      | otherwise ->
          ensureKeyLinearity rest (S.insert k foundKeySet)
