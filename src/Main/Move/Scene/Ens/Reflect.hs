module Main.Move.Scene.Ens.Reflect
  ( fromFilePath,
    fromFilePath',
  )
where

import BaseParser.Move.GetInfo
import BaseParser.Move.Parse
import BaseParser.Rule.Parser qualified as P
import Control.Comonad.Cofree
import Control.Monad.Trans
import Data.Set qualified as S
import Data.Text qualified as T
import Ens.Rule.Ens qualified as E
import Error.Rule.EIO (EIO)
import Language.Common.Move.Raise (raiseError)
import Logger.Rule.Hint
import Main.Move.Context.Parse (readTextFile)
import Path
import SyntaxTree.Move.ParseSeries
import SyntaxTree.Rule.C
import SyntaxTree.Rule.Series qualified as SE
import Text.Megaparsec hiding (runParser)
import Text.Read (readMaybe)

fromFilePath :: Path Abs File -> EIO (C, (E.Ens, C))
fromFilePath path = do
  fileContent <- liftIO $ readTextFile path
  fromFilePath' path fileContent

fromFilePath' :: Path Abs File -> T.Text -> EIO (C, (E.Ens, C))
fromFilePath' filePath fileContent = do
  runParser filePath fileContent True parseEns

parseEns :: P.Parser (E.Ens, C)
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

parseInt :: Hint -> P.Parser (E.Ens, C)
parseInt m = do
  (x, c) <- integer
  return (m :< E.Int (fromInteger x), c)

parseFloat :: Hint -> P.Parser (E.Ens, C)
parseFloat m = do
  (x, c) <- float
  return (m :< E.Float x, c)

parseBool :: Hint -> P.Parser (E.Ens, C)
parseBool m = do
  (x, c) <- bool
  return (m :< E.Bool x, c)

parseString :: Hint -> P.Parser (E.Ens, C)
parseString m = do
  (x, c) <- string
  return (m :< E.String x, c)

parseList :: Hint -> P.Parser (E.Ens, C)
parseList m = do
  (ensSeries, c) <- seriesBracket parseEns
  return (m :< E.List ensSeries, c)

parseDictionary :: Hint -> P.Parser (E.Ens, C)
parseDictionary m = do
  (kvs, c) <- seriesBrace parseKeyValuePair
  let kvs' = SE.joinC kvs
  lift $ ensureKeyLinearity (SE.extract kvs') S.empty
  return (m :< E.Dictionary (fmap snd kvs'), c)

parseKeyValuePair :: P.Parser ((C, (Hint, (T.Text, E.Ens))), C)
parseKeyValuePair = do
  m <- getCurrentHint
  (k, cLead) <- symbol
  (v, cTrail) <- parseEns
  return ((cLead, (m, (k, v))), cTrail)

ensureKeyLinearity :: [(Hint, (T.Text, a))] -> S.Set T.Text -> EIO ()
ensureKeyLinearity mks foundKeySet =
  case mks of
    [] ->
      return ()
    (m, (k, _)) : rest
      | S.member k foundKeySet ->
          raiseError m $ "Found a duplicate key: `" <> k <> "`"
      | otherwise ->
          ensureKeyLinearity rest (S.insert k foundKeySet)

integer :: P.Parser (Integer, C)
integer = do
  (s, c) <- symbol
  case readMaybe (T.unpack s) of
    Just value ->
      return (value, c)
    Nothing ->
      failure (Just (P.asTokens s)) (S.fromList [P.asLabel "integer"])

float :: P.Parser (Double, C)
float = do
  (s, c) <- symbol
  case readMaybe (T.unpack s) of
    Just value ->
      return (value, c)
    Nothing -> do
      failure (Just (P.asTokens s)) (S.fromList [P.asLabel "float"])

bool :: P.Parser (Bool, C)
bool = do
  (s, c) <- symbol
  case s of
    "true" ->
      return (True, c)
    "false" ->
      return (False, c)
    _ -> do
      failure (Just (P.asTokens s)) (S.fromList [P.asTokens "true", P.asTokens "false"])
