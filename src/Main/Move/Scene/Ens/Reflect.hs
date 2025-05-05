module Main.Move.Scene.Ens.Reflect
  ( Handle (..),
    new,
    fromFilePath,
    fromFilePath',
  )
where

import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Trans
import Data.Set qualified as S
import Data.Text qualified as T
import Ens.Rule.Ens qualified as E
import Error.Rule.EIO (EIO)
import Gensym.Rule.Handle qualified as Gensym
import Language.Common.Move.Raise (raiseError)
import Language.Common.Rule.Hint
import Main.Move.Context.Parse (readTextFile)
import Main.Move.Scene.Parse.Core qualified as P
import Path
import SyntaxTree.Rule.C
import SyntaxTree.Rule.Series qualified as SE
import Text.Megaparsec hiding (parse)

newtype Handle = Handle
  { gensymHandle :: Gensym.Handle
  }

type MustParseWholeFile =
  Bool

parse :: Path Abs File -> T.Text -> MustParseWholeFile -> P.Parser a -> EIO (C, a)
parse filePath fileContent mustParseWholeFile parser = do
  let fileParser = do
        leadingComments <- P.spaceConsumer
        value <- parser
        when mustParseWholeFile eof
        return (leadingComments, value)
  let path = toFilePath filePath
  result <- runParserT fileParser path fileContent
  case result of
    Right v ->
      return v
    Left errorBundle ->
      throwError $ P.createParseError errorBundle

new :: Gensym.Handle -> Handle
new gensymHandle = do
  Handle {..}

fromFilePath :: Path Abs File -> EIO (C, (E.Ens, C))
fromFilePath path = do
  fileContent <- liftIO $ readTextFile path
  fromFilePath' path fileContent

fromFilePath' :: Path Abs File -> T.Text -> EIO (C, (E.Ens, C))
fromFilePath' filePath fileContent = do
  parse filePath fileContent True parseEns

parseEns :: P.Parser (E.Ens, C)
parseEns = do
  m <- P.getCurrentHint
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
  (x, c) <- P.integer
  return (m :< E.Int (fromInteger x), c)

parseFloat :: Hint -> P.Parser (E.Ens, C)
parseFloat m = do
  (x, c) <- P.float
  return (m :< E.Float x, c)

parseBool :: Hint -> P.Parser (E.Ens, C)
parseBool m = do
  (x, c) <- P.bool
  return (m :< E.Bool x, c)

parseString :: Hint -> P.Parser (E.Ens, C)
parseString m = do
  (x, c) <- P.string
  return (m :< E.String x, c)

parseList :: Hint -> P.Parser (E.Ens, C)
parseList m = do
  (ensSeries, c) <- P.seriesBracket parseEns
  return (m :< E.List ensSeries, c)

parseDictionary :: Hint -> P.Parser (E.Ens, C)
parseDictionary m = do
  (kvs, c) <- P.seriesBrace parseKeyValuePair
  let kvs' = SE.joinC kvs
  lift $ ensureKeyLinearity (SE.extract kvs') S.empty
  return (m :< E.Dictionary (fmap snd kvs'), c)

parseKeyValuePair :: P.Parser ((C, (Hint, (T.Text, E.Ens))), C)
parseKeyValuePair = do
  m <- P.getCurrentHint
  (k, cLead) <- P.symbol
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
