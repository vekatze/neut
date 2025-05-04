module Main.Move.Scene.Ens.Reflect
  ( Handle (..),
    new,
    fromFilePath,
    fromFilePath',
  )
where

import Control.Comonad.Cofree
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Trans
import Data.Set qualified as S
import Data.Text qualified as T
import Gensym.Rule.Handle qualified as Gensym
import Language.Common.Rule.Error (newError)
import Language.Common.Rule.Hint
import Language.RawTerm.Rule.C
import Language.RawTerm.Rule.Syntax.Series qualified as SE
import Main.Move.Context.EIO (EIO)
import Main.Move.Context.Parse
import Main.Move.Scene.Parse.Core qualified as P
import Main.Rule.Ens qualified as E
import Path
import Text.Megaparsec hiding (parse)

newtype Handle = Handle
  { gensymHandle :: Gensym.Handle
  }

new :: Gensym.Handle -> Handle
new gensymHandle = do
  Handle {..}

fromFilePath :: Handle -> Path Abs File -> EIO (C, (E.Ens, C))
fromFilePath h path = do
  fileContent <- liftIO $ readTextFile path
  fromFilePath' h path fileContent

fromFilePath' :: Handle -> Path Abs File -> T.Text -> EIO (C, (E.Ens, C))
fromFilePath' h filePath fileContent = do
  let h' = P.Handle {gensymHandle = gensymHandle h}
  P.parseFile h' filePath fileContent True (const parseEns)

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
          throwError $ newError m $ "Found a duplicate key: `" <> k <> "`"
      | otherwise ->
          ensureKeyLinearity rest (S.insert k foundKeySet)
