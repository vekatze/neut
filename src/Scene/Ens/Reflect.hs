module Scene.Ens.Reflect (fromFilePath) where

import Context.App
import Context.Parse
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Control.Monad.Trans
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
  commentList <- parseCommentList
  k <- symbol'
  v <- parseEns
  return ((m, k), foldCommentList m v commentList)

foldCommentList :: Hint -> E.Ens -> [T.Text] -> E.Ens
foldCommentList m =
  foldr (\comment acc -> m :< E.Comment comment acc)

parseEns :: Parser E.Ens
parseEns = do
  parseOptionalComment $ do
    choice
      [ parseDictionary,
        parseList,
        parseString,
        try parseInt,
        parseFloat,
        parseBool
      ]

parseInt :: Parser E.Ens
parseInt = do
  m <- getCurrentHint
  s <- fromInteger <$> integer'
  return $ m :< E.Int s

parseFloat :: Parser E.Ens
parseFloat = do
  m <- getCurrentHint
  s <- float'
  return $ m :< E.Float s

parseBool :: Parser E.Ens
parseBool = do
  m <- getCurrentHint
  s <- bool'
  return $ m :< E.Bool s

parseString :: Parser E.Ens
parseString = do
  m <- getCurrentHint
  s <- string'
  return $ m :< E.String s

parseDictionary :: Parser E.Ens
parseDictionary = do
  m <- getCurrentHint
  delimiter' "{"
  (mks, vs) <- unzip <$> manyTill parseKeyValuePair (delimiter' "}")
  lift $ ensureKeyLinearity mks S.empty
  return $ m :< E.Dictionary (zip (map snd mks) vs)

parseList :: Parser E.Ens
parseList = do
  m <- getCurrentHint
  delimiter' "["
  vs <- many parseEns
  delimiter' "]"
  return $ m :< E.List vs

parseCommentList :: Parser [T.Text]
parseCommentList =
  many $ do
    chunk "//"
    comment <- takeWhileP (Just "character") (/= '\n')
    spaceConsumer'
    return comment

parseOptionalComment :: Parser E.Ens -> Parser E.Ens
parseOptionalComment p = do
  m <- getCurrentHint
  choice
    [ do
        chunk "//"
        comment <- takeWhileP (Just "character") (/= '\n')
        spaceConsumer'
        content <- p
        return $ m :< E.Comment comment content,
      p
    ]

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
