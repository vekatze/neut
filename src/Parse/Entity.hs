module Parse.Entity
  ( parse,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)))
import Data.Basic (Hint)
import Data.Entity
  ( Entity,
    EntityF
      ( EntityBool,
        EntityDictionary,
        EntityFloat64,
        EntityInt64,
        EntityList,
        EntityString
      ),
  )
import qualified Data.HashMap.Lazy as M
import Data.IORef (readIORef)
import qualified Data.Text as T
import Parse.Core
  ( char,
    currentHint,
    float,
    initializeParserForFile,
    integer,
    many,
    manyStrict,
    parseBool,
    raiseParseError,
    skip,
    string,
    symbol,
    textRef,
    tryPlanList,
    withNewState,
  )
import Path (Abs, File, Path)

parse :: Path Abs File -> IO Entity
parse path = do
  withNewState $ do
    initializeParserForFile path
    m <- currentHint
    ens <- skip >> parseFile
    return $ m :< EntityDictionary ens

parseEntity :: IO Entity
parseEntity = do
  m <- currentHint
  v <- do
    tryPlanList
      [ EntityDictionary <$> parseDictionary,
        EntityList <$> parseList,
        EntityString <$> string,
        EntityBool <$> parseBool,
        EntityFloat64 <$> float,
        EntityInt64 <$> (fromInteger <$> integer),
        raiseEntityParseError m
      ]
  return $ m :< v

parseDictionary :: IO (M.HashMap T.Text Entity)
parseDictionary = do
  char '{' >> skip
  dictionary <- parseDictionaryInner
  char '}' >> skip
  return dictionary

parseDictionaryInner :: IO (M.HashMap T.Text Entity)
parseDictionaryInner = do
  fmap M.fromList $
    many $ do
      k <- symbol
      char '=' >> skip
      v <- parseEntity
      return (k, v)

parseFile :: IO (M.HashMap T.Text Entity)
parseFile = do
  fmap M.fromList $
    manyStrict $ do
      k <- symbol
      char '=' >> skip
      v <- parseEntity
      return (k, v)

parseList :: IO [Entity]
parseList = do
  char '[' >> skip
  vs <- many parseEntity
  char ']' >> skip
  return vs

raiseEntityParseError :: Hint -> IO a
raiseEntityParseError m = do
  text <- readIORef textRef
  case T.uncons text of
    Nothing ->
      raiseParseError m "unexpected end of input"
    Just (c, _) ->
      raiseParseError m $ "unexpected character: " <> T.singleton c
