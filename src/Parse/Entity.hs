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
import Data.Global (popTrace, pushTrace)
import qualified Data.HashMap.Lazy as M
import Data.IORef (readIORef)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Parse.Core
  ( char,
    currentHint,
    float,
    initializeState,
    integer,
    many,
    manyStrict,
    parseBool,
    raiseParseError,
    skip,
    string,
    symbol,
    text,
    tryPlanList,
  )
import Path (Abs, File, Path, toFilePath)

parse :: Path Abs File -> IO Entity
parse path = do
  TIO.readFile (toFilePath path) >>= initializeState
  pushTrace path
  m <- currentHint
  ens <- parseFile
  popTrace
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
  s <- readIORef text
  case T.uncons s of
    Nothing ->
      raiseParseError m "unexpected end of input"
    Just (c, _) ->
      raiseParseError m $ "unexpected character: " <> T.singleton c
