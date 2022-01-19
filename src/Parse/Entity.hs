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
  ( currentHint,
    initializeParserForFile,
    manyStrict,
    parseBool,
    parseChar,
    parseFloat,
    parseInteger,
    parseMany,
    parseString,
    parseSymbol,
    raiseParseError,
    skip,
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
        EntityString <$> parseString,
        EntityBool <$> parseBool,
        EntityFloat64 <$> parseFloat,
        EntityInt64 <$> (fromInteger <$> parseInteger)
      ]
      (raiseEntityParseError m)
  return $ m :< v

parseDictionary :: IO (M.HashMap T.Text Entity)
parseDictionary = do
  parseChar '{' >> skip
  dictionary <- parseDictionaryInner
  parseChar '}' >> skip
  return dictionary

parseDictionaryInner :: IO (M.HashMap T.Text Entity)
parseDictionaryInner = do
  fmap M.fromList $
    parseMany $ do
      k <- parseSymbol
      parseChar '=' >> skip
      v <- parseEntity
      return (k, v)

parseFile :: IO (M.HashMap T.Text Entity)
parseFile = do
  fmap M.fromList $
    manyStrict $ do
      k <- parseSymbol
      parseChar '=' >> skip
      v <- parseEntity
      return (k, v)

parseList :: IO [Entity]
parseList = do
  parseChar '[' >> skip
  vs <- parseMany parseEntity
  parseChar ']' >> skip
  return vs

raiseEntityParseError :: Hint -> IO a
raiseEntityParseError m = do
  text <- readIORef textRef
  case T.uncons text of
    Nothing ->
      raiseParseError m "unexpected end of input"
    Just (c, _) ->
      raiseParseError m $ "unexpected character: " <> T.singleton c
