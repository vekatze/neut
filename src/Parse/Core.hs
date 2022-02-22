module Parse.Core where

import Control.Comonad.Cofree (Cofree (..))
import Control.Exception.Safe (catch, throw)
import Data.Basic
  ( BinderF,
    Hint,
    Ident,
    LamKindF (LamKindNormal),
    asIdent,
    getPosInfo,
    newHint,
  )
import Data.Global
  ( definiteSep,
    getCurrentFilePath,
    globalLocatorListRef,
    locatorAliasMapRef,
    moduleAliasMapRef,
    newCount,
    newIdentFromText,
    setCurrentFilePath,
  )
import qualified Data.HashMap.Lazy as Map
import Data.IORef
  ( IORef,
    modifyIORef',
    newIORef,
    readIORef,
    writeIORef,
  )
import Data.Log (Error (..), logError)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.WeakTerm
  ( WeakTerm,
    WeakTermF (WeakTermPiIntro, WeakTermVar),
  )
import Path (Abs, File, Path, toFilePath)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

type EscapeFlag =
  Bool

{-# NOINLINE textRef #-}
textRef :: IORef T.Text
textRef =
  unsafePerformIO (newIORef "")

{-# NOINLINE lineRef #-}
lineRef :: IORef Int
lineRef =
  unsafePerformIO (newIORef 1)

{-# NOINLINE columnRef #-}
columnRef :: IORef Int
columnRef =
  unsafePerformIO (newIORef 1)

--
-- basic functions for the parser combinator
--

data ParserState = ParserState
  { parserStateText :: T.Text,
    parserStateLine :: Int,
    parserStateColumn :: Int,
    parserStatePrefixEnv :: [T.Text],
    parserStateChecksumAliasMap :: Map.HashMap T.Text T.Text,
    parserStateImportAliasMap :: Map.HashMap T.Text T.Text
  }

saveState :: IO ParserState
saveState = do
  text <- readIORef textRef
  line <- readIORef lineRef
  column <- readIORef columnRef
  globalLocatorList <- readIORef globalLocatorListRef
  moduleAliasMap <- readIORef moduleAliasMapRef
  locatorAliasMap <- readIORef locatorAliasMapRef
  return $
    ParserState
      { parserStateText = text,
        parserStateLine = line,
        parserStateColumn = column,
        parserStatePrefixEnv = globalLocatorList,
        parserStateImportAliasMap = locatorAliasMap,
        parserStateChecksumAliasMap = moduleAliasMap
      }

loadState :: ParserState -> IO ()
loadState state = do
  writeIORef textRef $ parserStateText state
  writeIORef lineRef $ parserStateLine state
  writeIORef columnRef $ parserStateColumn state
  writeIORef globalLocatorListRef $ parserStatePrefixEnv state
  writeIORef locatorAliasMapRef $ parserStateImportAliasMap state
  writeIORef moduleAliasMapRef $ parserStateChecksumAliasMap state

withNewState :: IO a -> IO a
withNewState action = do
  state <- saveState
  result <- action
  loadState state
  return result

initializeParserForFile :: Path Abs File -> IO ()
initializeParserForFile path = do
  fileContent <- TIO.readFile (toFilePath path)
  writeIORef lineRef 1
  writeIORef columnRef 1
  writeIORef textRef fileContent
  setCurrentFilePath path

--
-- basic parser combinators
--

parseChar :: Char -> IO ()
parseChar c = do
  m <- currentHint
  text <- readIORef textRef
  case T.uncons text of
    Nothing ->
      raiseParseError m $
        "unexpected end of input\nexpecting: '" <> T.singleton c <> "'"
    Just (c', rest)
      | c == c' ->
        if c `S.member` newlineSet
          then updateStreamL rest
          else updateStreamC 1 rest
      | otherwise ->
        raiseParseError m $
          "unexpected character: '"
            <> T.singleton c'
            <> "'\nexpecting: '"
            <> T.singleton c
            <> "'"

parseToken :: T.Text -> IO ()
parseToken expected = do
  text <- readIORef textRef
  case T.stripPrefix expected text of
    Just rest -> do
      updateStreamC (T.length expected) rest
      skip
    Nothing -> do
      m <- currentHint
      raiseParseError m $ "could not find the expected token `" <> expected <> "`"

parseSymbol :: IO T.Text
parseSymbol = do
  m <- currentHint
  mx <- parseByPredicate isSymbolChar
  case mx of
    Just x ->
      return x
    Nothing ->
      raiseParseError m "unexpected non-symbol character, expecting: symbol-char"

{-# INLINE parseByPredicate #-}
parseByPredicate :: (Char -> Bool) -> IO (Maybe T.Text)
parseByPredicate predicate = do
  text <- readIORef textRef
  let x = T.takeWhile predicate text
  let rest = T.dropWhile predicate text
  updateStreamC (T.length x) rest
  skip
  if T.null x
    then return Nothing
    else return $ Just x

parseInteger :: IO Integer
parseInteger = do
  m <- currentHint
  x <- parseSymbol
  case readMaybe (T.unpack x) of
    Just intValue ->
      return intValue
    Nothing ->
      raiseParseError m $ "unexpected symbol: " <> x <> "\n expecting: an integer"

parseFloat :: IO Double
parseFloat = do
  m <- currentHint
  x <- parseSymbol
  case readMaybe (T.unpack x) of
    Just floatValue ->
      return floatValue
    Nothing ->
      raiseParseError m $ "unexpected symbol: " <> x <> "\n expecting: a float"

parseBool :: IO Bool
parseBool = do
  m <- currentHint
  b <- parseSymbol
  case b of
    "true" ->
      return True
    "false" ->
      return False
    _ ->
      raiseParseError m $ "unexpected symbol: `" <> b <> "`\n expecting: `true` or `false`"

parseString :: IO T.Text
parseString = do
  parseChar '"'
  text <- readIORef textRef
  len <- stringLengthOf False text 0
  let (x, s') = T.splitAt len text
  writeIORef textRef $ T.tail s'
  skip
  return x

stringLengthOf :: EscapeFlag -> T.Text -> Int -> IO Int
stringLengthOf flag s i =
  case T.uncons s of
    Nothing -> do
      m <- currentHint
      raiseParseError m "unexpected end of input while parsing string"
    Just (c, rest)
      | c == '"' -> do
        incrementColumn
        if flag
          then stringLengthOf False rest (i + 1)
          else return i
      | c == '\\' -> do
        incrementColumn
        stringLengthOf (not flag) rest (i + 1)
      | c `S.member` newlineSet -> do
        incrementLine
        stringLengthOf False rest (i + 1)
      | otherwise -> do
        incrementColumn
        stringLengthOf False rest (i + 1)

skip :: IO ()
skip = do
  text <- readIORef textRef
  case T.uncons text of
    Just (c, rest)
      | c == '/',
        Just ('/', _) <- T.uncons rest ->
        parseComment
      | c `S.member` newlineSet ->
        updateStreamL rest >> skip
      | c `S.member` spaceSet ->
        updateStreamC 1 rest >> skip
    _ ->
      return ()

parseComment :: IO ()
parseComment = do
  text <- readIORef textRef
  case T.uncons text of
    Just (c, rest)
      | c `S.member` newlineSet ->
        updateStreamL rest >> skip
      | otherwise ->
        updateStreamC 1 rest >> parseComment
    Nothing ->
      return ()

parseBetweenParen :: IO a -> IO a
parseBetweenParen f = do
  parseChar '(' >> skip
  item <- f
  parseChar ')' >> skip
  return item

parseBetweenAngle :: IO a -> IO a
parseBetweenAngle f = do
  parseChar '<' >> skip
  item <- f
  parseChar '>' >> skip
  return item

parseBetweenBracket :: IO a -> IO a
parseBetweenBracket f = do
  parseChar '[' >> skip
  item <- f
  parseChar ']' >> skip
  return item

parseAsBlock :: IO a -> IO a
parseAsBlock =
  parseInBlock "as"

parseDoBlock :: IO a -> IO a
parseDoBlock =
  parseInBlock "do"

parseInBlock :: T.Text -> IO a -> IO a
parseInBlock name f = do
  _ <- parseToken name
  item <- f
  _ <- parseToken "end"
  return item

parseArgList :: IO a -> IO [a]
parseArgList f =
  parseBetweenParen $ sepBy (parseChar ',' >> skip) f

parseArgList2 :: IO a -> IO [a]
parseArgList2 f =
  parseBetweenParen $ sepBy2 (parseChar ',' >> skip) f

parseImpArgList :: IO a -> IO [a]
parseImpArgList f =
  tryPlanList
    [parseBetweenAngle $ sepBy (parseChar ',' >> skip) f]
    (return [])

parseManyList :: IO a -> IO [a]
parseManyList f =
  parseMany $ parseToken "-" >> f

parseMany :: IO a -> IO [a]
parseMany f = do
  tryPlanList
    [ do
        x <- f
        xs <- parseMany f
        return $ x : xs
    ]
    (return [])

manyStrict :: IO a -> IO [a]
manyStrict f = do
  text <- readIORef textRef
  if T.null text
    then return []
    else do
      x <- f
      xs <- manyStrict f
      return $ x : xs

many1 :: IO a -> IO [a]
many1 f = do
  item <- f
  itemList <- parseMany f
  return $ item : itemList

takeN :: Int -> IO a -> IO [a]
takeN n f = do
  if n == 0
    then return []
    else do
      item <- f
      itemList <- takeN (n - 1) f
      return $ item : itemList

-- tryPlanList :: IO a -> [IO a] -> IO a
-- tryPlanList plan planList =
--   case planList of
--     [] -> do
--       plan
--     f : fs -> do
--       s <- saveState
--       catch plan (helper s (tryPlanList f fs))

tryPlanList :: [IO a] -> IO a -> IO a
tryPlanList planList recovery = do
  s <- saveState
  let run [] =
        recovery
      run (plan : restPlanList) =
        catch plan (helper s (run restPlanList))
  run planList

helper :: ParserState -> IO a -> Error -> IO a
helper s g _ = do
  loadState s
  g

lookAhead :: IO a -> IO a
lookAhead f = do
  s <- saveState
  result <- f
  loadState s
  return result

sepBy :: IO b -> IO a -> IO [a]
sepBy sep f =
  tryPlanList [sepBy1 sep f] (return [])

sepBy1 :: IO b -> IO a -> IO [a]
sepBy1 sep f = do
  item1 <- f
  itemList <- parseMany $ sep >> f
  return $ item1 : itemList

sepBy2 :: IO b -> IO a -> IO [a]
sepBy2 sep f = do
  item1 <- f
  _ <- sep
  item2 <- f
  itemList <- parseMany $ sep >> f
  return $ item1 : item2 : itemList

parseVar :: IO (Hint, T.Text)
parseVar = do
  m <- currentHint
  x <- parseSymbol
  if isKeyword x
    then raiseParseError m $ "found a reserved symbol `" <> x <> "`, expecting a variable"
    else return (m, x)

parseDefiniteDescription :: IO (Hint, T.Text)
parseDefiniteDescription = do
  m <- currentHint
  x <- parseSymbol
  parseToken definiteSep
  y <- parseSymbol
  return (m, x <> definiteSep <> y)

--
-- language-dependent auxiliary parser combinators
--

currentHint :: IO Hint
currentHint = do
  line <- readIORef lineRef
  column <- readIORef columnRef
  path <- toFilePath <$> getCurrentFilePath
  return $ newHint (fromEnum line) (fromEnum column) path

--
-- symbol
--

{-# INLINE isSymbolChar #-}
isSymbolChar :: Char -> Bool
isSymbolChar c =
  c `S.notMember` nonSymbolSet

{-# INLINE nonSymbolSet #-}
nonSymbolSet :: S.Set Char
nonSymbolSet =
  S.fromList "() \"\n:;,!?<>[]"

--
-- auxiliary charset
--

{-# INLINE spaceSet #-}
spaceSet :: S.Set Char
spaceSet =
  S.fromList " "

{-# INLINE newlineSet #-}
newlineSet :: S.Set Char
newlineSet =
  S.fromList "\n"

--
-- utility functions
--

{-# INLINE updateStreamL #-}
updateStreamL :: T.Text -> IO ()
updateStreamL text = do
  modifyIORef' textRef $ const text
  incrementLine

{-# INLINE updateStreamC #-}
updateStreamC :: Int -> T.Text -> IO ()
updateStreamC column text = do
  modifyIORef' textRef $ const text
  modifyIORef' columnRef $ (+) column

{-# INLINE incrementLine #-}
incrementLine :: IO ()
incrementLine = do
  modifyIORef' lineRef $ (+) 1
  modifyIORef' columnRef $ const 1

{-# INLINE incrementColumn #-}
incrementColumn :: IO ()
incrementColumn =
  modifyIORef' columnRef $ (+) 1

raiseParseError :: Hint -> T.Text -> IO a
raiseParseError m txt =
  throw $ Error [logError (getPosInfo m) txt]

isKeyword :: T.Text -> Bool
isKeyword s =
  S.member s keywordSet

keywordSet :: S.Set T.Text
keywordSet =
  S.fromList
    [ "-",
      "->",
      "&",
      "*",
      ".",
      ":",
      "<-",
      "=",
      "?",
      "?admit",
      "admit",
      "as",
      "define",
      "define-codata",
      "define-data",
      "define-enum",
      "define-inline",
      "define-prefix",
      "define-resource-type",
      "magic",
      "do",
      "else",
      "else-if",
      "end",
      "ensure",
      "hole",
      "idealize",
      "if",
      "import",
      "in",
      "introspect",
      "lambda",
      "let",
      "let?",
      "match",
      "match-noetic",
      "new",
      "over",
      "private",
      "public",
      "question",
      "reduce",
      "section",
      "switch",
      "tau",
      "text",
      "then",
      "unuse",
      "use",
      "with"
    ]

parseVarText :: IO T.Text
parseVarText =
  snd <$> parseVar

weakVar :: Hint -> T.Text -> WeakTerm
weakVar m str =
  m :< WeakTermVar (asIdent str)

weakVar' :: Hint -> Ident -> WeakTerm
weakVar' m ident =
  m :< WeakTermVar ident

lam :: Hint -> [BinderF WeakTerm] -> WeakTerm -> WeakTerm
lam m varList e =
  m :< WeakTermPiIntro LamKindNormal varList e

newTextualIdentFromText :: T.Text -> IO Ident
newTextualIdentFromText txt = do
  i <- newCount
  newIdentFromText $ ";" <> txt <> T.pack (show i)

weakTermToWeakIdent :: Hint -> IO WeakTerm -> IO (BinderF WeakTerm)
weakTermToWeakIdent m f = do
  a <- f
  h <- newTextualIdentFromText "_"
  return (m, h, a)
