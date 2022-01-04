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
  ( aliasEnvRef,
    getCurrentFilePath,
    newCount,
    newIdentFromText,
    prefixEnvRef,
    setCurrentFilePath,
  )
import Data.IORef
  ( IORef,
    modifyIORef',
    newIORef,
    readIORef,
    writeIORef,
  )
import Data.Log (Error (..), logError, raiseCritical)
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
    parserStateAliasEnv :: [(T.Text, T.Text)]
  }

saveState :: IO ParserState
saveState = do
  text <- readIORef textRef
  line <- readIORef lineRef
  column <- readIORef columnRef
  prefixEnv <- readIORef prefixEnvRef
  aliasEnv <- readIORef aliasEnvRef
  return $
    ParserState
      { parserStateText = text,
        parserStateLine = line,
        parserStateColumn = column,
        parserStatePrefixEnv = prefixEnv,
        parserStateAliasEnv = aliasEnv
      }

loadState :: ParserState -> IO ()
loadState state = do
  writeIORef textRef $ parserStateText state
  writeIORef lineRef $ parserStateLine state
  writeIORef columnRef $ parserStateColumn state
  writeIORef prefixEnvRef $ parserStatePrefixEnv state
  writeIORef aliasEnvRef $ parserStateAliasEnv state

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

betweenParen :: IO a -> IO a
betweenParen f = do
  char '(' >> skip
  item <- f
  char ')' >> skip
  return item

asBlock :: IO a -> IO a
asBlock =
  inBlock "as"

doBlock :: IO a -> IO a
doBlock =
  inBlock "do"

inBlock :: T.Text -> IO a -> IO a
inBlock name f = do
  _ <- token name
  item <- f
  _ <- token "end"
  return item

token :: T.Text -> IO ()
token expected = do
  m <- currentHint
  actual <- symbol
  if actual == expected
    then return ()
    else raiseParseError m $ "found an unexpected token `" <> actual <> "`, expecting: " <> expected

char :: Char -> IO ()
char c = do
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

skip :: IO ()
skip = do
  text <- readIORef textRef
  case T.uncons text of
    Just (c, rest)
      | c == '-',
        Just ('-', _) <- T.uncons rest ->
        comment
      | c `S.member` newlineSet ->
        updateStreamL rest >> skip
      | c `S.member` spaceSet ->
        updateStreamC 1 rest >> skip
    _ ->
      return ()

comment :: IO ()
comment = do
  text <- readIORef textRef
  case T.uncons text of
    Just (c, rest)
      | c `S.member` newlineSet ->
        updateStreamL rest >> skip
      | otherwise ->
        updateStreamC 1 rest >> comment
    Nothing ->
      return ()

many :: IO a -> IO [a]
many f = do
  tryPlanList
    [ do
        x <- f
        xs <- many f
        return $ x : xs,
      return []
    ]

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
  itemList <- many f
  return $ item : itemList

integer :: IO Integer
integer = do
  m <- currentHint
  x <- symbol
  case readMaybe (T.unpack x) of
    Just intValue ->
      return intValue
    Nothing ->
      raiseParseError m $ "unexpected symbol: " <> x <> "\n expecting: an integer"

float :: IO Double
float = do
  m <- currentHint
  x <- symbol
  case readMaybe (T.unpack x) of
    Just floatValue ->
      return floatValue
    Nothing ->
      raiseParseError m $ "unexpected symbol: " <> x <> "\n expecting: an integer"

parseBool :: IO Bool
parseBool = do
  m <- currentHint
  b <- symbol
  case b of
    "true" ->
      return True
    "false" ->
      return False
    _ ->
      raiseParseError m $ "unexpected symbol: `" <> b <> "`\n expecting: `true` or `false`"

tryPlanList :: [IO a] -> IO a
tryPlanList planList =
  case planList of
    [] -> do
      m <- currentHint
      raiseCritical m "Parse.Core.tryPlanList: this function shouldn't be called for the empty list"
    [f] ->
      f
    f : fs -> do
      s <- saveState
      catch f (helper s (tryPlanList fs))

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

sepBy2 :: IO a -> IO b -> IO [a]
sepBy2 f sep = do
  item1 <- f
  _ <- sep
  item2 <- f
  itemList <- many $ sep >> f
  return $ item1 : item2 : itemList

symbol :: IO T.Text
symbol = do
  m <- currentHint
  mx <- symbolMaybe isSymbolChar
  returnSymbol m mx

simpleSymbol :: IO T.Text
simpleSymbol = do
  m <- currentHint
  mx <- symbolMaybe isSimpleSymbolChar
  returnSymbol m mx

{-# INLINE returnSymbol #-}
returnSymbol :: Hint -> Maybe T.Text -> IO T.Text
returnSymbol m mx =
  case mx of
    Nothing ->
      raiseParseError m "unexpected non-symbol character, expecting: symbol-character"
    Just x ->
      return x

{-# INLINE symbolMaybe #-}
symbolMaybe :: (Char -> Bool) -> IO (Maybe T.Text)
symbolMaybe predicate = do
  text <- readIORef textRef
  let x = T.takeWhile predicate text
  let rest = T.dropWhile predicate text
  updateStreamC (T.length x) rest
  skip
  if T.null x
    then return Nothing
    else return $ Just x

var :: IO (Hint, T.Text)
var = do
  m <- currentHint
  x <- symbol
  if isKeyword x
    then raiseParseError m $ "found a reserved symbol `" <> x <> "`, expecting a variable"
    else return (m, x)

simpleVar :: IO (Hint, T.Text)
simpleVar = do
  m <- currentHint
  x <- simpleSymbol
  if isKeyword x
    then raiseParseError m $ "found a reserved symbol `" <> x <> "`, expecting a variable"
    else return (m, x)

string :: IO T.Text
string = do
  char '"'
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

currentHint :: IO Hint
currentHint = do
  line <- readIORef lineRef
  column <- readIORef columnRef
  path <- toFilePath <$> getCurrentFilePath
  return $ newHint (fromEnum line) (fromEnum column) path

{-# INLINE isSymbolChar #-}
isSymbolChar :: Char -> Bool
isSymbolChar c =
  c `S.notMember` nonSymbolSet

{-# INLINE isSimpleSymbolChar #-}
isSimpleSymbolChar :: Char -> Bool
isSimpleSymbolChar c =
  c `S.notMember` nonSimpleSymbolSet

{-# INLINE spaceSet #-}
spaceSet :: S.Set Char
spaceSet =
  S.fromList " "

{-# INLINE newlineSet #-}
newlineSet :: S.Set Char
newlineSet =
  S.fromList "\n"

{-# INLINE nonSymbolSet #-}
nonSymbolSet :: S.Set Char
nonSymbolSet =
  S.fromList "() \"\n;,"

{-# INLINE nonSimpleSymbolSet #-}
nonSimpleSymbolSet :: S.Set Char
nonSimpleSymbolSet =
  S.insert '.' nonSymbolSet

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

-- asKeyword :: T.Text -> Maybe Keywordみたいにすべきかも。
isKeyword :: T.Text -> Bool
isKeyword s =
  S.member s keywordSet

keywordSet :: S.Set T.Text
keywordSet =
  S.fromList
    [ "-",
      "->",
      ".",
      ":",
      "<-",
      "=",
      "*",
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
      "derangement",
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
      "pi",
      "question",
      "reduce",
      "remove-prefix",
      "section",
      "switch",
      "tau",
      "then",
      "unuse",
      "use",
      "with"
    ]

varText :: IO T.Text
varText =
  snd <$> var

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
