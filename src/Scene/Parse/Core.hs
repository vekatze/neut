module Scene.Parse.Core where

import Context.App
import Context.Gensym qualified as Gensym
import Context.Parse
import Context.Throw qualified as Throw
import Control.Monad
import Control.Monad.Trans
import Data.List.NonEmpty
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Void
import Entity.BaseName qualified as BN
import Entity.Const
import Entity.Error qualified as E
import Entity.Hint
import Entity.Hint.Reflect qualified as Hint
import Path
import Text.Megaparsec
import Text.Megaparsec.Char hiding (string')
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Read qualified as R

type Parser = ParsecT Void T.Text App

run :: Parser a -> Path Abs File -> App a
run parser path = do
  let filePath = toFilePath path
  fileContent <- readSourceFile path
  result <- runParserT (spaceConsumer >> parser) filePath fileContent
  case result of
    Right v ->
      return v
    Left errorBundle ->
      Throw.throw $ createParseError errorBundle

createParseError :: ParseErrorBundle T.Text Void -> E.Error
createParseError errorBundle = do
  let (foo, posState) = attachSourcePos errorOffset (bundleErrors errorBundle) (bundlePosState errorBundle)
  let hint = Hint.fromSourcePos $ pstateSourcePos posState
  let message = T.pack $ concatMap (parseErrorTextPretty . fst) $ toList foo
  E.newError hint message

getCurrentHint :: Parser Hint
getCurrentHint =
  Hint.fromSourcePos <$> getSourcePos

{-# INLINE spaceConsumer #-}
spaceConsumer :: Parser ()
spaceConsumer =
  L.space asciiSpaceOrNewLine1 (L.skipLineComment "//") empty

{-# INLINE isAsciiSpace #-}
isAsciiSpace :: Char -> Bool
isAsciiSpace c =
  c == ' '

{-# INLINE asciiSpaceOrNewLine1 #-}
asciiSpaceOrNewLine1 :: Parser ()
asciiSpaceOrNewLine1 =
  void $ takeWhile1P (Just "space or newline") isAsciiSpaceOrNewLine

{-# INLINE isAsciiSpaceOrNewLine #-}
isAsciiSpaceOrNewLine :: Char -> Bool
isAsciiSpaceOrNewLine c =
  c == ' ' || c == '\n'

{-# INLINE lexeme #-}
lexeme :: Parser a -> Parser a
lexeme =
  L.lexeme spaceConsumer

symbol :: Parser T.Text
symbol = do
  lexeme symbol'

symbol' :: Parser T.Text
symbol' = do
  takeWhile1P Nothing (`S.notMember` nonSymbolCharSet)

baseName :: Parser BN.BaseName
baseName = do
  lexeme $ do
    bn <- takeWhile1P Nothing (`S.notMember` nonBaseNameCharSet)
    return $ BN.fromText bn

baseNameCapitalized :: Parser BN.BaseName
baseNameCapitalized = do
  lexeme $ do
    c <- upperChar
    bn <- takeWhileP Nothing (`S.notMember` nonBaseNameCharSet)
    return $ BN.fromText $ T.singleton c <> bn

keyword :: T.Text -> Parser ()
keyword expected = do
  lexeme $ keyword' expected

keyword' :: T.Text -> Parser ()
keyword' expected = do
  try $ do
    _ <- chunk expected
    label (T.unpack expected) $ notFollowedBy symbol

nonSymbolChar :: Parser Char
nonSymbolChar =
  satisfy (`S.notMember` nonSymbolCharSet) <?> "non-symbol character"

delimiter :: T.Text -> Parser ()
delimiter = do
  lexeme . delimiter'

delimiter' :: T.Text -> Parser ()
delimiter' expected = do
  void $ chunk expected

string :: Parser T.Text
string =
  lexeme string'

string' :: Parser T.Text
string' = do
  _ <- char '\"'
  T.pack <$> manyTill L.charLiteral (char '\"')

integer :: Parser Integer
integer =
  lexeme integer'

integer' :: Parser Integer
integer' = do
  s <- symbol'
  case R.readMaybe (T.unpack s) of
    Just value ->
      return value
    Nothing ->
      failure (Just (asTokens s)) (S.fromList [asLabel "integer"])

float :: Parser Double
float =
  lexeme float'

float' :: Parser Double
float' = do
  s <- symbol'
  case R.readMaybe (T.unpack s) of
    Just value ->
      return value
    Nothing -> do
      failure (Just (asTokens s)) (S.fromList [asLabel "float"])

bool :: Parser Bool
bool = do
  s <- symbol
  case s of
    "true" ->
      return True
    "false" ->
      return False
    _ -> do
      failure (Just (asTokens s)) (S.fromList [asTokens "true", asTokens "false"])

betweenParen :: Parser a -> Parser a
betweenParen =
  lexeme . betweenParen'

betweenParen' :: Parser a -> Parser a
betweenParen' =
  between (delimiter "(") (delimiter' ")")

betweenBrace :: Parser a -> Parser a
betweenBrace =
  lexeme . betweenBrace'

betweenBrace' :: Parser a -> Parser a
betweenBrace' =
  between (delimiter "{") (delimiter' "}")

betweenBracket :: Parser a -> Parser a
betweenBracket =
  between (delimiter "[") (delimiter "]")

commaList :: Parser a -> Parser [a]
commaList f = do
  sepBy f (delimiter ",")

argList :: Parser a -> Parser [a]
argList = do
  lexeme . argList'

argList' :: Parser a -> Parser [a]
argList' f = do
  betweenParen' $ commaList f

impArgList :: Parser a -> Parser [a]
impArgList f =
  choice
    [ betweenBracket $ commaList f,
      return []
    ]

manyList :: Parser a -> Parser [a]
manyList f =
  many $ delimiter "-" >> f

argSeqOrList :: Parser a -> Parser [a]
argSeqOrList p =
  choice
    [ argList p,
      keyword "of" >> betweenBrace (manyList p)
    ]

var :: Parser (Hint, T.Text)
var = do
  lexeme var'

var' :: Parser (Hint, T.Text)
var' = do
  m <- getCurrentHint
  x <- symbol'
  if x /= "_"
    then return (m, x)
    else do
      unusedVar <- lift Gensym.newTextForHole
      return (m, unusedVar)

{-# INLINE nonSymbolCharSet #-}
nonSymbolCharSet :: S.Set Char
nonSymbolCharSet =
  S.fromList "=() \"\n\t:;,<>[]{}"

{-# INLINE nonBaseNameCharSet #-}
nonBaseNameCharSet :: S.Set Char
nonBaseNameCharSet =
  S.insert nsSepChar nonSymbolCharSet

asTokens :: T.Text -> ErrorItem Char
asTokens s =
  Tokens $ fromList $ T.unpack s

asLabel :: T.Text -> ErrorItem Char
asLabel s =
  Label $ fromList $ T.unpack s
