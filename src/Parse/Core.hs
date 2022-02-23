module Parse.Core where

import Control.Exception.Safe (throw)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Basic
  ( Hint,
    getPosInfo,
    newHint,
  )
import Data.Global (setCurrentFilePath)
import Data.List.NonEmpty (fromList, toList)
import Data.Log (Error (..), logError)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import Path (Abs, File, Path, toFilePath)
import Text.Megaparsec
  ( ErrorItem (Tokens),
    MonadParsec (notFollowedBy),
    ParseErrorBundle (bundleErrors, bundlePosState),
    ParsecT,
    PosState (pstateSourcePos),
    SourcePos (sourceColumn, sourceLine, sourceName),
    between,
    choice,
    chunk,
    errorOffset,
    failure,
    getSourcePos,
    many,
    manyTill,
    parseErrorTextPretty,
    runParserT,
    satisfy,
    sepBy,
    takeWhile1P,
    unPos,
    (<?>),
  )
import Text.Megaparsec.Char (char, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (attachSourcePos)
import Text.Read (readMaybe)

type Parser = ParsecT Void T.Text IO

run :: Parser a -> Path Abs File -> IO a
run parser path = do
  setCurrentFilePath path
  let filePath = toFilePath path
  fileContent <- TIO.readFile filePath
  result <- runParserT (spaceConsumer >> parser) filePath fileContent
  case result of
    Right v ->
      return v
    Left errorBundle ->
      throw $ createParseError errorBundle

createParseError :: ParseErrorBundle T.Text Void -> Error
createParseError errorBundle = do
  let (foo, posState) = attachSourcePos errorOffset (bundleErrors errorBundle) (bundlePosState errorBundle)
  let hint = sourcePosToHint $ pstateSourcePos posState
  let message = T.pack $ concatMap (parseErrorTextPretty . fst) $ toList foo
  Error [logError (getPosInfo hint) message]

sourcePosToHint :: SourcePos -> Hint
sourcePosToHint pos = do
  let line = unPos $ sourceLine pos
  let column = unPos $ sourceColumn pos
  let file = sourceName pos
  newHint line column file

currentHint :: Parser Hint
currentHint =
  sourcePosToHint <$> getSourcePos

spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/-" "-/")

lexeme :: Parser a -> Parser a
lexeme =
  L.lexeme spaceConsumer

symbol :: Parser T.Text
symbol = do
  lexeme $ takeWhile1P Nothing (`S.notMember` nonSymbolCharSet)

keyword :: T.Text -> Parser ()
keyword expected = do
  void $ chunk expected
  notFollowedBy nonSymbolChar
  spaceConsumer

delimiter :: T.Text -> Parser ()
delimiter expected = do
  lexeme $ void $ chunk expected

nonSymbolChar :: Parser Char
nonSymbolChar =
  satisfy (`S.notMember` nonSymbolCharSet) <?> "non-symbol character"

asTokens :: T.Text -> ErrorItem Char
asTokens s =
  Tokens $ fromList $ T.unpack s

asLabel :: T.Text -> ErrorItem Char
asLabel s =
  Tokens $ fromList $ T.unpack s

string :: Parser T.Text
string = do
  lexeme $ do
    _ <- char '\"'
    T.pack <$> manyTill L.charLiteral (char '\"')

integer :: Parser Integer
integer = do
  s <- symbol
  case readMaybe (T.unpack s) of
    Just value ->
      return value
    Nothing ->
      failure (Just (asTokens s)) (S.fromList [asLabel "integer"])

float :: Parser Double
float = do
  s <- symbol
  case readMaybe (T.unpack s) of
    Just value ->
      return value
    Nothing ->
      failure (Just (asTokens s)) (S.fromList [asLabel "float"])

bool :: Parser Bool
bool = do
  s <- symbol
  case s of
    "true" ->
      return True
    "false" ->
      return False
    _ ->
      failure (Just (asTokens s)) (S.fromList [asTokens "true", asTokens "false"])

betweenParen :: Parser a -> Parser a
betweenParen =
  between (delimiter "(") (delimiter ")")

betweenAngle :: Parser a -> Parser a
betweenAngle =
  between (delimiter "<") (delimiter ">")

betweenBracket :: Parser a -> Parser a
betweenBracket =
  between (delimiter "[") (delimiter "]")

asBlock :: Parser a -> Parser a
asBlock =
  between (keyword "as") (keyword "end")

doBlock :: Parser a -> Parser a
doBlock =
  between (keyword "do") (keyword "end")

withBlock :: Parser a -> Parser a
withBlock =
  between (keyword "with") (keyword "end")

importBlock :: Parser a -> Parser a
importBlock =
  between (keyword "import") (keyword "end")

argList :: Parser a -> Parser [a]
argList f = do
  betweenParen $ sepBy f (delimiter ",")

impArgList :: Parser a -> Parser [a]
impArgList f =
  choice
    [ betweenAngle $ sepBy f (delimiter ","),
      return []
    ]

manyList :: Parser a -> Parser [a]
manyList f =
  many $ delimiter "-" >> f

var :: Parser (Hint, T.Text)
var = do
  m <- currentHint
  x <- symbol
  return (m, x)

{-# INLINE nonSymbolCharSet #-}
nonSymbolCharSet :: S.Set Char
nonSymbolCharSet =
  S.fromList "() \"\n\t:;,!?<>[]{}"

{-# INLINE spaceCharSet #-}
spaceCharSet :: S.Set Char
spaceCharSet =
  S.fromList " \n\t"

p :: (Show a) => a -> Parser ()
p = liftIO . print
