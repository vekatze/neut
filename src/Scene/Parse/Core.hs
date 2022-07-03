module Scene.Parse.Core where

import Context.Throw
import Control.Monad
import Control.Monad.IO.Class
import Data.List.NonEmpty
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import Entity.FilePos
import Entity.Hint
import qualified Entity.Hint.Reflect as Hint
import Entity.Log
import Path
import Path.IO
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Read as R

type Parser = ParsecT Void T.Text IO

run :: Context -> Parser a -> Path Abs File -> IO a
run ctx parser path = do
  fileExists <- doesFileExist path
  unless fileExists $ do
    raiseError' ctx $ T.pack $ "no such file exists: " <> toFilePath path
  let filePath = toFilePath path
  fileContent <- TIO.readFile filePath
  result <- runParserT (spaceConsumer >> parser) filePath fileContent
  case result of
    Right v ->
      return v
    Left errorBundle ->
      throw ctx $ createParseError errorBundle

createParseError :: ParseErrorBundle T.Text Void -> Error
createParseError errorBundle = do
  let (foo, posState) = attachSourcePos errorOffset (bundleErrors errorBundle) (bundlePosState errorBundle)
  let hint = Hint.fromSourcePos $ pstateSourcePos posState
  let message = T.pack $ concatMap (parseErrorTextPretty . fst) $ toList foo
  Error [logError (fromHint hint) message]

currentHint :: Parser Hint
currentHint =
  Hint.fromSourcePos <$> getSourcePos

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
  case R.readMaybe (T.unpack s) of
    Just value ->
      return value
    Nothing ->
      failure (Just (asTokens s)) (S.fromList [asLabel "integer"])

float :: Parser Double
float = do
  s <- symbol
  case R.readMaybe (T.unpack s) of
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
