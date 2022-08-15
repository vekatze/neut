module Scene.Parse.Core where

import qualified Context.Gensym as Gensym
import qualified Context.Locator as Locator
import qualified Context.Throw as Throw
import Control.Monad
import Data.List.NonEmpty
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Void
import qualified Entity.BaseName as BN
import Entity.Const
import Entity.FilePos
import Entity.Hint
import qualified Entity.Hint.Reflect as Hint
import Entity.Log
import Entity.TargetPlatform
import Path
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Read as R

type Parser m = ParsecT Void T.Text m

class (Throw.Context m, Gensym.Context m, Locator.Context m) => Context m where
  getTargetPlatform :: m TargetPlatform
  readSourceFile :: Path Abs File -> m T.Text
  ensureExistence :: Path Abs File -> m ()

run :: (Throw.Context m, Context m) => Parser m a -> Path Abs File -> m a
run parser path = do
  ensureExistence path
  let filePath = toFilePath path
  fileContent <- readSourceFile path
  result <- runParserT (spaceConsumer >> parser) filePath fileContent
  case result of
    Right v ->
      return v
    Left errorBundle ->
      Throw.throw $ createParseError errorBundle

createParseError :: ParseErrorBundle T.Text Void -> Error
createParseError errorBundle = do
  let (foo, posState) = attachSourcePos errorOffset (bundleErrors errorBundle) (bundlePosState errorBundle)
  let hint = Hint.fromSourcePos $ pstateSourcePos posState
  let message = T.pack $ concatMap (parseErrorTextPretty . fst) $ toList foo
  Error [logError (fromHint hint) message]

getCurrentHint :: Parser m Hint
getCurrentHint =
  Hint.fromSourcePos <$> getSourcePos

spaceConsumer :: Parser m ()
spaceConsumer =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/-" "-/")

lexeme :: Context m => Parser m a -> Parser m a
lexeme =
  L.lexeme spaceConsumer

-- symbol :: Context m => Parser m a
symbol :: Context m => Parser m T.Text
symbol = do
  lexeme $ takeWhile1P Nothing (`S.notMember` nonSymbolCharSet)

baseName :: Context m => Parser m BN.BaseName
baseName = do
  bn <- takeWhile1P Nothing (`S.notMember` nonBaseNameCharSet)
  lexeme $ return $ BN.fromText bn

keyword :: Context m => T.Text -> Parser m ()
keyword expected = do
  void $ chunk expected
  notFollowedBy nonSymbolChar
  spaceConsumer

delimiter :: Context m => T.Text -> Parser m ()
delimiter expected = do
  lexeme $ void $ chunk expected

nonSymbolChar :: Context m => Parser m Char
nonSymbolChar =
  satisfy (`S.notMember` nonSymbolCharSet) <?> "non-symbol character"

string :: Context m => Parser m T.Text
string = do
  lexeme $ do
    _ <- char '\"'
    T.pack <$> manyTill L.charLiteral (char '\"')

integer :: Context m => Parser m Integer
integer = do
  s <- symbol
  case R.readMaybe (T.unpack s) of
    Just value ->
      return value
    Nothing ->
      failure (Just (asTokens s)) (S.fromList [asLabel "integer"])

float :: Context m => Parser m Double
float = do
  s <- symbol
  case R.readMaybe (T.unpack s) of
    Just value ->
      return value
    Nothing -> do
      failure (Just (asTokens s)) (S.fromList [asLabel "float"])

bool :: Context m => Parser m Bool
bool = do
  s <- symbol
  case s of
    "true" ->
      return True
    "false" ->
      return False
    _ -> do
      failure (Just (asTokens s)) (S.fromList [asTokens "true", asTokens "false"])

betweenParen :: Context m => Parser m a -> Parser m a
betweenParen =
  between (delimiter "(") (delimiter ")")

betweenAngle :: Context m => Parser m a -> Parser m a
betweenAngle =
  between (delimiter "<") (delimiter ">")

betweenBracket :: Context m => Parser m a -> Parser m a
betweenBracket =
  between (delimiter "[") (delimiter "]")

asBlock :: Context m => Parser m a -> Parser m a
asBlock =
  between (keyword "as") (keyword "end")

doBlock :: Context m => Parser m a -> Parser m a
doBlock =
  between (keyword "do") (keyword "end")

withBlock :: Context m => Parser m a -> Parser m a
withBlock =
  between (keyword "with") (keyword "end")

importBlock :: Context m => Parser m a -> Parser m a
importBlock =
  between (keyword "import") (keyword "end")

argList :: Context m => Parser m a -> Parser m [a]
argList f = do
  betweenParen $ sepBy f (delimiter ",")

impArgList :: Context m => Parser m a -> Parser m [a]
impArgList f =
  choice
    [ betweenAngle $ sepBy f (delimiter ","),
      return []
    ]

manyList :: Context m => Parser m a -> Parser m [a]
manyList f =
  many $ delimiter "-" >> f

var :: Context m => Parser m (Hint, T.Text)
var = do
  m <- getCurrentHint
  x <- symbol
  return (m, x)

{-# INLINE nonSymbolCharSet #-}
nonSymbolCharSet :: S.Set Char
nonSymbolCharSet =
  S.fromList "() \"\n\t:;,!?<>[]{}"

{-# INLINE nonBaseNameCharSet #-}
nonBaseNameCharSet :: S.Set Char
nonBaseNameCharSet =
  S.insert nsSepChar nonSymbolCharSet

{-# INLINE spaceCharSet #-}
spaceCharSet :: S.Set Char
spaceCharSet =
  S.fromList " \n\t"

asTokens :: T.Text -> ErrorItem Char
asTokens s =
  Tokens $ fromList $ T.unpack s

asLabel :: T.Text -> ErrorItem Char
asLabel s =
  Tokens $ fromList $ T.unpack s
