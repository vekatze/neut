module Scene.Parse.Tree
  ( parseFile,
    parseFileHeadTree,
    parseTree,
  )
where

import Context.App
import Context.Parse
import Context.Remark (printNote')
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Trans
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Void
import Entity.Atom qualified as AT
import Entity.Error qualified as E
import Entity.Hint
import Entity.Hint.Reflect qualified as Hint
import Entity.Tree hiding (chunk)
import Path
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = ParsecT Void T.Text App

parseSymbol :: Parser Tree
parseSymbol = do
  m <- getCurrentHint
  s <- atom
  return $ m :< Atom (AT.Symbol s)

parseString :: Parser Tree
parseString = do
  m <- getCurrentHint
  s <- string
  return $ m :< Atom (AT.String s)

parseNode :: Parser Tree
parseNode = do
  m <- getCurrentHint
  ts <- betweenParen $ many parseTree
  return $ m :< Node ts

parseList :: Parser Tree
parseList = do
  m <- getCurrentHint
  ts <- betweenBracket $ sepBy (some parseTree) (delimiter ",")
  return $ m :< List ts

parseTree :: Parser Tree
parseTree =
  choice
    [ parseNode,
      parseList,
      parseString,
      parseSymbol
    ]

parseTreeList :: Parser TreeList
parseTreeList = do
  m <- getCurrentHint
  ts <- many parseTree
  return (m, ts)

parseFile :: Path Abs File -> App TreeList
parseFile path = do
  run parseTreeList path

parseFileHeadTree :: Path Abs File -> App Tree
parseFileHeadTree path = do
  run parseTree path

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
  let message = T.pack $ concatMap (parseErrorTextPretty . fst) $ NE.toList foo
  E.newError hint message

atom :: Parser T.Text
atom = do
  lexeme $ takeWhile1P Nothing (`S.notMember` nonAtomCharSet)

betweenParen :: Parser a -> Parser a
betweenParen =
  between (delimiter "(") (delimiter ")")

betweenBracket :: Parser a -> Parser a
betweenBracket =
  between (delimiter "[") (delimiter "]")

getCurrentHint :: Parser Hint
getCurrentHint =
  Hint.fromSourcePos <$> getSourcePos

{-# INLINE isAsciiSpaceOrNewLine #-}
isAsciiSpaceOrNewLine :: Char -> Bool
isAsciiSpaceOrNewLine c =
  c == ' ' || c == '\n'

delimiter :: T.Text -> Parser ()
delimiter expected = do
  lexeme $ void $ chunk expected

{-# INLINE spaceConsumer #-}
spaceConsumer :: Parser ()
spaceConsumer =
  L.space asciiSpaceOrNewLine1 (L.skipLineComment ";") empty

{-# INLINE asciiSpaceOrNewLine1 #-}
asciiSpaceOrNewLine1 :: Parser ()
asciiSpaceOrNewLine1 =
  void $ takeWhile1P (Just "space or newline") isAsciiSpaceOrNewLine

string :: Parser T.Text
string =
  lexeme $ do
    _ <- char '\"'
    T.pack <$> manyTill L.charLiteral (char '\"')

{-# INLINE lexeme #-}
lexeme :: Parser a -> Parser a
lexeme =
  L.lexeme spaceConsumer

{-# INLINE nonAtomCharSet #-}
nonAtomCharSet :: S.Set Char
nonAtomCharSet =
  S.fromList "()[]{} \"\n\t,"
