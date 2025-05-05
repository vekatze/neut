module BaseParser.Move.Parse
  ( spaceConsumer,
    lexeme,
    delimiter,
  )
where

import BaseParser.Rule.Parser
import Control.Monad
import Data.Text qualified as T
import SyntaxTree.Rule.C
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer qualified as L

skipSpace :: Parser ()
skipSpace =
  L.space asciiSpaceOrNewLine1 empty empty

comment :: Parser T.Text
comment = do
  skipSpace
  chunk "//"
  takeWhileP (Just "character") (/= '\n')

{-# INLINE spaceConsumer #-}
spaceConsumer :: Parser C
spaceConsumer =
  hidden $ do
    skipSpace
    many (comment <* skipSpace)

{-# INLINE asciiSpaceOrNewLine1 #-}
asciiSpaceOrNewLine1 :: Parser ()
asciiSpaceOrNewLine1 =
  void $ takeWhile1P (Just "space or newline") isAsciiSpaceOrNewLine

{-# INLINE isAsciiSpaceOrNewLine #-}
isAsciiSpaceOrNewLine :: Char -> Bool
isAsciiSpaceOrNewLine c =
  c == ' ' || c == '\n'

{-# INLINE lexeme #-}
lexeme :: Parser a -> Parser (a, C)
lexeme p = do
  v <- p
  c <- spaceConsumer
  return (v, c)

delimiter :: T.Text -> Parser C
delimiter expected = do
  fmap snd $ lexeme $ void $ chunk expected
