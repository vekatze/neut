module Main.Move.Scene.Parse.Core
  ( symbol,
    baseName,
    nonSymbolCharSet,
    keyword,
    string,
    rune,
    integer,
    float,
    bool,
    betweenParen,
    betweenBrace,
    betweenBrace',
  )
where

import BaseParser.Move.GetInfo
import BaseParser.Move.Parse
import BaseParser.Rule.Parser
import Data.Set qualified as S
import Data.Text qualified as T
import Language.Common.Rule.BaseName qualified as BN
import Language.Common.Rule.Const
import Main.Rule.Syntax.Block
import SyntaxTree.Rule.C
import SyntaxTree.Rule.Series qualified as SE
import Text.Megaparsec
import Text.Megaparsec.Char hiding (string)
import Text.Read qualified as R

symbol :: Parser (T.Text, C)
symbol = do
  lexeme $ takeWhile1P Nothing (`S.notMember` nonSymbolCharSet)

baseName :: Parser (BN.BaseName, C)
baseName = do
  lexeme $ do
    bn <- takeWhile1P Nothing (`S.notMember` nonBaseNameCharSet)
    return $ BN.fromText bn

keyword :: T.Text -> Parser C
keyword expected = do
  fmap snd $ lexeme $ try $ do
    _ <- chunk expected
    label (T.unpack expected) $ notFollowedBy symbol

string :: Parser (T.Text, C)
string =
  lexeme $ do
    _ <- char '"'
    stringInner []

stringInner :: [Char] -> Parser T.Text
stringInner acc = do
  c <- anySingle
  case c of
    '\\' -> do
      c' <- anySingle
      stringInner (c' : '\\' : acc)
    '"' ->
      return $ T.pack $ Prelude.reverse acc
    _ ->
      stringInner (c : acc)

rune :: Parser (T.Text, C)
rune =
  lexeme $ do
    _ <- char '`'
    runeInner []

runeInner :: [Char] -> Parser T.Text
runeInner acc = do
  c <- anySingle
  case c of
    '\\' -> do
      c' <- anySingle
      if c' == '`'
        then runeInner (c' : acc)
        else runeInner (c' : '\\' : acc)
    '`' ->
      return $ T.pack $ Prelude.reverse acc
    _ ->
      runeInner (c : acc)

integer :: Parser (Integer, C)
integer = do
  (s, c) <- symbol
  case R.readMaybe (T.unpack s) of
    Just value ->
      return (value, c)
    Nothing ->
      failure (Just (asTokens s)) (S.fromList [asLabel "integer"])

float :: Parser (Double, C)
float = do
  (s, c) <- symbol
  case R.readMaybe (T.unpack s) of
    Just value ->
      return (value, c)
    Nothing -> do
      failure (Just (asTokens s)) (S.fromList [asLabel "float"])

bool :: Parser (Bool, C)
bool = do
  (s, c) <- symbol
  case s of
    "true" ->
      return (True, c)
    "false" ->
      return (False, c)
    _ -> do
      failure (Just (asTokens s)) (S.fromList [asTokens "true", asTokens "false"])

betweenParen :: Parser a -> Parser (C, (a, C))
betweenParen p = do
  c1 <- delimiter "("
  v <- p
  c2 <- delimiter ")"
  return (c1, (v, c2))

betweenBrace :: Parser a -> Parser (C, (a, C))
betweenBrace p = do
  c1 <- delimiter "{"
  v <- p
  c2 <- delimiter "}"
  return (c1, (v, c2))

betweenBrace' :: Parser a -> Parser (Block' a)
betweenBrace' p = do
  c1 <- delimiter "{"
  v <- p
  loc <- getCurrentLoc
  c2 <- delimiter "}"
  return (c1, (v, loc, c2))

_series :: Maybe SE.Container -> SE.Separator -> C -> Parser (a, C) -> Parser (SE.Series a)
_series container separator leadingComment p = do
  case separator of
    SE.Comma -> do
      _seriesAnd container leadingComment p
    SE.Bar -> do
      _seriesOr container leadingComment p

_seriesAnd :: Maybe SE.Container -> C -> Parser (a, C) -> Parser (SE.Series a)
_seriesAnd container leadingComment p = do
  _seriesSepTrail False container SE.Comma leadingComment p

_seriesOr :: Maybe SE.Container -> C -> Parser (a, C) -> Parser (SE.Series a)
_seriesOr container leadingComment p = do
  _seriesSepLead container SE.Bar leadingComment p

_seriesSepTrail :: Bool -> Maybe SE.Container -> SE.Separator -> C -> Parser (a, C) -> Parser (SE.Series a)
_seriesSepTrail afterComma container separator leadingComment p = do
  choice
    [ _seriesSepTrail1 container separator leadingComment p,
      return
        (SE.emptySeries container separator)
          { SE.hasOptionalSeparator = afterComma,
            SE.trailingComment = leadingComment
          }
    ]

_seriesSepTrail1 :: Maybe SE.Container -> SE.Separator -> C -> Parser (a, C) -> Parser (SE.Series a)
_seriesSepTrail1 container separator leadingComment p = do
  (v, c) <- p
  choice
    [ do
        cSep <- delimiter $ SE.getSeparator separator
        rest <- _seriesSepTrail True container separator (c ++ cSep) p
        return $ SE.cons (leadingComment, v) rest,
      do
        return $ SE.singleton container separator leadingComment (v, c)
    ]

_seriesSepLead :: Maybe SE.Container -> SE.Separator -> C -> Parser (a, C) -> Parser (SE.Series a)
_seriesSepLead container separator leadingComment p = do
  choice
    [ do
        cSep <- delimiter $ SE.getSeparator separator
        se <- _seriesSepBy1 container separator (leadingComment ++ cSep) p
        return se {SE.hasOptionalSeparator = True},
      _seriesSepBy container separator leadingComment p
    ]

_seriesSepBy :: Maybe SE.Container -> SE.Separator -> C -> Parser (a, C) -> Parser (SE.Series a)
_seriesSepBy container separator leadingComment p = do
  choice
    [ _seriesSepBy1 container separator leadingComment p,
      return $ SE.pushComment leadingComment $ SE.emptySeries container separator
    ]

_seriesSepBy1 :: Maybe SE.Container -> SE.Separator -> C -> Parser (a, C) -> Parser (SE.Series a)
_seriesSepBy1 container separator leadingComment p = do
  v <- p
  elems <- many $ do
    cSep <- delimiter $ SE.getSeparator separator
    val <- p
    return (cSep, val)
  return $ SE.fromListWithComment container separator $ (leadingComment, v) : elems

{-# INLINE nonBaseNameCharSet #-}
nonBaseNameCharSet :: S.Set Char
nonBaseNameCharSet =
  S.insert nsSepChar nonSymbolCharSet
