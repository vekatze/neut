module Aux.SyntaxTree.Move.ParseSeries
  ( series,
    bareSeries,
    seriesParen,
    seriesParen',
    seriesBrace,
    seriesBrace',
    seriesBracket,
    seriesAngle,
    seriesBraceList,
    seriesBraceList',
  )
where

import Aux.CodeParser.Move.GetInfo
import Aux.CodeParser.Move.Parse
import Aux.CodeParser.Rule.Parser
import Aux.Logger.Rule.Hint
import Aux.SyntaxTree.Rule.C
import Aux.SyntaxTree.Rule.Series qualified as SE
import Text.Megaparsec

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

series :: SE.Prefix -> SE.Container -> SE.Separator -> Parser (a, C) -> Parser (SE.Series a, C)
series prefix container separator p = do
  let (open, close) = SE.getContainerPair container
  c1 <- delimiter open
  se <- _series (Just container) separator c1 p
  c2 <- delimiter close
  return (se {SE.prefix = prefix}, c2)

series' :: SE.Prefix -> SE.Container -> SE.Separator -> Parser (a, C) -> Parser (SE.Series a, Loc, C)
series' prefix container separator p = do
  let (opener, closer) = getParserPair container
  c1 <- opener
  se <- _series (Just container) separator c1 p
  loc <- getCurrentLoc
  c2 <- closer
  return (se {SE.prefix = prefix}, loc, c2)

bareSeries :: SE.Prefix -> SE.Separator -> Parser (a, C) -> Parser (SE.Series a)
bareSeries prefix separator p = do
  se <- _series Nothing separator [] p
  return se {SE.prefix = prefix}

getParserPair :: SE.Container -> (Parser C, Parser C)
getParserPair container =
  case container of
    SE.Paren ->
      (delimiter "(", delimiter ")")
    SE.Brace ->
      (delimiter "{", delimiter "}")
    SE.Bracket ->
      (delimiter "[", delimiter "]")
    SE.Angle ->
      (delimiter "<", delimiter ">")

seriesParen :: Parser (a, C) -> Parser (SE.Series a, C)
seriesParen =
  series Nothing SE.Paren SE.Comma

seriesParen' :: Parser (a, C) -> Parser (SE.Series a, Loc, C)
seriesParen' =
  series' Nothing SE.Paren SE.Comma

seriesBrace :: Parser (a, C) -> Parser (SE.Series a, C)
seriesBrace =
  series Nothing SE.Brace SE.Comma

seriesBrace' :: Parser (a, C) -> Parser (SE.Series a, Loc, C)
seriesBrace' =
  series' Nothing SE.Brace SE.Comma

seriesBracket :: Parser (a, C) -> Parser (SE.Series a, C)
seriesBracket =
  series Nothing SE.Bracket SE.Comma

seriesAngle :: Parser (a, C) -> Parser (SE.Series a, C)
seriesAngle =
  series Nothing SE.Angle SE.Comma

seriesBraceList :: Parser (a, C) -> Parser (SE.Series a, C)
seriesBraceList =
  series Nothing SE.Brace SE.Bar

seriesBraceList' :: Parser (a, C) -> Parser (SE.Series a, Loc, C)
seriesBraceList' =
  series' Nothing SE.Brace SE.Bar
