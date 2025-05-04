module Main.Move.Scene.Parse.Core
  ( Handle (..),
    Parser,
    new,
    parseFile,
    getCurrentHint,
    getCurrentLoc,
    symbol,
    baseName,
    delimiter,
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
    series,
    bareSeries,
    seriesParen,
    seriesParen',
    seriesBrace,
    seriesBrace',
    seriesBracket,
    seriesAngle,
    seriesBraceList,
    seriesBraceList',
    asLabel,
    var,
  )
where

import Control.Monad
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Trans
import Data.List.NonEmpty
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Void
import Language.Common.Rule.BaseName qualified as BN
import Language.Common.Rule.Const
import Language.Common.Rule.Error qualified as E
import Language.Common.Rule.Hint
import Language.Common.Rule.Hint.Reflect qualified as Hint
import Language.RawTerm.Rule.C
import Language.RawTerm.Rule.Syntax.Series qualified as SE
import Main.Move.Context.EIO
import Main.Move.Context.Gensym (newTextForHole)
import Main.Move.Context.Gensym qualified as Gensym
import Main.Rule.Syntax.Block
import Path
import Text.Megaparsec
import Text.Megaparsec.Char hiding (string)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Read qualified as R

type MustParseWholeFile =
  Bool

newtype Handle = Handle
  { gensymHandle :: Gensym.Handle
  }

type Parser a = ParsecT Void T.Text EIO a

new :: Gensym.Handle -> Handle
new gensymHandle = do
  Handle {..}

parseFile :: Handle -> Path Abs File -> T.Text -> MustParseWholeFile -> (Handle -> Parser a) -> EIO (C, a)
parseFile h filePath fileContent mustParseWholeFile parser = do
  let fileParser = do
        leadingComments <- spaceConsumer
        value <- parser h
        when mustParseWholeFile eof
        return (leadingComments, value)
  let path = toFilePath filePath
  result <- runParserT fileParser path fileContent
  case result of
    Right v ->
      return v
    Left errorBundle ->
      throwError $ createParseError errorBundle

createParseError :: ParseErrorBundle T.Text Void -> E.Error
createParseError errorBundle = do
  let (foo, posState) = attachSourcePos errorOffset (bundleErrors errorBundle) (bundlePosState errorBundle)
  let hint = Hint.fromSourcePos $ pstateSourcePos posState
  let message = T.pack $ concatMap (parseErrorTextPretty . fst) $ toList foo
  E.newError hint message

getCurrentHint :: Parser Hint
getCurrentHint =
  Hint.fromSourcePos <$> getSourcePos

getCurrentLoc :: Parser Loc
getCurrentLoc = do
  pos <- getSourcePos
  let line = unPos $ sourceLine pos
  let column = unPos $ sourceColumn pos
  return (line, column)

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
  c <- spaceConsumer -- read spaces *before* p
  return (v, c)

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

delimiter :: T.Text -> Parser C
delimiter expected = do
  fmap snd $ lexeme $ void $ chunk expected

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

var :: Handle -> Parser ((Hint, T.Text), C)
var h = do
  m <- getCurrentHint
  (x, c) <- symbol
  if x /= "_"
    then return ((m, x), c)
    else do
      unusedVar <- liftIO $ newTextForHole (gensymHandle h)
      return ((m, unusedVar), c)

{-# INLINE nonSymbolCharSet #-}
nonSymbolCharSet :: S.Set Char
nonSymbolCharSet =
  S.fromList "=() `\"\n\t:;,<>[]{}/*|"

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
