module Scene.Parse.Core where

import Context.App
import Context.Gensym qualified as Gensym
import Context.Throw qualified as Throw
import Control.Monad
import Control.Monad.Trans
import Data.List.NonEmpty
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Void
import Entity.BaseName qualified as BN
import Entity.C
import Entity.Const
import Entity.Error qualified as E
import Entity.Hint
import Entity.Hint.Reflect qualified as Hint
import Entity.Syntax.Block
import Entity.Syntax.Series qualified as SE
import Path
import Text.Megaparsec
import Text.Megaparsec.Char hiding (string)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Read qualified as R

type Parser = ParsecT Void T.Text App

type MustParseWholeFile =
  Bool

parseFile :: MustParseWholeFile -> Parser a -> Path Abs File -> T.Text -> App (C, a)
parseFile mustParseWholeFile parser path fileContent = do
  let fileParser = do
        leadingComments <- spaceConsumer
        value <- parser
        when mustParseWholeFile eof
        return (leadingComments, value)
  let filePath = toFilePath path
  result <- runParserT fileParser filePath fileContent
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

{-# INLINE isAsciiSpace #-}
isAsciiSpace :: Char -> Bool
isAsciiSpace c =
  c == ' '

{-# INLINE asciiSpaceOrNewLine1 #-}
asciiSpaceOrNewLine1 :: Parser ()
asciiSpaceOrNewLine1 =
  void $ takeWhile1P (Just "space or newline") isAsciiSpaceOrNewLine

{-# INLINE asciiSpace1 #-}
asciiSpace1 :: Parser ()
asciiSpace1 =
  void $ takeWhile1P (Just "space") isAsciiSpaceOrNewLine

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

nonSymbolChar :: Parser Char
nonSymbolChar =
  satisfy (`S.notMember` nonSymbolCharSet) <?> "non-symbol character"

delimiter :: T.Text -> Parser C
delimiter expected = do
  fmap snd $ lexeme $ void $ chunk expected

string :: Parser (T.Text, C)
string =
  lexeme $ do
    _ <- char '\"'
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
  c2 <- delimiter "}"
  loc <- getCurrentLoc
  return (c1, (v, loc, c2))

sepList :: Parser c -> Parser c -> Parser a -> Parser [(c, a)]
sepList first sep f = do
  c <- first
  mv <- optional f
  case mv of
    Nothing ->
      return []
    Just v -> do
      rest <- many $ do
        c' <- sep
        v' <- f
        return (c', v')
      return $ (c, v) : rest

commaList :: Parser C -> Parser a -> Parser [(C, a)]
commaList first f = do
  sepList first (delimiter ",") f

_series :: C -> SE.Separator -> Parser (a, C) -> Parser ([(C, a)], C)
_series leadingComment sep p = do
  case sep of
    SE.Comma -> do
      mv <- optional p
      case mv of
        Nothing ->
          return ([], leadingComment)
        Just (v, c) -> do
          choice
            [ do
                cComma <- delimiter $ SE.getSeparator sep
                (vs, trailingComment') <- _series (c ++ cComma) sep p
                return ((leadingComment, v) : vs, trailingComment'),
              do
                return ([(leadingComment, v)], c)
            ]
    SE.Hyphen -> do
      choice
        [ do
            cHyphen <- delimiter $ SE.getSeparator sep
            (v, c) <- p
            (vs, trailingComment) <- _series c sep p
            return ((leadingComment ++ cHyphen, v) : vs, trailingComment),
          do
            return ([], leadingComment)
        ]

series :: SE.Prefix -> SE.Container -> SE.Separator -> Parser (a, C) -> Parser (SE.Series a, C)
series prefix container sep p = do
  let (open, close) = SE.getContainerPair container
  c1 <- delimiter open
  (vs, trail) <- _series c1 sep p
  c2 <- delimiter close
  return
    ( SE.Series
        { elems = vs,
          trailingComment = trail,
          prefix = prefix,
          separator = sep,
          container = Just container
        },
      c2
    )

series' :: SE.Prefix -> SE.Container -> SE.Separator -> Parser (a, C) -> Parser (SE.Series a, C)
series' prefix container sep p = do
  let (opener, closer) = getParserPair container
  c1 <- opener
  (vs, trail) <- _series c1 sep p
  c2 <- closer
  return
    ( SE.Series
        { elems = vs,
          trailingComment = trail,
          prefix = prefix,
          separator = sep,
          container = Just container
        },
      c2
    )

bareSeries :: SE.Prefix -> SE.Separator -> Parser (a, C) -> Parser (SE.Series a)
bareSeries prefix sep p = do
  (vs, trail) <- _series [] sep p
  return $
    SE.Series
      { elems = vs,
        trailingComment = trail,
        prefix = prefix,
        separator = sep,
        container = Nothing
      }

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

seriesBrace :: Parser (a, C) -> Parser (SE.Series a, C)
seriesBrace =
  series Nothing SE.Brace SE.Comma

seriesBracket :: Parser (a, C) -> Parser (SE.Series a, C)
seriesBracket =
  series Nothing SE.Bracket SE.Comma

seriesAngle :: Parser (a, C) -> Parser (SE.Series a, C)
seriesAngle =
  series Nothing SE.Angle SE.Comma

seriesBraceList :: Parser (a, C) -> Parser (SE.Series a, C)
seriesBraceList =
  series Nothing SE.Brace SE.Hyphen

seqOrList :: Parser (a, C) -> Parser (SE.Series a, C)
seqOrList p =
  choice
    [ do
        seriesParen p,
      do
        c1 <- keyword "of"
        series (Just ("of", c1)) SE.Brace SE.Hyphen p
    ]

var :: Parser ((Hint, T.Text), C)
var = do
  m <- getCurrentHint
  (x, c) <- symbol
  if x /= "_"
    then return ((m, x), c)
    else do
      unusedVar <- lift Gensym.newTextForHole
      return ((m, unusedVar), c)

{-# INLINE nonSymbolCharSet #-}
nonSymbolCharSet :: S.Set Char
nonSymbolCharSet =
  S.fromList "=() \"\n\t:;,<>[]{}/*"

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
