module Scene.Parse.Core where

import qualified Context.Gensym as Gensym
import qualified Context.Throw as Throw
import Control.Monad
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Entity.BaseName as BN
import Entity.Const
import Entity.Hint
import Entity.TargetPlatform
import Path
import qualified Text.Read as R

class (Throw.Context m, Gensym.Context m) => Context m where
  type ErrorItem m :: * -> *
  run :: m a -> Path Abs File -> m a
  getCurrentHint :: m Hint
  getTargetPlatform :: m TargetPlatform
  spaceConsumer :: m ()
  baseLexeme :: m () -> m a -> m a
  takeWhile1P :: (Char -> Bool) -> m T.Text
  chunk :: T.Text -> m T.Text
  satisfy :: (Char -> Bool) -> m Char
  notFollowedBy :: m a -> m ()
  (<?>) :: m a -> String -> m a
  failure :: Maybe (ErrorItem m Char) -> [ErrorItem m Char] -> m a
  asTokens :: T.Text -> m (ErrorItem m Char)
  asLabel :: T.Text -> m (ErrorItem m Char)
  charLiteral :: m Char
  try :: m a -> m a
  char :: Char -> m ()
  manyTill :: m a -> m end -> m [a]
  between :: m () -> m () -> m a -> m a
  sepBy :: m a -> m sep -> m [a]
  many :: m a -> m [a]
  choice :: [m a] -> m a
  eof :: m ()

-- class (Monad m, Monad inner) => NestedMonad inner m | m -> inner where
--   run :: inner a -> m a
--   mySepBy :: m a -> m sep -> m [a]

-- run :: (MonadIO m, Throw.Context m) => Parser m a -> Path Abs File -> m a
-- run parser path = do
--   fileExists <- doesFileExist path
--   unless fileExists $ do
--     Throw.raiseError' $ T.pack $ "no such file exists: " <> toFilePath path
--   let filePath = toFilePath path
--   fileContent <- liftIO $ TIO.readFile filePath
--   result <- runParserT (spaceConsumer >> parser) filePath fileContent
--   case result of
--     Right v ->
--       return v
--     Left errorBundle ->
--       Throw.throw $ createParseError errorBundle

-- createParseError :: ParseErrorBundle T.Text Void -> Error
-- createParseError errorBundle = do
--   let (foo, posState) = attachSourcePos errorOffset (bundleErrors errorBundle) (bundlePosState errorBundle)
--   let hint = Hint.fromSourcePos $ pstateSourcePos posState
--   let message = T.pack $ concatMap (parseErrorTextPretty . fst) $ toList foo
--   Error [logError (fromHint hint) message]

-- currentHint :: Parser m Hint
-- currentHint =
--   Hint.fromSourcePos <$> getSourcePos

-- spaceConsumer :: Parser m ()
-- spaceConsumer =
--   L.space
--     space1
--     (L.skipLineComment "//")
--     (L.skipBlockCommentNested "/-" "-/")

-- asTokens :: T.Text -> ErrorItem Char
-- asTokens s =
--   Tokens $ fromList $ T.unpack s

-- asLabel :: T.Text -> ErrorItem Char
-- asLabel s =
--   Tokens $ fromList $ T.unpack s

lexeme :: Context m => m a -> m a
lexeme =
  baseLexeme spaceConsumer

symbol :: Context m => m T.Text
symbol = do
  lexeme $ takeWhile1P (`S.notMember` nonSymbolCharSet)

baseName :: Context m => m BN.BaseName
baseName = do
  bn <- takeWhile1P (`S.notMember` nonBaseNameCharSet)
  lexeme $ return $ BN.fromText bn

keyword :: Context m => T.Text -> m ()
keyword expected = do
  void $ chunk expected
  notFollowedBy nonSymbolChar
  spaceConsumer

delimiter :: Context m => T.Text -> m ()
delimiter expected = do
  lexeme $ void $ chunk expected

nonSymbolChar :: Context m => m Char
nonSymbolChar =
  satisfy (`S.notMember` nonSymbolCharSet) <?> "non-symbol character"

string :: Context m => m T.Text
string = do
  lexeme $ do
    _ <- char '\"'
    T.pack <$> manyTill charLiteral (char '\"')

integer :: Context m => m Integer
integer = do
  s <- symbol
  case R.readMaybe (T.unpack s) of
    Just value ->
      return value
    Nothing -> do
      s' <- asTokens s
      labelInteger <- asLabel "integer"
      failure (Just s') [labelInteger]

float :: Context m => m Double
float = do
  s <- symbol
  case R.readMaybe (T.unpack s) of
    Just value ->
      return value
    Nothing -> do
      s' <- asTokens s
      labelFloat <- asLabel "float"
      failure (Just s') [labelFloat]

bool :: Context m => m Bool
bool = do
  s <- symbol
  case s of
    "true" ->
      return True
    "false" ->
      return False
    _ -> do
      s' <- asTokens s
      labelTrue <- asLabel "true"
      labelFalse <- asLabel "false"
      failure (Just s') [labelTrue, labelFalse]

-- failure (Just (asTokens s)) (S.fromList [asTokens "true", asTokens "false"])

betweenParen :: Context m => m a -> m a
betweenParen =
  between (delimiter "(") (delimiter ")")

betweenAngle :: Context m => m a -> m a
betweenAngle =
  between (delimiter "<") (delimiter ">")

betweenBracket :: Context m => m a -> m a
betweenBracket =
  between (delimiter "[") (delimiter "]")

asBlock :: Context m => m a -> m a
asBlock =
  between (keyword "as") (keyword "end")

doBlock :: Context m => m a -> m a
doBlock =
  between (keyword "do") (keyword "end")

withBlock :: Context m => m a -> m a
withBlock =
  between (keyword "with") (keyword "end")

importBlock :: Context m => m a -> m a
importBlock =
  between (keyword "import") (keyword "end")

argList :: Context m => m a -> m [a]
argList f = do
  betweenParen $ sepBy f (delimiter ",")

impArgList :: Context m => m a -> m [a]
impArgList f =
  choice
    [ betweenAngle $ sepBy f (delimiter ","),
      return []
    ]

manyList :: Context m => m a -> m [a]
manyList f =
  many $ delimiter "-" >> f

var :: Context m => m (Hint, T.Text)
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

-- -- p :: (Show a) => a -> Parser m ()
-- -- p = liftIO . print
