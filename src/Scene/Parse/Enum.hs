module Scene.Parse.Enum
  ( parseDefineEnum,
  )
where

import Context.App
import qualified Context.Global as Global
import qualified Context.Locator as Locator
import Control.Monad.IO.Class
import qualified Data.Text as T
import Entity.EnumInfo
import qualified Entity.EnumInfo as EnumInfo
import Scene.Parse.Core
import Text.Megaparsec

parseDefineEnum :: Context -> Parser EnumInfo
parseDefineEnum ctx = do
  m <- currentHint
  try $ keyword "define-enum"
  definiteEnumName <- var >>= liftIO . Locator.attachCurrentLocator (locator ctx) . snd
  itemList <- asBlock $ manyList parseDefineEnumClause
  enumInfo <- liftIO $ EnumInfo.new (throw ctx) m definiteEnumName itemList
  liftIO $ uncurry (Global.registerEnum (global ctx) m) (fromEnumInfo enumInfo)
  -- liftIO $ EnumInfo.registerIfNew (ctx & throw) m enumInfo
  return enumInfo

parseDefineEnumClause :: Parser (T.Text, Maybe Integer)
parseDefineEnumClause = do
  choice
    [ try parseDefineEnumClauseWithDiscriminant,
      parseDefineEnumClauseWithoutDiscriminant
    ]

parseDefineEnumClauseWithDiscriminant :: Parser (T.Text, Maybe Integer)
parseDefineEnumClauseWithDiscriminant = do
  item <- snd <$> var
  keyword "="
  discriminant <- integer
  return (item, Just discriminant)

parseDefineEnumClauseWithoutDiscriminant :: Parser (T.Text, Maybe Integer)
parseDefineEnumClauseWithoutDiscriminant = do
  item <- snd <$> var
  return (item, Nothing)
