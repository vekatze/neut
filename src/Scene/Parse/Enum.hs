module Scene.Parse.Enum
  ( parseDefineEnum,
  )
where

import Context.App
import Control.Monad.IO.Class
import Data.Function
import qualified Data.Text as T
import Entity.EnumInfo
import qualified Entity.EnumInfo as EnumInfo
import qualified Entity.EnumInfo.Env as EnumInfo
import Entity.Namespace
import Scene.Parse.Core
import Text.Megaparsec

parseDefineEnum :: Axis -> Parser EnumInfo
parseDefineEnum axis = do
  m <- currentHint
  try $ keyword "define-enum"
  definiteEnumName <- var >>= liftIO . attachSectionPrefix . snd
  itemList <- asBlock $ manyList parseDefineEnumClause
  enumInfo <- liftIO $ EnumInfo.new (axis & throw) m definiteEnumName itemList
  liftIO $ EnumInfo.registerIfNew (axis & throw) m enumInfo
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
