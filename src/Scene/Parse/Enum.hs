module Scene.Parse.Enum
  ( parseDefineEnum,
  )
where

import Context.App
import qualified Context.Global as Global
import qualified Context.Locator as Locator
import Control.Monad.IO.Class
import qualified Data.Text as T
import Entity.Discriminant
import Entity.EnumInfo
import qualified Entity.EnumInfo as EnumInfo
import qualified Entity.EnumTypeName as ET
import Scene.Parse.Core
import Text.Megaparsec

parseDefineEnum :: Context -> Parser EnumInfo
parseDefineEnum ctx = do
  m <- currentHint
  try $ keyword "define-enum"
  definiteEnumName <- baseName >>= liftIO . Locator.attachCurrentLocator (locator ctx)
  itemList <- asBlock $ manyList parseDefineEnumClause
  enumInfo <- liftIO $ EnumInfo.new (throw ctx) m (ET.EnumTypeName definiteEnumName) itemList
  liftIO $ uncurry (Global.registerEnum (global ctx) m) (fromEnumInfo enumInfo)
  return enumInfo

parseDefineEnumClause :: Parser (T.Text, Maybe Discriminant)
parseDefineEnumClause = do
  choice
    [ try parseDefineEnumClauseWithDiscriminant,
      parseDefineEnumClauseWithoutDiscriminant
    ]

parseDefineEnumClauseWithDiscriminant :: Parser (T.Text, Maybe Discriminant)
parseDefineEnumClauseWithDiscriminant = do
  item <- snd <$> var
  keyword "="
  i <- integer
  return (item, Just $ MakeDiscriminant {reify = i})

parseDefineEnumClauseWithoutDiscriminant :: Parser (T.Text, Maybe Discriminant)
parseDefineEnumClauseWithoutDiscriminant = do
  item <- snd <$> var
  return (item, Nothing)
