module Scene.Parse.Enum
  ( parseDefineEnum,
    Context,
  )
where

import qualified Context.Global as Global
import qualified Context.Locator as Locator
import qualified Context.Throw as Throw
import qualified Data.Text as T
import qualified Entity.Discriminant as D
import Entity.EnumInfo
import qualified Entity.EnumInfo as EnumInfo
import qualified Entity.EnumTypeName as ET
import qualified Scene.Parse.Core as P

class (Locator.Context m, Global.Context m, Throw.Context m, P.Context m) => Context m

parseDefineEnum :: Context m => m EnumInfo
parseDefineEnum = do
  m <- P.getCurrentHint
  P.try $ P.keyword "define-enum"
  definiteEnumName <- P.baseName >>= Locator.attachCurrentLocator
  itemList <- P.asBlock $ P.manyList parseDefineEnumClause
  enumInfo <- EnumInfo.new m (ET.EnumTypeName definiteEnumName) itemList
  uncurry (Global.registerEnum m) (fromEnumInfo enumInfo)
  return enumInfo

parseDefineEnumClause :: Context m => m (T.Text, Maybe D.Discriminant)
parseDefineEnumClause = do
  P.choice
    [ P.try parseDefineEnumClauseWithDiscriminant,
      parseDefineEnumClauseWithoutDiscriminant
    ]

parseDefineEnumClauseWithDiscriminant :: Context m => m (T.Text, Maybe D.Discriminant)
parseDefineEnumClauseWithDiscriminant = do
  item <- snd <$> P.var
  P.keyword "="
  i <- P.integer
  return (item, Just $ D.MakeDiscriminant {D.reify = i})

parseDefineEnumClauseWithoutDiscriminant :: Context m => m (T.Text, Maybe D.Discriminant)
parseDefineEnumClauseWithoutDiscriminant = do
  item <- snd <$> P.var
  return (item, Nothing)
