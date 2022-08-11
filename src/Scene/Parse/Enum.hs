module Scene.Parse.Enum
  ( parseDefineEnum,
    Context,
  )
where

import qualified Context.Global as Global
import qualified Context.Locator as Locator
import qualified Context.Throw as Throw
import Control.Monad.Trans
import qualified Data.Text as T
import qualified Entity.Discriminant as D
import Entity.EnumInfo
import qualified Entity.EnumInfo as EnumInfo
import qualified Entity.EnumTypeName as ET
import qualified Scene.Parse.Core as P
import Text.Megaparsec

class (Locator.Context m, Global.Context m, Throw.Context m, P.Context m) => Context m

parseDefineEnum :: Context m => P.Parser m EnumInfo
parseDefineEnum = do
  m <- lift P.getCurrentHint
  try $ P.keyword "define-enum"
  definiteEnumName <- P.baseName >>= lift . Locator.attachCurrentLocator
  itemList <- P.asBlock $ P.manyList parseDefineEnumClause
  enumInfo <- lift $ EnumInfo.new m (ET.EnumTypeName definiteEnumName) itemList
  lift $ uncurry (Global.registerEnum m) (fromEnumInfo enumInfo)
  return enumInfo

parseDefineEnumClause :: Context m => P.Parser m (T.Text, Maybe D.Discriminant)
parseDefineEnumClause = do
  choice
    [ try parseDefineEnumClauseWithDiscriminant,
      parseDefineEnumClauseWithoutDiscriminant
    ]

parseDefineEnumClauseWithDiscriminant :: Context m => P.Parser m (T.Text, Maybe D.Discriminant)
parseDefineEnumClauseWithDiscriminant = do
  item <- snd <$> P.var
  P.keyword "="
  i <- P.integer
  return (item, Just $ D.MakeDiscriminant {D.reify = i})

parseDefineEnumClauseWithoutDiscriminant :: Context m => P.Parser m (T.Text, Maybe D.Discriminant)
parseDefineEnumClauseWithoutDiscriminant = do
  item <- snd <$> P.var
  return (item, Nothing)
