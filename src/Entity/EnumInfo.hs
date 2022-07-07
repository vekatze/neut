module Entity.EnumInfo
  ( EnumInfo,
    EnumValue,
    EnumValueName,
    EnumTypeName,
    new,
    fromEnumInfo,
  )
where

import qualified Context.Throw as Throw
import Control.Monad
import Data.Binary
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Entity.Discriminant as D
import Entity.Global
import qualified Entity.Hint as Hint
import GHC.Generics

type EnumTypeName = T.Text

type EnumValueName = T.Text

type EnumValue = (EnumValueName, D.Discriminant) -- e.g. (this.core::top.unit, 0), (foo.bar.buz::color.yellow, 2)

newtype EnumInfo = EnumInfoCons {fromEnumInfo :: (EnumTypeName, [EnumValue])} deriving (Generic)

instance Binary EnumInfo

new :: Throw.Context -> Hint.Hint -> T.Text -> [(T.Text, Maybe D.Discriminant)] -> IO EnumInfo
new ctx m definiteEnumName itemList = do
  let itemList' = attachPrefix definiteEnumName $ setDiscriminant D.zero itemList
  unless (isLinear (map snd itemList')) $
    Throw.raiseError ctx m "found a collision of discriminant"
  return $ EnumInfoCons {fromEnumInfo = (definiteEnumName, itemList')}

attachPrefix :: T.Text -> [(T.Text, a)] -> [(T.Text, a)]
attachPrefix prefix =
  map (\(name, v) -> (prefix <> nsSep <> name, v))

setDiscriminant :: D.Discriminant -> [(a, Maybe D.Discriminant)] -> [(a, D.Discriminant)]
setDiscriminant discriminant clauseList =
  case clauseList of
    [] ->
      []
    (item, Nothing) : rest ->
      (item, discriminant) : setDiscriminant (D.increment discriminant) rest
    (item, Just value) : rest ->
      (item, value) : setDiscriminant (D.increment value) rest

{-# INLINE isLinear #-}
isLinear :: [D.Discriminant] -> Bool
isLinear =
  isLinear' S.empty

isLinear' :: S.Set D.Discriminant -> [D.Discriminant] -> Bool
isLinear' found input =
  case input of
    [] ->
      True
    (x : xs)
      | x `S.member` found ->
        False
      | otherwise ->
        isLinear' (S.insert x found) xs
