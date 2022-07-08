module Entity.EnumInfo
  ( EnumInfo,
    EnumValue,
    new,
    fromEnumInfo,
    constBottom,
    constTop,
    constTopUnit,
    constBool,
    constBoolTrue,
    constBoolFalse,
  )
where

import qualified Context.Throw as Throw
import Control.Monad
import Data.Bifunctor
import Data.Binary
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Entity.Discriminant as D
import Entity.EnumTypeName
import qualified Entity.EnumValueName as EV
import qualified Entity.Hint as Hint
import GHC.Generics

type EnumValue = (EV.EnumValueName, D.Discriminant) -- e.g. (this.core::top.unit, 0), (foo.bar.buz::color.yellow, 2)

newtype EnumInfo = EnumInfoCons {fromEnumInfo :: (EnumTypeName, [EnumValue])}
  deriving (Generic)

instance Binary EnumInfo

new :: Throw.Context -> Hint.Hint -> EnumTypeName -> [(T.Text, Maybe D.Discriminant)] -> IO EnumInfo
new ctx m enumTypeName itemList = do
  let itemList' = attachPrefix enumTypeName $ setDiscriminant D.zero itemList
  unless (isLinear (map snd itemList')) $
    Throw.raiseError ctx m "found a collision of discriminant"
  return $ EnumInfoCons {fromEnumInfo = (enumTypeName, itemList')}

attachPrefix :: EnumTypeName -> [(T.Text, a)] -> [(EV.EnumValueName, a)]
attachPrefix enumTypeName =
  map (first (EV.new enumTypeName))

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

constBottom :: EnumTypeName
constBottom =
  EnumTypeName "bottom"

constTop :: EnumTypeName
constTop =
  EnumTypeName "top"

constTopUnit :: EV.EnumValueName
constTopUnit =
  EV.new constTop "unit"

{-# INLINE constBool #-}
constBool :: EnumTypeName
constBool =
  EnumTypeName "bool"

{-# INLINE constBoolTrue #-}
constBoolTrue :: EV.EnumValueName
constBoolTrue =
  EV.new constBool "true"

{-# INLINE constBoolFalse #-}
constBoolFalse :: EV.EnumValueName
constBoolFalse =
  EV.new constBool "false"
