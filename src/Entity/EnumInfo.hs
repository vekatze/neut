module Entity.EnumInfo
  ( EnumInfo,
    EnumItem,
    Discriminant,
    EnumValueName,
    EnumTypeName,
    new,
    fromEnumInfo,
    -- initialEnumEnv,
  )
where

import Context.Throw
import Control.Monad
import Data.Binary (Binary)
import Data.Function
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.Global
import qualified Entity.Hint as Hint
import GHC.Generics

type Discriminant = Integer

type EnumTypeName = T.Text

type EnumValueName = T.Text

type EnumItem = (EnumValueName, Discriminant) -- e.g. (this.core::top.unit, 0), (foo.bar.buz::color.yellow, 2)

newtype EnumInfo = EnumInfoCons {fromEnumInfo :: (T.Text, [EnumItem])} deriving (Generic)

instance Binary EnumInfo

new :: Context -> Hint.Hint -> T.Text -> [(T.Text, Maybe Integer)] -> IO EnumInfo
new context m definiteEnumName itemList = do
  let itemList' = attachPrefix definiteEnumName $ setDiscriminant 0 itemList
  unless (isLinear (map snd itemList')) $
    (context & raiseError) m "found a collision of discriminant"
  return $ EnumInfoCons {fromEnumInfo = (definiteEnumName, itemList')}

attachPrefix :: T.Text -> [(T.Text, a)] -> [(T.Text, a)]
attachPrefix prefix =
  map (\(name, v) -> (prefix <> nsSep <> name, v))

setDiscriminant :: Integer -> [(a, Maybe Integer)] -> [(a, Integer)]
setDiscriminant discriminant clauseList =
  case clauseList of
    [] ->
      []
    (item, Nothing) : rest ->
      (item, discriminant) : setDiscriminant (discriminant + 1) rest
    (item, Just value) : rest ->
      (item, value) : setDiscriminant (value + 1) rest

-- initialEnumEnv :: [EnumInfo]
-- initialEnumEnv =
--   [ EnumInfoCons (constBottom, []),
--     EnumInfoCons (constTop, [(constTopUnit, 0)]),
--     EnumInfoCons (constBool, [(constBoolFalse, 0), (constBoolTrue, 1)])
--   ]

{-# INLINE isLinear #-}
isLinear :: [Integer] -> Bool
isLinear =
  isLinear' S.empty

isLinear' :: S.Set Integer -> [Integer] -> Bool
isLinear' found input =
  case input of
    [] ->
      True
    (x : xs)
      | x `S.member` found ->
        False
      | otherwise ->
        isLinear' (S.insert x found) xs
