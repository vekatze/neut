{-# LANGUAGE DeriveGeneric #-}

module Entity.EnumInfo
  ( EnumInfo,
    new,
    fromEnumInfo,
    initialEnumEnv,
  )
where

import Control.Monad
import Data.Binary (Binary)
import Data.IORef
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.Global
import qualified Entity.Hint as Hint
import Entity.Log
import GHC.Generics

type EnumItem = (T.Text, Int) -- e.g. (top.unit, 0), (color.yellow, 2)

newtype EnumInfo = EnumInfoCons {fromEnumInfo :: (T.Text, [EnumItem])} deriving (Generic)

instance Binary EnumInfo

new :: Hint.Hint -> T.Text -> [(T.Text, Maybe Int)] -> IO EnumInfo
new m name itemList = do
  currentGlobalLocator <- readIORef currentGlobalLocatorRef
  let itemList' = attachPrefix currentGlobalLocator $ setDiscriminant 0 itemList
  unless (isLinear (map snd itemList')) $
    raiseError m "found a collision of discriminant"
  return $ EnumInfoCons {fromEnumInfo = (name, itemList')}

attachPrefix :: T.Text -> [(T.Text, a)] -> [(T.Text, a)]
attachPrefix prefix =
  map (\(name, v) -> (prefix <> nsSep <> name, v))

setDiscriminant :: Int -> [(a, Maybe Int)] -> [(a, Int)]
setDiscriminant discriminant clauseList =
  case clauseList of
    [] ->
      []
    (item, Nothing) : rest ->
      (item, discriminant) : setDiscriminant (discriminant + 1) rest
    (item, Just value) : rest ->
      (item, value) : setDiscriminant (value + 1) rest

initialEnumEnv :: [EnumInfo]
initialEnumEnv =
  [ EnumInfoCons (constBottom, []),
    EnumInfoCons (constTop, [(constTopUnit, 0)]),
    EnumInfoCons (constBool, [(constBoolFalse, 0), (constBoolTrue, 1)])
  ]

{-# INLINE isLinear #-}
isLinear :: [Int] -> Bool
isLinear =
  isLinear' S.empty

isLinear' :: S.Set Int -> [Int] -> Bool
isLinear' found input =
  case input of
    [] ->
      True
    (x : xs)
      | x `S.member` found ->
        False
      | otherwise ->
        isLinear' (S.insert x found) xs
