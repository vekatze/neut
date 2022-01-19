module Parse.Enum
  ( parseDefineEnum,
    insEnumEnv,
    insEnumEnv',
  )
where

import Control.Monad (unless)
import Data.Basic
  ( Hint,
  )
import Data.Global
  ( currentSectionRef,
    enumEnvRef,
    nsSep,
    revEnumEnvRef,
  )
import qualified Data.HashMap.Lazy as Map
import Data.IORef (modifyIORef', readIORef)
import Data.List (find)
import Data.Log (raiseError)
import Data.Namespace (attachSectionPrefix)
import qualified Data.Set as S
import Data.Stmt
  ( EnumInfo,
  )
import qualified Data.Text as T
import Parse.Core
  ( currentHint,
    parseAsBlock,
    parseInteger,
    parseManyList,
    parseToken,
    parseVar,
    parseVarText,
    tryPlanList,
  )

parseDefineEnum :: IO EnumInfo
parseDefineEnum = do
  m <- currentHint
  parseToken "define-enum"
  name <- parseVar >>= attachSectionPrefix . snd
  itemList <- parseAsBlock $ parseManyList parseDefineEnumClause
  currentSection <- readIORef currentSectionRef
  let itemList' = arrangeEnumItemList currentSection 0 itemList
  unless (isLinear (map snd itemList')) $
    raiseError m "found a collision of discriminant"
  insEnumEnv m name itemList'
  return (m, name, itemList')

arrangeEnumItemList :: T.Text -> Int -> [(T.Text, Maybe Int)] -> [(T.Text, Int)]
arrangeEnumItemList currentSection currentValue clauseList =
  case clauseList of
    [] ->
      []
    (item, Nothing) : rest ->
      (currentSection <> nsSep <> item, currentValue) : arrangeEnumItemList currentSection (currentValue + 1) rest
    (item, Just v) : rest ->
      (currentSection <> nsSep <> item, v) : arrangeEnumItemList currentSection (v + 1) rest

-- (item, Nothing) : rest ->
--   (name <> nsSep <> item, currentValue) : arrangeEnumItemList name (currentValue + 1) rest
-- (item, Just v) : rest ->
--   (name <> nsSep <> item, v) : arrangeEnumItemList name (v + 1) rest

parseDefineEnumClause :: IO (T.Text, Maybe Int)
parseDefineEnumClause = do
  tryPlanList
    [ parseDefineEnumClauseWithDiscriminant
    ]
    parseDefineEnumClauseWithoutDiscriminant

parseDefineEnumClauseWithDiscriminant :: IO (T.Text, Maybe Int)
parseDefineEnumClauseWithDiscriminant = do
  item <- snd <$> parseVar
  parseToken "="
  discriminant <- parseInteger
  return (item, Just (fromInteger discriminant))

parseDefineEnumClauseWithoutDiscriminant :: IO (T.Text, Maybe Int)
parseDefineEnumClauseWithoutDiscriminant = do
  item <- parseVarText
  return (item, Nothing)

insEnumEnv' :: T.Text -> [(T.Text, Int)] -> IO ()
insEnumEnv' name xis = do
  let (xs, is) = unzip xis
  let rev = Map.fromList $ zip xs (zip (repeat name) is)
  modifyIORef' enumEnvRef $ Map.insert name xis
  modifyIORef' revEnumEnvRef $ Map.union rev

insEnumEnv :: Hint -> T.Text -> [(T.Text, Int)] -> IO ()
insEnumEnv m name xis = do
  enumEnv <- readIORef enumEnvRef
  let definedEnums = Map.keys enumEnv ++ map fst (concat (Map.elems enumEnv))
  case find (`elem` definedEnums) $ name : map fst xis of
    Just x ->
      raiseError m $ "the constant `" <> x <> "` is already defined [ENUM]"
    _ ->
      insEnumEnv' name xis

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
