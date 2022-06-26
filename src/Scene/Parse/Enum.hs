module Scene.Parse.Enum
  ( parseDefineEnum,
    insEnumEnv,
    insEnumEnv',
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Lazy as Map
import Data.IORef (modifyIORef', readIORef)
import Data.List (find)
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.Basic
  ( Hint,
  )
import Entity.Global
  ( currentGlobalLocatorRef,
    enumEnvRef,
    nsSep,
    revEnumEnvRef,
  )
import Entity.Log (raiseError)
import Entity.Namespace (attachSectionPrefix)
import Entity.Stmt
  ( EnumInfo,
  )
import Scene.Parse.Core
  ( Parser,
    asBlock,
    currentHint,
    integer,
    keyword,
    manyList,
    var,
  )
import Text.Megaparsec (choice, try)

parseDefineEnum :: Parser EnumInfo
parseDefineEnum = do
  m <- currentHint
  try $ keyword "define-enum"
  name <- var >>= liftIO . attachSectionPrefix . snd
  itemList <- asBlock $ manyList parseDefineEnumClause
  currentGlobalLocator <- liftIO $ readIORef currentGlobalLocatorRef
  let itemList' = arrangeEnumItemList currentGlobalLocator 0 itemList
  unless (isLinear (map snd itemList')) $
    liftIO $ raiseError m "found a collision of discriminant"
  liftIO $ insEnumEnv m name itemList'
  return (m, name, itemList')

arrangeEnumItemList :: T.Text -> Int -> [(T.Text, Maybe Int)] -> [(T.Text, Int)]
arrangeEnumItemList currentGlobalLocator currentValue clauseList =
  case clauseList of
    [] ->
      []
    (item, Nothing) : rest ->
      (currentGlobalLocator <> nsSep <> item, currentValue) : arrangeEnumItemList currentGlobalLocator (currentValue + 1) rest
    (item, Just v) : rest ->
      (currentGlobalLocator <> nsSep <> item, v) : arrangeEnumItemList currentGlobalLocator (v + 1) rest

parseDefineEnumClause :: Parser (T.Text, Maybe Int)
parseDefineEnumClause = do
  choice
    [ try parseDefineEnumClauseWithDiscriminant,
      parseDefineEnumClauseWithoutDiscriminant
    ]

parseDefineEnumClauseWithDiscriminant :: Parser (T.Text, Maybe Int)
parseDefineEnumClauseWithDiscriminant = do
  item <- snd <$> var
  keyword "="
  discriminant <- integer
  return (item, Just (fromInteger discriminant))

parseDefineEnumClauseWithoutDiscriminant :: Parser (T.Text, Maybe Int)
parseDefineEnumClauseWithoutDiscriminant = do
  item <- snd <$> var
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
