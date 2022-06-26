module Scene.Parse.Enum
  ( parseDefineEnum,
    insEnumEnv,
    insEnumEnv',
  )
where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.HashMap.Lazy as Map
import Data.IORef
import Data.List
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.Global
import Entity.Hint
import Entity.Log
import Entity.Namespace
import Entity.Stmt
import Scene.Parse.Core
import Text.Megaparsec

parseDefineEnum :: Parser EnumInfo
parseDefineEnum = do
  m <- currentHint
  try $ keyword "define-enum"
  name <- var >>= liftIO . attachSectionPrefix . snd
  itemList <- asBlock $ manyList parseDefineEnumClause
  currentGlobalLocator <- liftIO $ readIORef currentGlobalLocatorRef
  let itemList' = attachPrefix currentGlobalLocator $ setDiscriminant 0 itemList
  unless (isLinear (map snd itemList')) $
    liftIO $ raiseError m "found a collision of discriminant"
  liftIO $ insEnumEnv m name itemList'
  return (m, name, itemList')

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

insEnumEnv :: Hint -> T.Text -> [(T.Text, Int)] -> IO ()
insEnumEnv m name xis = do
  enumEnv <- readIORef enumEnvRef
  let definedEnums = Map.keys enumEnv ++ map fst (concat (Map.elems enumEnv))
  case find (`elem` definedEnums) $ name : map fst xis of
    Just x ->
      raiseError m $ "the constant `" <> x <> "` is already defined [ENUM]"
    _ ->
      insEnumEnv' name xis

insEnumEnv' :: T.Text -> [(T.Text, Int)] -> IO ()
insEnumEnv' name xis = do
  let (xs, is) = unzip xis
  let rev = Map.fromList $ zip xs (zip (repeat name) is)
  modifyIORef' enumEnvRef $ Map.insert name xis
  modifyIORef' revEnumEnvRef $ Map.union rev

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

setDiscriminant :: Int -> [(a, Maybe Int)] -> [(a, Int)]
setDiscriminant discriminant clauseList =
  case clauseList of
    [] ->
      []
    (item, Nothing) : rest ->
      (item, discriminant) : setDiscriminant (discriminant + 1) rest
    (item, Just value) : rest ->
      (item, value) : setDiscriminant (value + 1) rest

attachPrefix :: T.Text -> [(T.Text, a)] -> [(T.Text, a)]
attachPrefix prefix =
  map (\(name, v) -> (prefix <> nsSep <> name, v))
