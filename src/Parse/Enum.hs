module Parse.Enum
  ( parseDefineEnum,
    initializeEnumEnv,
    insEnumEnv,
  )
where

import Control.Monad (forM_, unless)
import Data.Basic
  ( Hint,
  )
import Data.Global
  ( boolFalse,
    boolTrue,
    enumEnv,
    nsSep,
    revEnumEnv,
  )
import qualified Data.HashMap.Lazy as Map
import Data.IORef (modifyIORef', readIORef)
import Data.List (find)
import Data.Log (raiseError)
import Data.Namespace
  ( withSectionPrefix,
  )
import qualified Data.Set as S
import Data.Stmt
  ( EnumInfo,
  )
import qualified Data.Text as T
import Parse.Core
  ( currentHint,
    integer,
    many,
    token,
    tryPlanList,
    varText,
  )

parseDefineEnum :: IO EnumInfo
parseDefineEnum = do
  m <- currentHint
  token "define-enum"
  name <- varText >>= withSectionPrefix
  itemList <- many parseDefineEnumClause
  let itemList' = arrangeEnumItemList name 0 itemList
  unless (isLinear (map snd itemList')) $
    raiseError m "found a collision of discriminant"
  insEnumEnv m name itemList'
  return (m, name, itemList')

arrangeEnumItemList :: T.Text -> Int -> [(T.Text, Maybe Int)] -> [(T.Text, Int)]
arrangeEnumItemList name currentValue clauseList =
  case clauseList of
    [] ->
      []
    (item, Nothing) : rest ->
      (name <> nsSep <> item, currentValue) : arrangeEnumItemList name (currentValue + 1) rest
    (item, Just v) : rest ->
      (name <> nsSep <> item, v) : arrangeEnumItemList name (v + 1) rest

parseDefineEnumClause :: IO (T.Text, Maybe Int)
parseDefineEnumClause = do
  tryPlanList
    [ parseDefineEnumClauseWithDiscriminant,
      parseDefineEnumClauseWithoutDiscriminant
    ]

parseDefineEnumClauseWithDiscriminant :: IO (T.Text, Maybe Int)
parseDefineEnumClauseWithDiscriminant = do
  token "-"
  item <- varText
  token "<-"
  discriminant <- integer
  return (item, Just (fromInteger discriminant))

parseDefineEnumClauseWithoutDiscriminant :: IO (T.Text, Maybe Int)
parseDefineEnumClauseWithoutDiscriminant = do
  token "-"
  item <- varText
  return (item, Nothing)

initEnumEnvInfo :: [(T.Text, [(T.Text, Int)])]
initEnumEnvInfo =
  [ ("bottom", []),
    ("top", [("top.unit", 0)]),
    ("bool", [(boolFalse, 0), (boolTrue, 1)])
  ]

initializeEnumEnv :: IO ()
initializeEnumEnv =
  forM_ initEnumEnvInfo $ uncurry insEnumEnv'

insEnumEnv' :: T.Text -> [(T.Text, Int)] -> IO ()
insEnumEnv' name xis = do
  let (xs, is) = unzip xis
  let rev = Map.fromList $ zip xs (zip (repeat name) is)
  modifyIORef' enumEnv $ \env -> Map.insert name xis env
  modifyIORef' revEnumEnv $ \env -> Map.union rev env

insEnumEnv :: Hint -> T.Text -> [(T.Text, Int)] -> IO ()
insEnumEnv m name xis = do
  eenv <- readIORef enumEnv
  let definedEnums = Map.keys eenv ++ map fst (concat (Map.elems eenv))
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
