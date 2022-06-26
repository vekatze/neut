module Entity.Entity
  ( Entity,
    EntityF (..),
    toInt64,
    toFloat64,
    toBool,
    toString,
    toList,
    toDictionary,
    access,
    ppEntity,
    ppEntityTopLevel,
  )
where

import Control.Comonad.Cofree
import qualified Data.HashMap.Lazy as M
import Data.Int
import Data.List
import qualified Data.Text as T
import Entity.Basic
import Entity.Log

data EntityF a
  = EntityInt64 Int64
  | EntityFloat64 Double
  | EntityBool Bool
  | EntityString T.Text
  | EntityList [a]
  | EntityDictionary (M.HashMap T.Text a)

type Entity = Cofree EntityF Hint

data EntityType
  = EntityTypeInt64
  | EntityTypeFloat64
  | EntityTypeBool
  | EntityTypeString
  | EntityTypeList
  | EntityTypeDictionary

access :: T.Text -> Entity -> IO Entity
access k entity@(m :< _) = do
  dictionary <- toDictionary entity
  case M.lookup k dictionary of
    Just v ->
      return v
    Nothing ->
      raiseKeyNotFoundError m k

toInt64 :: Entity -> IO Int64
toInt64 entity@(m :< _) =
  case entity of
    _ :< EntityInt64 s ->
      return s
    _ ->
      raiseTypeError m EntityTypeInt64 (typeOf entity)

toFloat64 :: Entity -> IO Double
toFloat64 entity@(m :< _) =
  case entity of
    _ :< EntityFloat64 s ->
      return s
    _ ->
      raiseTypeError m EntityTypeFloat64 (typeOf entity)

toBool :: Entity -> IO Bool
toBool entity@(m :< _) =
  case entity of
    _ :< EntityBool x ->
      return x
    _ ->
      raiseTypeError m EntityTypeBool (typeOf entity)

toString :: Entity -> IO T.Text
toString entity@(m :< _) =
  case entity of
    _ :< EntityString s ->
      return s
    _ ->
      raiseTypeError m EntityTypeString (typeOf entity)

toDictionary :: Entity -> IO (M.HashMap T.Text Entity)
toDictionary entity@(m :< _) =
  case entity of
    _ :< EntityDictionary e ->
      return e
    _ ->
      raiseTypeError m EntityTypeDictionary (typeOf entity)

toList :: Entity -> IO [Entity]
toList entity@(m :< _) =
  case entity of
    _ :< EntityList e ->
      return e
    _ ->
      raiseTypeError m EntityTypeList (typeOf entity)

typeOf :: Entity -> EntityType
typeOf v =
  case v of
    _ :< EntityInt64 _ ->
      EntityTypeInt64
    _ :< EntityFloat64 _ ->
      EntityTypeFloat64
    _ :< EntityBool _ ->
      EntityTypeBool
    _ :< EntityString _ ->
      EntityTypeString
    _ :< EntityList _ ->
      EntityTypeList
    _ :< EntityDictionary _ ->
      EntityTypeDictionary

showEntityType :: EntityType -> T.Text
showEntityType entityType =
  case entityType of
    EntityTypeInt64 ->
      "i64"
    EntityTypeFloat64 ->
      "f64"
    EntityTypeBool ->
      "bool"
    EntityTypeString ->
      "string"
    EntityTypeList ->
      "list"
    EntityTypeDictionary ->
      "dictionary"

raiseKeyNotFoundError :: Hint -> T.Text -> IO a
raiseKeyNotFoundError m k =
  raiseError m $ "couldn't find the required key `" <> k <> "`."

raiseTypeError :: Hint -> EntityType -> EntityType -> IO a
raiseTypeError m expectedType actualType =
  raiseError m $
    "the value here is expected to be of type `"
      <> showEntityType expectedType
      <> "`, but is: `"
      <> showEntityType actualType
      <> "`"

showWithOffset :: Int -> T.Text -> T.Text
showWithOffset n text =
  T.replicate n "  " <> text

ppInt64 :: Int64 -> T.Text
ppInt64 i =
  T.pack (show i)

ppFloat64 :: Double -> T.Text
ppFloat64 i =
  T.pack (show i)

ppBool :: Bool -> T.Text
ppBool x =
  if x
    then "true"
    else "false"

ppString :: T.Text -> T.Text
ppString x =
  T.pack $ show x

ppList :: Int -> [Cofree EntityF a] -> T.Text
ppList n xs = do
  let header = "["
  let xs' = map (showWithOffset (n + 1) . ppEntity (n + 1)) xs
  let footer = showWithOffset n "]"
  T.intercalate "\n" $ [header] <> xs' <> [footer]

ppDictionary :: Int -> M.HashMap T.Text (Cofree EntityF a) -> T.Text
ppDictionary n dict = do
  if M.size dict == 0
    then "{}"
    else do
      let header = "{"
      let dictList = sortOn fst $ M.toList dict
      let strList = map (uncurry $ ppDictionaryEntry (n + 1)) dictList
      let footer = showWithOffset n "}"
      T.intercalate "\n" $ [header] <> strList <> [footer]

ppDictionaryEntry :: Int -> T.Text -> Cofree EntityF a -> T.Text
ppDictionaryEntry n key value = do
  showWithOffset n $ key <> " = " <> ppEntity n value

ppEntity :: Int -> Cofree EntityF a -> T.Text
ppEntity n entity = do
  case entity of
    _ :< EntityInt64 i ->
      ppInt64 i
    _ :< EntityFloat64 i ->
      ppFloat64 i
    _ :< EntityBool b ->
      ppBool b
    _ :< EntityString s ->
      ppString s
    _ :< EntityList xs -> do
      ppList n xs
    _ :< EntityDictionary dict -> do
      ppDictionary n dict

ppEntityTopLevel :: M.HashMap T.Text (Cofree EntityF a) -> T.Text
ppEntityTopLevel dict = do
  let dictList = sortOn fst $ M.toList dict
  let strList = map (uncurry $ ppDictionaryEntry 0) dictList
  T.intercalate "\n" strList <> "\n"
