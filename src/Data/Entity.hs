module Data.Entity
  ( Entity,
    EntityF (..),
    toInt64,
    toFloat64,
    toBool,
    toString,
    toList,
    toDictionary,
    access,
  )
where

import Control.Comonad.Cofree (Cofree (..))
import Data.Basic (Hint)
import qualified Data.HashMap.Lazy as M
import Data.Int (Int64)
import Data.Log (raiseError)
import qualified Data.Text as T

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
