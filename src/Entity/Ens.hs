module Entity.Ens
  ( Ens,
    EnsF (..),
    toInt64,
    toFloat64,
    toBool,
    toString,
    toList,
    toDictionary,
    access,
    ppEns,
    ppEnsTopLevel,
  )
where

import Control.Comonad.Cofree
import qualified Data.HashMap.Lazy as M
import Data.Int
import Data.List
import qualified Data.Text as T
import Entity.Basic
import Entity.Log

data EnsF a
  = EnsInt64 Int64
  | EnsFloat64 Double
  | EnsBool Bool
  | EnsString T.Text
  | EnsList [a]
  | EnsDictionary (M.HashMap T.Text a)

type Ens = Cofree EnsF Hint

data EnsType
  = EnsTypeInt64
  | EnsTypeFloat64
  | EnsTypeBool
  | EnsTypeString
  | EnsTypeList
  | EnsTypeDictionary

access :: T.Text -> Ens -> IO Ens
access k entity@(m :< _) = do
  dictionary <- toDictionary entity
  case M.lookup k dictionary of
    Just v ->
      return v
    Nothing ->
      raiseKeyNotFoundError m k

toInt64 :: Ens -> IO Int64
toInt64 entity@(m :< _) =
  case entity of
    _ :< EnsInt64 s ->
      return s
    _ ->
      raiseTypeError m EnsTypeInt64 (typeOf entity)

toFloat64 :: Ens -> IO Double
toFloat64 entity@(m :< _) =
  case entity of
    _ :< EnsFloat64 s ->
      return s
    _ ->
      raiseTypeError m EnsTypeFloat64 (typeOf entity)

toBool :: Ens -> IO Bool
toBool entity@(m :< _) =
  case entity of
    _ :< EnsBool x ->
      return x
    _ ->
      raiseTypeError m EnsTypeBool (typeOf entity)

toString :: Ens -> IO T.Text
toString entity@(m :< _) =
  case entity of
    _ :< EnsString s ->
      return s
    _ ->
      raiseTypeError m EnsTypeString (typeOf entity)

toDictionary :: Ens -> IO (M.HashMap T.Text Ens)
toDictionary entity@(m :< _) =
  case entity of
    _ :< EnsDictionary e ->
      return e
    _ ->
      raiseTypeError m EnsTypeDictionary (typeOf entity)

toList :: Ens -> IO [Ens]
toList entity@(m :< _) =
  case entity of
    _ :< EnsList e ->
      return e
    _ ->
      raiseTypeError m EnsTypeList (typeOf entity)

typeOf :: Ens -> EnsType
typeOf v =
  case v of
    _ :< EnsInt64 _ ->
      EnsTypeInt64
    _ :< EnsFloat64 _ ->
      EnsTypeFloat64
    _ :< EnsBool _ ->
      EnsTypeBool
    _ :< EnsString _ ->
      EnsTypeString
    _ :< EnsList _ ->
      EnsTypeList
    _ :< EnsDictionary _ ->
      EnsTypeDictionary

showEnsType :: EnsType -> T.Text
showEnsType entityType =
  case entityType of
    EnsTypeInt64 ->
      "i64"
    EnsTypeFloat64 ->
      "f64"
    EnsTypeBool ->
      "bool"
    EnsTypeString ->
      "string"
    EnsTypeList ->
      "list"
    EnsTypeDictionary ->
      "dictionary"

raiseKeyNotFoundError :: Hint -> T.Text -> IO a
raiseKeyNotFoundError m k =
  raiseError m $ "couldn't find the required key `" <> k <> "`."

raiseTypeError :: Hint -> EnsType -> EnsType -> IO a
raiseTypeError m expectedType actualType =
  raiseError m $
    "the value here is expected to be of type `"
      <> showEnsType expectedType
      <> "`, but is: `"
      <> showEnsType actualType
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

ppList :: Int -> [Cofree EnsF a] -> T.Text
ppList n xs = do
  let header = "["
  let xs' = map (showWithOffset (n + 1) . ppEns (n + 1)) xs
  let footer = showWithOffset n "]"
  T.intercalate "\n" $ [header] <> xs' <> [footer]

ppDictionary :: Int -> M.HashMap T.Text (Cofree EnsF a) -> T.Text
ppDictionary n dict = do
  if M.size dict == 0
    then "{}"
    else do
      let header = "{"
      let dictList = sortOn fst $ M.toList dict
      let strList = map (uncurry $ ppDictionaryEntry (n + 1)) dictList
      let footer = showWithOffset n "}"
      T.intercalate "\n" $ [header] <> strList <> [footer]

ppDictionaryEntry :: Int -> T.Text -> Cofree EnsF a -> T.Text
ppDictionaryEntry n key value = do
  showWithOffset n $ key <> " = " <> ppEns n value

ppEns :: Int -> Cofree EnsF a -> T.Text
ppEns n entity = do
  case entity of
    _ :< EnsInt64 i ->
      ppInt64 i
    _ :< EnsFloat64 i ->
      ppFloat64 i
    _ :< EnsBool b ->
      ppBool b
    _ :< EnsString s ->
      ppString s
    _ :< EnsList xs -> do
      ppList n xs
    _ :< EnsDictionary dict -> do
      ppDictionary n dict

ppEnsTopLevel :: M.HashMap T.Text (Cofree EnsF a) -> T.Text
ppEnsTopLevel dict = do
  let dictList = sortOn fst $ M.toList dict
  let strList = map (uncurry $ ppDictionaryEntry 0) dictList
  T.intercalate "\n" strList <> "\n"
