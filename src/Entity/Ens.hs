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
import Data.HashMap.Strict qualified as M
import Data.Int
import Data.List
import Data.Text qualified as T
import Entity.EnsType qualified as ET
import Entity.Hint
import Entity.Remark

data EnsF a
  = Int64 Int64
  | Float64 Double
  | Bool Bool
  | String T.Text
  | List [a]
  | Dictionary (M.HashMap T.Text a)

type Ens = Cofree EnsF Hint

access :: T.Text -> Ens -> Either Error Ens
access k entity@(m :< _) = do
  (_, dictionary) <- toDictionary entity
  case M.lookup k dictionary of
    Just v ->
      return v
    Nothing ->
      raiseKeyNotFoundError m k

toInt64 :: Ens -> Either Error Int64
toInt64 entity@(m :< _) =
  case entity of
    _ :< Int64 s ->
      return s
    _ ->
      raiseTypeError m ET.Int64 (typeOf entity)

toFloat64 :: Ens -> Either Error Double
toFloat64 entity@(m :< _) =
  case entity of
    _ :< Float64 s ->
      return s
    _ ->
      raiseTypeError m ET.Float64 (typeOf entity)

toBool :: Ens -> Either Error Bool
toBool entity@(m :< _) =
  case entity of
    _ :< Bool x ->
      return x
    _ ->
      raiseTypeError m ET.Bool (typeOf entity)

toString :: Ens -> Either Error (Hint, T.Text)
toString entity@(m :< _) =
  case entity of
    _ :< String s ->
      return (m, s)
    _ ->
      raiseTypeError m ET.String (typeOf entity)

toDictionary :: Ens -> Either Error (Hint, M.HashMap T.Text Ens)
toDictionary entity@(m :< _) =
  case entity of
    _ :< Dictionary e ->
      return (m, e)
    _ ->
      raiseTypeError m ET.Dictionary (typeOf entity)

toList :: Ens -> Either Error [Ens]
toList entity@(m :< _) =
  case entity of
    _ :< List e ->
      return e
    _ ->
      raiseTypeError m ET.List (typeOf entity)

typeOf :: Ens -> ET.EnsType
typeOf v =
  case v of
    _ :< Int64 _ ->
      ET.Int64
    _ :< Float64 _ ->
      ET.Float64
    _ :< Bool _ ->
      ET.Bool
    _ :< String _ ->
      ET.String
    _ :< List _ ->
      ET.List
    _ :< Dictionary _ ->
      ET.Dictionary

raiseTypeError :: Hint -> ET.EnsType -> ET.EnsType -> Either Error a
raiseTypeError m expectedType actualType =
  Left $
    newError m $
      "the value here is expected to be of type `"
        <> ET.showEnsType expectedType
        <> "`, but is: `"
        <> ET.showEnsType actualType
        <> "`"

raiseKeyNotFoundError :: Hint -> T.Text -> Either Error a
raiseKeyNotFoundError m k =
  Left $
    newError m $
      "couldn't find the required key `"
        <> k
        <> "`."

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
  if null xs
    then "[]"
    else do
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
    _ :< Int64 i ->
      ppInt64 i
    _ :< Float64 i ->
      ppFloat64 i
    _ :< Bool b ->
      ppBool b
    _ :< String s ->
      ppString s
    _ :< List xs -> do
      ppList n xs
    _ :< Dictionary dict -> do
      ppDictionary n dict

ppEnsTopLevel :: M.HashMap T.Text (Cofree EnsF a) -> T.Text
ppEnsTopLevel dict = do
  let dictList = sortOn fst $ M.toList dict
  let strList = map (uncurry $ ppDictionaryEntry 0) dictList
  T.intercalate "\n" strList <> "\n"
