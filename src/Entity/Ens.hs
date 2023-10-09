module Entity.Ens
  ( Ens,
    EnsF (..),
    MiniEns,
    toInt,
    toFloat,
    toBool,
    toString,
    toList,
    toDictionary,
    access,
    access',
    emptyList,
    emptyDict,
    ensPath,
    ppEns,
    ppEnsTopLevel,
  )
where

import Control.Comonad.Cofree
import Data.HashMap.Strict qualified as M
import Data.List
import Data.Text qualified as T
import Entity.EnsType qualified as ET
import Entity.Error
import Entity.Hint
import Path

data EnsF a
  = Int Int
  | Float Double
  | Bool Bool
  | String T.Text
  | List [a]
  | Dictionary (M.HashMap T.Text a)

type Ens = Cofree EnsF Hint

type MiniEns = Cofree EnsF ()

access :: T.Text -> Ens -> Either Error Ens
access k ens@(m :< _) = do
  (_, dictionary) <- toDictionary ens
  case M.lookup k dictionary of
    Just v ->
      return v
    Nothing ->
      raiseKeyNotFoundError m k

access' :: T.Text -> EnsF Ens -> Ens -> Either Error Ens
access' k defaultValue ens@(m :< _) = do
  (_, dictionary) <- toDictionary ens
  case M.lookup k dictionary of
    Just v ->
      return v
    Nothing ->
      return $ m :< defaultValue

ensPath :: Path a b -> EnsF (Cofree EnsF c)
ensPath path =
  String $ T.pack (toFilePath path)

emptyDict :: EnsF Ens
emptyDict =
  Dictionary M.empty

emptyList :: EnsF Ens
emptyList =
  List []

toInt :: Ens -> Either Error Int
toInt ens@(m :< _) =
  case ens of
    _ :< Int s ->
      return s
    _ ->
      raiseTypeError m ET.Int (typeOf ens)

toFloat :: Ens -> Either Error Double
toFloat ens@(m :< _) =
  case ens of
    _ :< Float s ->
      return s
    _ ->
      raiseTypeError m ET.Float (typeOf ens)

toBool :: Ens -> Either Error Bool
toBool ens@(m :< _) =
  case ens of
    _ :< Bool x ->
      return x
    _ ->
      raiseTypeError m ET.Bool (typeOf ens)

toString :: Ens -> Either Error (Hint, T.Text)
toString ens@(m :< _) =
  case ens of
    _ :< String s ->
      return (m, s)
    _ ->
      raiseTypeError m ET.String (typeOf ens)

toDictionary :: Ens -> Either Error (Hint, M.HashMap T.Text Ens)
toDictionary ens@(m :< _) =
  case ens of
    _ :< Dictionary e ->
      return (m, e)
    _ ->
      raiseTypeError m ET.Dictionary (typeOf ens)

toList :: Ens -> Either Error [Ens]
toList ens@(m :< _) =
  case ens of
    _ :< List e ->
      return e
    _ ->
      raiseTypeError m ET.List (typeOf ens)

typeOf :: Ens -> ET.EnsType
typeOf v =
  case v of
    _ :< Int _ ->
      ET.Int
    _ :< Float _ ->
      ET.Float
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

ppInt :: Int -> T.Text
ppInt i =
  T.pack (show i)

ppFloat :: Double -> T.Text
ppFloat i =
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
  showWithOffset n $ key <> " " <> ppEns n value

ppEns :: Int -> Cofree EnsF a -> T.Text
ppEns n ens = do
  case ens of
    _ :< Int i ->
      ppInt i
    _ :< Float i ->
      ppFloat i
    _ :< Bool b ->
      ppBool b
    _ :< String s ->
      ppString s
    _ :< List xs -> do
      ppList n xs
    _ :< Dictionary dict -> do
      ppDictionary n dict

ppEnsTopLevel :: Cofree EnsF a -> T.Text
ppEnsTopLevel ens = do
  ppEns 0 ens <> "\n"
