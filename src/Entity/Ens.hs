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
  )
where

import Control.Comonad.Cofree
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
  | Dictionary [(T.Text, a)]
  | Comment T.Text a

type Ens = Cofree EnsF Hint

type MiniEns = Cofree EnsF ()

getContent :: Ens -> Ens
getContent ens =
  case ens of
    _ :< Comment _ ens' ->
      getContent ens'
    _ ->
      ens

access :: T.Text -> Ens -> Either Error Ens
access k ens@(m :< _) = do
  (_, dictionary) <- toDictionary ens
  case lookup k dictionary of
    Just v ->
      return v
    Nothing ->
      raiseKeyNotFoundError m k

access' :: T.Text -> EnsF Ens -> Ens -> Either Error Ens
access' k defaultValue ens@(m :< _) = do
  (_, dictionary) <- toDictionary ens
  case lookup k dictionary of
    Just v ->
      return v
    Nothing ->
      return $ m :< defaultValue

ensPath :: Path a b -> EnsF (Cofree EnsF c)
ensPath path =
  String $ T.pack (toFilePath path)

emptyDict :: EnsF Ens
emptyDict =
  Dictionary []

emptyList :: EnsF Ens
emptyList =
  List []

toInt :: Ens -> Either Error Int
toInt ens@(m :< _) =
  case getContent ens of
    _ :< Int s ->
      return s
    _ ->
      raiseTypeError m ET.Int (typeOf ens)

toFloat :: Ens -> Either Error Double
toFloat ens@(m :< _) =
  case getContent ens of
    _ :< Float s ->
      return s
    _ ->
      raiseTypeError m ET.Float (typeOf ens)

toBool :: Ens -> Either Error Bool
toBool ens@(m :< _) =
  case getContent ens of
    _ :< Bool x ->
      return x
    _ ->
      raiseTypeError m ET.Bool (typeOf ens)

toString :: Ens -> Either Error (Hint, T.Text)
toString ens@(m :< _) =
  case getContent ens of
    _ :< String s ->
      return (m, s)
    _ ->
      raiseTypeError m ET.String (typeOf ens)

toDictionary :: Ens -> Either Error (Hint, [(T.Text, Ens)])
toDictionary ens@(m :< _) =
  case getContent ens of
    _ :< Dictionary e ->
      return (m, e)
    _ ->
      raiseTypeError m ET.Dictionary (typeOf ens)

toList :: Ens -> Either Error [Ens]
toList ens@(m :< _) =
  case getContent ens of
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
    _ :< Comment _ v' ->
      typeOf v'

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
