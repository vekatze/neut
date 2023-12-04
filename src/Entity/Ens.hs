module Entity.Ens
  ( Ens,
    EnsF (..),
    MiniEns,
    EqEns (..),
    put,
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
    merge,
    nubEnsList,
  )
where

import Control.Comonad.Cofree
import Control.Monad
import Data.Bifunctor
import Data.List (nub)
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

newtype EqEns a = EqEns (Cofree EnsF a)

instance Eq (EqEns a) where
  (EqEns ens1) == (EqEns ens2) =
    case (ens1, ens2) of
      (_ :< Int x1, _ :< Int x2) ->
        x1 == x2
      (_ :< Float x1, _ :< Float x2) ->
        x1 == x2
      (_ :< Bool x1, _ :< Bool x2) ->
        x1 == x2
      (_ :< String x1, _ :< String x2) ->
        x1 == x2
      (_ :< List xs1, _ :< List xs2) ->
        map EqEns xs1 == map EqEns xs2
      (_ :< Dictionary kvs1, _ :< Dictionary kvs2) -> do
        let kvs1' = map (second EqEns) kvs1
        let kvs2' = map (second EqEns) kvs2
        kvs1' == kvs2'
      (_ :< Comment com1 ens1', _ :< Comment com2 ens2') ->
        com1 == com2 && EqEns ens1' == EqEns ens2'
      _ ->
        False

type MiniEns = Cofree EnsF ()

getContent :: Cofree EnsF a -> Cofree EnsF a
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

put :: T.Text -> Ens -> Ens -> Either Error Ens
put k v ens = do
  (m, dict) <- toDictionary ens
  case lookup k dict of
    Just _ ->
      return $ m :< Dictionary (replace k v dict)
    Nothing ->
      return $ m :< Dictionary (dict ++ [(k, v)])

replace :: (Eq a) => a -> b -> [(a, b)] -> [(a, b)]
replace k v kvs =
  case kvs of
    [] ->
      []
    (k', v') : rest
      | k == k' ->
          (k', v) : replace k v rest
      | otherwise ->
          (k', v') : replace k v rest

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

toList :: Ens -> Either Error (Hint, [Ens])
toList ens@(m :< _) =
  case getContent ens of
    _ :< List e ->
      return (m, e)
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

-- left-biased merge
merge :: Ens -> Ens -> Either Error Ens
merge ens1 ens2 =
  case (ens1, ens2) of
    (m :< Int x1, _ :< Int _) ->
      return $ m :< Int x1
    (m :< Float x1, _ :< Float _) ->
      return $ m :< Float x1
    (m :< Bool x1, _ :< Bool _) ->
      return $ m :< Bool x1
    (m :< String x1, _ :< String _) ->
      return $ m :< String x1
    (m :< List xs1, _ :< List xs2) -> do
      return $ m :< List (xs1 ++ xs2)
    (m :< Dictionary kvs1, _ :< Dictionary kvs2) -> do
      kvs1' <- forM kvs1 $ \(k1, v1) -> do
        case lookup k1 kvs2 of
          Just v2 -> do
            v1' <- merge v1 v2
            return (k1, v1')
          Nothing ->
            return (k1, v1)
      let ks1 = map fst kvs1'
      let kvs2' = filter (\(k, _) -> k `notElem` ks1) kvs2
      return $ m :< Dictionary (kvs1' ++ kvs2')
    (m :< Comment com ens1', _) -> do
      ens <- merge ens1' ens2
      return $ m :< Comment com ens
    (m :< _, _) ->
      raiseTypeError m (typeOf ens1) (typeOf ens2)

nubEnsList :: [Ens] -> [Ens]
nubEnsList ensList =
  map (\(EqEns x) -> x) $ nub (map EqEns ensList)
