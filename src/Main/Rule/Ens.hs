module Main.Rule.Ens
  ( Ens,
    EnsF (..),
    TopEns,
    EqEns (..),
    FullEns,
    put,
    conservativeUpdate,
    strip,
    hasKey,
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
    inject,
    dictFromList,
    dictFromList',
    dictFromListVertical,
    dictFromListVertical',
  )
where

import Control.Comonad.Cofree
import Control.Monad
import Data.Bifunctor
import Data.List (find, nubBy)
import Data.Text qualified as T
import Language.Common.Rule.Error
import Language.Common.Rule.Hint
import Main.Rule.EnsType qualified as ET
import Path
import SyntaxTree.Rule.C
import SyntaxTree.Rule.Series qualified as SE

data EnsF a
  = Int Int
  | Float Double
  | Bool Bool
  | String T.Text
  | List (SE.Series a)
  | Dictionary (SE.Series (T.Text, a))

type Ens = Cofree EnsF Hint

type TopEns = (Ens, C)

type FullEns = (C, (Ens, C))

newtype EqEns a = EqEns (Cofree EnsF a)

instance Eq (EqEns Hint) where
  EqEns ens1 == EqEns ens2 =
    case (ens1, ens2) of
      (_ :< Int x1, _ :< Int x2) ->
        x1 == x2
      (_ :< Float x1, _ :< Float x2) ->
        x1 == x2
      (_ :< Bool x1, _ :< Bool x2) ->
        x1 == x2
      (_ :< String x1, _ :< String x2) ->
        x1 == x2
      (_ :< List xs1, _ :< List xs2) -> do
        let b1 = map (second EqEns) (SE.elems xs1) == map (second EqEns) (SE.elems xs2)
        let b2 = SE.trailingComment xs1 == SE.trailingComment xs2
        let b3 = SE.prefix xs1 == SE.prefix xs2
        b1 && b2 && b3
      (_ :< Dictionary kvs1, _ :< Dictionary kvs2) -> do
        let kvs1' = fmap (second EqEns) kvs1
        let kvs2' = fmap (second EqEns) kvs2
        kvs1' == kvs2'
      _ ->
        False

hasKey :: T.Text -> Ens -> Bool
hasKey k ens =
  case ens of
    _ :< Dictionary kvsEns
      | kvs <- SE.extract kvsEns,
        Just _ <- lookup k kvs ->
          True
    _ ->
      False

access :: T.Text -> Ens -> Either Error Ens
access k ens@(m :< _) = do
  (_, dictionary) <- toDictionary ens
  case lookup k (SE.extract dictionary) of
    Just v ->
      return v
    Nothing ->
      raiseKeyNotFoundError m k

access' :: T.Text -> EnsF Ens -> Ens -> Either Error Ens
access' k defaultValue ens@(m :< _) = do
  (_, dictionary) <- toDictionary ens
  case lookup k (SE.extract dictionary) of
    Just v ->
      return v
    Nothing ->
      return $ m :< defaultValue

strip :: (C, (Ens, C)) -> Ens
strip (_, (ens, _)) =
  ens

put :: T.Text -> Ens -> Ens -> Either Error Ens
put k v ens = do
  (m, dict) <- toDictionary ens
  return $ m :< Dictionary (SE.replace k v dict)

conservativeUpdate :: [T.Text] -> Ens -> Ens -> Either Error Ens
conservativeUpdate keyPath value ens = do
  case keyPath of
    [] ->
      return value
    k : rest -> do
      (m, dictionary) <- toDictionary ens
      dictionary' <- forM dictionary $ \(kd, vd) -> do
        if k == kd
          then do
            vd' <- conservativeUpdate rest value vd
            return (kd, vd')
          else return (kd, vd)
      return $ m :< Dictionary dictionary'

ensPath :: Path a b -> EnsF (Cofree EnsF c)
ensPath path =
  String $ T.pack (toFilePath path)

emptyDict :: EnsF Ens
emptyDict =
  Dictionary (SE.emptySeries (Just SE.Brace) SE.Comma)

emptyList :: EnsF Ens
emptyList =
  List (SE.emptySeries (Just SE.Bracket) SE.Comma)

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

toBool :: Ens -> Either Error (Hint, Bool)
toBool ens@(m :< _) =
  case ens of
    _ :< Bool x ->
      return (m, x)
    _ ->
      raiseTypeError m ET.Bool (typeOf ens)

toString :: Ens -> Either Error (Hint, T.Text)
toString ens@(m :< _) =
  case ens of
    _ :< String s ->
      return (m, s)
    _ ->
      raiseTypeError m ET.String (typeOf ens)

toDictionary :: Ens -> Either Error (Hint, SE.Series (T.Text, Ens))
toDictionary ens@(m :< _) =
  case ens of
    _ :< Dictionary kvs ->
      return (m, kvs)
    _ ->
      raiseTypeError m ET.Dictionary (typeOf ens)

toList :: Ens -> Either Error (Hint, SE.Series Ens)
toList ens@(m :< _) =
  case ens of
    _ :< List e ->
      return (m, e)
    _ ->
      raiseTypeError m ET.List (typeOf ens)

typeOf :: Ens -> ET.EnsType
typeOf v =
  case v of
    _ :< Int {} ->
      ET.Int
    _ :< Float {} ->
      ET.Float
    _ :< Bool {} ->
      ET.Bool
    _ :< String {} ->
      ET.String
    _ :< List {} ->
      ET.List
    _ :< Dictionary {} ->
      ET.Dictionary

raiseTypeError :: Hint -> ET.EnsType -> ET.EnsType -> Either Error a
raiseTypeError m expectedType actualType =
  Left $
    newError m $
      "The value here is expected to be of type `"
        <> ET.showEnsType expectedType
        <> "`, but is: `"
        <> ET.showEnsType actualType
        <> "`"

raiseKeyNotFoundError :: Hint -> T.Text -> Either Error a
raiseKeyNotFoundError m k =
  Left $
    newError m $
      "Could not find the required key `"
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
      return $ m :< List (SE.appendLeftBiased xs1 xs2)
    (m :< Dictionary s1, _ :< Dictionary s2) -> do
      let kvs1 = SE.elems s1
      let kvs2 = SE.elems s2
      kvs1' <- forM kvs1 $ \(c1, (k1, v1)) -> do
        case find (\(_, (k, _)) -> k1 == k) kvs2 of
          Just (c2, (_, v2)) -> do
            v1' <- merge v1 v2
            return (c1 ++ c2, (k1, v1'))
          Nothing ->
            return (c1, (k1, v1))
      let ks1 = map (fst . snd) kvs1'
      let kvs2' = filter (\(_, (k, _)) -> k `notElem` ks1) kvs2
      return $ m :< Dictionary (s1 {SE.elems = kvs1' ++ kvs2'})
    (m :< _, _) ->
      raiseTypeError m (typeOf ens1) (typeOf ens2)

nubEnsList :: [(Ens, C)] -> [(Ens, C)]
nubEnsList ensList = do
  nubBy (\(e1, _) (e2, _) -> EqEns e1 == EqEns e2) ensList

inject :: Ens -> FullEns
inject ens =
  ([], (ens, []))

dictFromList :: a -> [(T.Text, Cofree EnsF a)] -> Cofree EnsF a
dictFromList m xs = do
  m :< dictFromList' xs

dictFromList' :: [(T.Text, Cofree EnsF a)] -> EnsF (Cofree EnsF a)
dictFromList' xs = do
  Dictionary (SE.fromList SE.Brace SE.Comma xs)

dictFromListVertical :: a -> [(T.Text, Cofree EnsF a)] -> Cofree EnsF a
dictFromListVertical m xs = do
  m :< dictFromListVertical' xs

dictFromListVertical' :: [(T.Text, Cofree EnsF a)] -> EnsF (Cofree EnsF a)
dictFromListVertical' xs = do
  Dictionary ((SE.fromList SE.Brace SE.Comma xs) {SE.hasOptionalSeparator = True})
