module Entity.Ens
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
  )
where

import Control.Comonad.Cofree
import Control.Monad
import Data.Bifunctor
import Data.List (nubBy)
import Data.Text qualified as T
import Entity.C
import Entity.EnsType qualified as ET
import Entity.Error
import Entity.Hint
import Path

data EnsF a
  = Int Int
  | Float Double
  | Bool Bool
  | String T.Text
  | List C [(a, C)]
  | Dictionary C [(T.Text, (C, (a, C)))]

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
      (_ :< List c1 xs1, _ :< List c2 xs2) ->
        map (first EqEns) xs1 == map (first EqEns) xs2 && c1 == c2
      (_ :< Dictionary c1 kvs1, _ :< Dictionary c2 kvs2) -> do
        let (ks1, vs1) = unzip kvs1
        let (ks2, vs2) = unzip kvs2
        ks1 == ks2 && map (second $ first EqEns) vs1 == map (second $ first EqEns) vs2 && c1 == c2
      _ ->
        False

hasKey :: T.Text -> Ens -> Bool
hasKey k ens =
  case ens of
    _ :< Dictionary _ kvs
      | Just _ <- lookup k kvs ->
          True
    _ ->
      False

access :: T.Text -> Ens -> Either Error (C, (Ens, C))
access k ens@(m :< _) = do
  (_, _, dictionary) <- toDictionary ens
  case lookup k dictionary of
    Just v ->
      return v
    Nothing ->
      raiseKeyNotFoundError m k

access' :: T.Text -> EnsF Ens -> Ens -> Either Error (C, (Ens, C))
access' k defaultValue ens@(m :< _) = do
  (_, _, dictionary) <- toDictionary ens
  case lookup k dictionary of
    Just v ->
      return v
    Nothing ->
      return ([], (m :< defaultValue, []))

strip :: (C, (Ens, C)) -> Ens
strip (_, (ens, _)) =
  ens

put :: T.Text -> Ens -> Ens -> Either Error Ens
put k v ens = do
  (m, c, dict) <- toDictionary ens
  case lookup k dict of
    Just (c1, (_, c2)) ->
      return $ m :< Dictionary c (replace k (c1, (v, c2)) dict)
    Nothing ->
      return $ m :< Dictionary c (dict ++ [(k, ([], (v, [])))])

conservativeUpdate :: [T.Text] -> Ens -> Ens -> Either Error Ens
conservativeUpdate keyPath value ens = do
  case keyPath of
    [] ->
      return value
    k : rest -> do
      (m, c, dictionary) <- toDictionary ens
      dictionary' <- forM dictionary $ \(kd, (c1, (vd, c2))) -> do
        if k == kd
          then do
            vd' <- conservativeUpdate rest value vd
            return (kd, (c1, (vd', c2)))
          else return (kd, (c1, (vd, c2)))
      return $ m :< Dictionary c dictionary'

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
  Dictionary [] []

emptyList :: EnsF Ens
emptyList =
  List [] []

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

toDictionary :: Ens -> Either Error (Hint, C, [(T.Text, (C, (Ens, C)))])
toDictionary ens@(m :< _) =
  case ens of
    _ :< Dictionary c kvs ->
      return (m, c, kvs)
    _ ->
      raiseTypeError m ET.Dictionary (typeOf ens)

toList :: Ens -> Either Error (Hint, C, [(Ens, C)])
toList ens@(m :< _) =
  case ens of
    _ :< List c e ->
      return (m, c, e)
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
    (m :< List c1 xs1, _ :< List c2 xs2) -> do
      return $ m :< List (c1 ++ c2) (xs1 ++ xs2)
    (m :< Dictionary c1 kvs1, _ :< Dictionary c2 kvs2) -> do
      kvs1' <- forM kvs1 $ \(k1, (cLead1, (v1, cTrail1))) -> do
        case lookup k1 kvs2 of
          Just (cLead2, (v2, cTrail2)) -> do
            v1' <- merge v1 v2
            return (k1, (cLead1 ++ cLead2, (v1', cTrail1 ++ cTrail2)))
          Nothing ->
            return (k1, (cLead1, (v1, cTrail1)))
      let ks1 = map fst kvs1'
      let kvs2' = filter (\(k, _) -> k `notElem` ks1) kvs2
      return $ m :< Dictionary (c1 ++ c2) (kvs1' ++ kvs2')
    (m :< _, _) ->
      raiseTypeError m (typeOf ens1) (typeOf ens2)

nubEnsList :: [(Ens, C)] -> [(Ens, C)]
nubEnsList ensList = do
  nubBy (\(e1, _) (e2, _) -> EqEns e1 == EqEns e2) ensList

inject :: Ens -> FullEns
inject ens =
  ([], (ens, []))
