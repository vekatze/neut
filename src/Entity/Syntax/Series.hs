module Entity.Syntax.Series
  ( Series (..),
    Separator (..),
    Container (..),
    Prefix,
    emptySeries,
    emptySeries',
    emptySeriesPC,
    fromList,
    fromList',
    fromList'',
    fromListWithComment,
    pushComment,
    assoc,
    getContainerPair,
    getSeparator,
    extract,
    isEmpty,
    containsNoComment,
    sortSeriesBy,
    appendLeftBiased,
    catMaybes,
    Entity.Syntax.Series.filter,
  )
where

import Data.Bifunctor
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Entity.C (C)

data Separator
  = Comma
  | Hyphen

data Container
  = Paren
  | Brace
  | Bracket
  | Angle

type Prefix =
  Maybe (T.Text, C)

data Series a = Series
  { elems :: [(C, a)],
    trailingComment :: C,
    prefix :: Prefix,
    container :: Maybe Container,
    separator :: Separator
  }

instance Functor Series where
  fmap f series =
    series {elems = map (second f) (elems series)}

emptySeries :: Container -> Separator -> Series a
emptySeries container separator =
  Series {elems = [], trailingComment = [], prefix = Nothing, separator, container = Just container}

emptySeries' :: Maybe Container -> Separator -> Series a
emptySeries' container separator =
  Series {elems = [], trailingComment = [], prefix = Nothing, separator, container}

-- pc: paren comma
emptySeriesPC :: Series a
emptySeriesPC =
  emptySeries Paren Comma

fromList :: Container -> Separator -> [a] -> Series a
fromList container separator xs =
  Series {elems = map ([],) xs, trailingComment = [], prefix = Nothing, separator, container = Just container}

fromList' :: [a] -> Series a
fromList' =
  fromList Paren Comma

fromList'' :: [a] -> Series a
fromList'' xs =
  Series {elems = map ([],) xs, trailingComment = [], prefix = Nothing, separator = Comma, container = Nothing}

fromListWithComment :: Container -> Separator -> [(C, (a, C))] -> Series a
fromListWithComment container separator xs = do
  let (xs', trailingComment) = _assoc xs []
  Series {elems = xs', trailingComment, prefix = Nothing, separator, container = Just container}

pushComment :: C -> Series a -> Series a
pushComment c series =
  case elems series of
    [] ->
      series {trailingComment = c ++ trailingComment series}
    (c1, x1) : rest ->
      series {elems = (c ++ c1, x1) : rest}

assoc :: Series (a, C) -> Series a
assoc series = do
  let Series {elems, trailingComment, separator, container} = series
  let (elems', trailingComment') = _assoc elems trailingComment
  Series {elems = elems', trailingComment = trailingComment', prefix = Nothing, separator, container}

_assoc :: [(C, (a, C))] -> C -> ([(C, a)], C)
_assoc es c =
  case es of
    [] ->
      ([], c)
    [(c1, (e, c2))] ->
      ([(c1, e)], c2 ++ c)
    (c11, (e1, c12)) : (c21, (e2, c22)) : rest -> do
      let (_assoced, c') = _assoc ((c12 ++ c21, (e2, c22)) : rest) c
      ((c11, e1) : _assoced, c')

getContainerPair :: Container -> (T.Text, T.Text)
getContainerPair container =
  case container of
    Paren ->
      ("(", ")")
    Brace ->
      ("{", "}")
    Bracket ->
      ("[", "]")
    Angle ->
      ("<", ">")

getSeparator :: Separator -> T.Text
getSeparator sep =
  case sep of
    Comma ->
      ","
    Hyphen ->
      "-"

extract :: Series a -> [a]
extract series =
  map snd $ elems series

isEmpty :: Series a -> Bool
isEmpty series =
  null (elems series) && null (trailingComment series)

containsNoComment :: Series a -> Bool
containsNoComment series = do
  let cs = map fst $ elems series
  let c = trailingComment series
  all null (c : cs)

sortSeriesBy :: (a -> a -> Ordering) -> Series a -> Series a
sortSeriesBy cmp series = do
  let cmp' (_, x) (_, y) = cmp x y
  series {elems = sortBy cmp' $ elems series}

appendLeftBiased :: Series a -> Series a -> Series a
appendLeftBiased series1 series2 = do
  Series
    { elems = elems series1 ++ elems series2,
      trailingComment = trailingComment series1 ++ trailingComment series2,
      prefix = prefix series1,
      container = container series1,
      separator = separator series1
    }

catMaybes :: Series (Maybe a) -> Series a
catMaybes series = do
  series {elems = mapMaybe distributeMaybe $ elems series}

filter :: (a -> Bool) -> Series a -> Series a
filter p series = do
  series {elems = Prelude.filter (\(_, a) -> p a) $ elems series}

distributeMaybe :: (a, Maybe b) -> Maybe (a, b)
distributeMaybe (a, mb) =
  case mb of
    Just b ->
      Just (a, b)
    Nothing ->
      Nothing
