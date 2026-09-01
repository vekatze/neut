module SyntaxTree.Series
  ( Series (..),
    Separator (..),
    Container (..),
    Prefix,
    emptySeries,
    emptySeries',
    emptySeriesPC,
    emptySeriesAC,
    fromList,
    fromList',
    fromList'',
    fromListWithComment,
    pushComment,
    singleton,
    cons,
    replace,
    joinC,
    assoc,
    getContainerPair,
    getSeparator,
    extract,
    isEmpty,
    sortSeriesBy,
    nubSeriesBy,
    appendLeftBiased,
    catMaybes,
    compressEither,
    hasComment,
    SyntaxTree.Series.filter,
  )
where

import Data.Bifunctor
import Data.List (partition, sortBy)
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import SyntaxTree.C (C, Comment (..), CommentType (..), toLineComment)

data Separator
  = Comma
  | Bar
  deriving (Eq)

data Container
  = Paren
  | Brace
  | Bracket
  | Angle
  deriving (Eq)

type Prefix =
  Maybe (T.Text, C)

data Series a = Series
  { elems :: [(C, a)],
    trailingComment :: C,
    prefix :: Prefix,
    container :: Maybe Container,
    separator :: Separator,
    hasOptionalSeparator :: Bool
  }
  deriving (Eq)

instance Functor Series where
  fmap f series =
    series {elems = map (second f) (elems series)}

instance Foldable Series where
  foldMap f xs =
    case uncons xs of
      Nothing ->
        mempty
      Just ((c, v), rest) -> do
        f v <> foldMap f (pushComment c rest)

instance Traversable Series where
  traverse f xs = do
    ys <-
      traverse
        ( \(c, x) -> do
            y <- f x
            pure (c, y)
        )
        (elems xs)
    pure xs {elems = ys}

emptySeries :: Maybe Container -> Separator -> Series a
emptySeries container separator =
  Series
    { elems = [],
      trailingComment = [],
      prefix = Nothing,
      separator,
      container = container,
      hasOptionalSeparator = False
    }

emptySeries' :: Maybe Container -> Separator -> Series a
emptySeries' container separator =
  Series
    { elems = [],
      trailingComment = [],
      prefix = Nothing,
      separator,
      container,
      hasOptionalSeparator = False
    }

singleton :: Maybe Container -> Separator -> C -> (a, C) -> Series a
singleton container separator leadingComment (v, c) =
  Series
    { elems = [(leadingComment, v)],
      trailingComment = c,
      prefix = Nothing,
      separator,
      container = container,
      hasOptionalSeparator = False
    }

replace :: (Eq a) => a -> b -> Series (a, b) -> Series (a, b)
replace k v kvs =
  case uncons kvs of
    Nothing ->
      kvs
    Just ((c, (k', v')), rest)
      | k == k' ->
          cons (c, (k', v)) (replace k v rest)
      | otherwise ->
          cons (c, (k', v')) (replace k v rest)

uncons :: Series a -> Maybe ((C, a), Series a)
uncons xs =
  case elems xs of
    [] ->
      Nothing
    cv : rest ->
      Just (cv, xs {elems = rest})

cons :: (C, a) -> Series a -> Series a
cons x xs =
  xs {elems = x : elems xs}

-- pc: paren comma
emptySeriesPC :: Series a
emptySeriesPC =
  emptySeries (Just Paren) Comma

-- ac: angle comma
emptySeriesAC :: Series a
emptySeriesAC =
  emptySeries (Just Angle) Comma

fromList :: Container -> Separator -> [a] -> Series a
fromList container separator xs =
  Series
    { elems = map ([],) xs,
      trailingComment = [],
      prefix = Nothing,
      separator,
      container = Just container,
      hasOptionalSeparator = False
    }

fromList' :: [a] -> Series a
fromList' =
  fromList Paren Comma

fromList'' :: [a] -> Series a
fromList'' xs =
  Series
    { elems = map ([],) xs,
      trailingComment = [],
      prefix = Nothing,
      separator = Comma,
      container = Nothing,
      hasOptionalSeparator = False
    }

fromListWithComment :: Maybe Container -> Separator -> [(C, (a, C))] -> Series a
fromListWithComment container separator xs = do
  let (xs', trailingComment) = _assoc xs []
  Series
    { elems = xs',
      trailingComment,
      prefix = Nothing,
      separator,
      container = container,
      hasOptionalSeparator = False
    }

pushComment :: C -> Series a -> Series a
pushComment c series =
  case elems series of
    [] ->
      series {trailingComment = c ++ trailingComment series}
    (c1, x1) : rest ->
      series {elems = (c ++ c1, x1) : rest}

assoc :: Series (a, C) -> Series a
assoc series = do
  let Series {elems, trailingComment, separator, container, hasOptionalSeparator} = series
  let (elems', trailingComment') = _assoc elems trailingComment
  Series
    { elems = elems',
      trailingComment = trailingComment',
      prefix = Nothing,
      separator,
      container,
      hasOptionalSeparator
    }

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

joinC :: Series (C, a) -> Series a
joinC xs =
  xs {elems = map (\(c1, (c2, v)) -> (c1 ++ c2, v)) $ elems xs}

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
    Bar ->
      "|"

extract :: Series a -> [a]
extract series =
  map snd $ elems series

isEmpty :: Series a -> Bool
isEmpty series =
  null (elems series) && null (trailingComment series)

sortSeriesBy :: (a -> a -> Ordering) -> Series a -> Series a
sortSeriesBy cmp series = do
  let (openerComment, anchoredList, trailing) = anchorComments (elems series) (trailingComment series)
  let cmp' (_, x, _) (_, y, _) = cmp x y
  let (elems', trailing') = unanchorComments openerComment (sortBy cmp' anchoredList) trailing
  series {elems = elems', trailingComment = trailing'}

nubSeriesBy :: (a -> a -> Bool) -> Series a -> Series a
nubSeriesBy cmp series = do
  let (openerComment, anchoredList, trailing) = anchorComments (elems series) (trailingComment series)
  let (elems', trailing') = unanchorComments openerComment (nubAnchoredBy cmp anchoredList) trailing
  series {elems = elems', trailingComment = trailing'}

splitInlineComment :: C -> (C, C)
splitInlineComment c =
  case c of
    comment : rest
      | commentType comment == InlineComment ->
          ([comment], rest)
    _ ->
      ([], c)

anchorComments :: [(C, a)] -> C -> (C, [(C, a, C)], C)
anchorComments elemList trailing = do
  case elemList of
    [] -> do
      let (openerComment, trailing') = splitInlineComment trailing
      (openerComment, [], trailing')
    (c, x) : rest -> do
      let (openerComment, leading) = splitInlineComment c
      let (anchoredList, trailing') = anchorRestComments leading x rest trailing
      (openerComment, anchoredList, trailing')

anchorRestComments :: C -> a -> [(C, a)] -> C -> ([(C, a, C)], C)
anchorRestComments leading x elemList trailing = do
  case elemList of
    [] -> do
      let (inlineComment, trailing') = splitInlineComment trailing
      ([(leading, x, inlineComment)], trailing')
    (c, y) : rest -> do
      let (inlineComment, leading') = splitInlineComment c
      let (anchoredList, trailing') = anchorRestComments leading' y rest trailing
      ((leading, x, inlineComment) : anchoredList, trailing')

unanchorComments :: C -> [(C, a, C)] -> C -> ([(C, a)], C)
unanchorComments pendingComment anchoredList trailing =
  case anchoredList of
    [] ->
      ([], pendingComment ++ trailing)
    (leading, x, inlineComment) : rest -> do
      let (elemList, trailing') = unanchorComments inlineComment rest trailing
      ((pendingComment ++ leading, x) : elemList, trailing')

nubAnchoredBy :: (a -> a -> Bool) -> [(C, a, C)] -> [(C, a, C)]
nubAnchoredBy cmp anchoredList =
  case anchoredList of
    [] ->
      []
    (leading, x, inlineComment) : rest -> do
      let (dropped, kept) = partition (\(_, y, _) -> cmp x y) rest
      let absorbed = map toLineComment $ concatMap (\(l, _, i) -> l ++ i) dropped
      (leading, x, inlineComment ++ absorbed) : nubAnchoredBy cmp kept

appendLeftBiased :: Series a -> Series a -> Series a
appendLeftBiased series1 series2 = do
  Series
    { elems = elems series1 ++ elems series2,
      trailingComment = trailingComment series1 ++ trailingComment series2,
      prefix = prefix series1,
      container = container series1,
      separator = separator series1,
      hasOptionalSeparator = hasOptionalSeparator series1 || hasOptionalSeparator series2
    }

catMaybes :: Series (Maybe a) -> Series a
catMaybes series = do
  series {elems = mapMaybe distributeMaybe $ elems series}

compressEither :: Series (Either C a) -> Series a
compressEither series = do
  let (trail, elems') = compressEither' [] $ elems series
  series {elems = elems', trailingComment = trail ++ trailingComment series}

compressEither' :: C -> [(C, Either C a)] -> (C, [(C, a)])
compressEither' c xs =
  case xs of
    [] ->
      (c, [])
    (c1, y) : rest ->
      case y of
        Left c2 ->
          compressEither' (c ++ c1 ++ c2) rest
        Right e -> do
          let (trail, rest') = compressEither' [] rest
          (trail, (c ++ c1, e) : rest')

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

comments :: Series a -> C
comments series = do
  let c = concatMap fst $ elems series
  trailingComment series ++ c

hasComment :: Series a -> Bool
hasComment series = do
  not $ null $ comments series
