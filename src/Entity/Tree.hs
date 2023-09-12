module Entity.Tree where

import Control.Comonad.Cofree
import Data.HashMap.Strict qualified as M
import Data.Text qualified as T
import Entity.Atom (asAttrKey)
import Entity.Atom qualified as AT
import Entity.Error
import Entity.Hint
import Entity.RawIdent qualified as RI
import Text.Read (readMaybe)

data TreeF a
  = Atom AT.Atom
  | Node [a]
  | List [[a]]
  deriving (Show)

type Tree = Cofree TreeF Hint

type MiniTree = Cofree TreeF ()

type TreeListF a = (a, [Tree])

type TreeList = (Hint, [Tree])

showTree :: Cofree TreeF a -> T.Text
showTree =
  ppTree 0

access :: Hint -> T.Text -> [Tree] -> Either Error TreeList
access m k treeList = do
  case treeList of
    [] ->
      raiseKeyNotFoundError m k
    tree : rest -> do
      (k', (mResult, cdr)) <- extractKeyValuePair tree
      if k == k'
        then return (mResult, cdr)
        else access m k rest

getHeadSym :: Tree -> Maybe T.Text
getHeadSym tree =
  case tree of
    _ :< Node ((_ :< Atom (AT.Symbol sym')) : _) ->
      Just sym'
    _ ->
      Nothing

getHeadSym' :: TreeList -> Maybe T.Text
getHeadSym' (_, treeList) =
  case treeList of
    [] ->
      Nothing
    t : _ ->
      getHeadSym t

headSymEq :: T.Text -> Tree -> Bool
headSymEq sym tree =
  case tree of
    _ :< Node ((_ :< Atom (AT.Symbol sym')) : _)
      | sym == sym' ->
          True
    _ ->
      False

access' :: T.Text -> Tree -> Either Error TreeList
access' k tree@(m :< _) = do
  (car, cdr) <- extractKeyValuePair tree
  if k == car
    then return cdr
    else raiseKeyNotFoundError' m k tree

chunk :: T.Text -> Tree -> Either Error ()
chunk expected t =
  case t of
    _ :< Atom (AT.Symbol s)
      | s == expected ->
          return ()
    m :< _ ->
      Left $ newError m $ "expected `" <> expected <> "`, but found:\n" <> showTree t

wrap :: Hint -> T.Text -> [Tree] -> Tree
wrap m text ts =
  m :< Node ((m :< Atom (AT.Symbol text)) : ts)

raiseKeyNotFoundError' :: Hint -> T.Text -> Tree -> Either Error a
raiseKeyNotFoundError' m k t =
  Left $
    newError m $
      "expected `" <> k <> "`, but found:\n" <> showTree t

accessOrEmpty :: Hint -> T.Text -> [Tree] -> Either Error TreeList
accessOrEmpty m k treeList = do
  case treeList of
    [] ->
      return (m, [])
    tree : rest -> do
      (k', (mResult, cdr)) <- extractKeyValuePair tree
      if k == k'
        then return (mResult, cdr)
        else accessOrEmpty m k rest

extractValueByKey :: Hint -> T.Text -> [Tree] -> Either Error Tree
extractValueByKey m k treeList = do
  case treeList of
    [] ->
      Left $ newError m $ "no such key found: " <> k
    tree : rest -> do
      (k', (mResult, cdr)) <- extractKeyValuePair tree
      if k == k'
        then case cdr of
          [t] ->
            return t
          _ ->
            Left $
              newError mResult $
                "the value for the key `" <> k <> "` must be atomic, but found:\n" <> ppTreeList (m, cdr)
        else extractValueByKey m k rest

extract :: TreeList -> Either Error Tree
extract (m, ts) = do
  case ts of
    [t] ->
      return t
    _ ->
      Left $ newError m $ "an atomic tree is expected, but found:\n" <> ppTreeList (m, ts)

getSymbol :: Tree -> Either Error (Hint, T.Text)
getSymbol tree =
  case tree of
    m :< Atom (AT.Symbol s) ->
      return (m, s)
    m :< _ ->
      Left $ newError m $ "a symbol is expected, but found:\n" <> showTree tree

getInt :: Tree -> Maybe (Hint, Int)
getInt tree =
  case tree of
    m :< Atom (AT.Symbol s)
      | Just i <- readMaybe (T.unpack s) ->
          return (m, i)
    _ ->
      Nothing

getFloat :: Tree -> Maybe (Hint, Double)
getFloat tree =
  case tree of
    m :< Atom (AT.Symbol s)
      | Just i <- readMaybe (T.unpack s) ->
          return (m, i)
    _ ->
      Nothing

toString :: Tree -> Either Error (Hint, T.Text)
toString tree =
  case tree of
    m :< Atom (AT.String s) ->
      return (m, s)
    m :< _ ->
      Left $ newError m $ "a string is expected, but found:\n" <> showTree tree

isList :: Tree -> Bool
isList t =
  case t of
    _ :< List {} ->
      True
    _ ->
      False

splitAttrs :: [Tree] -> ([Tree], M.HashMap T.Text Tree)
splitAttrs ts =
  case ts of
    [] ->
      ([], M.empty)
    [t] ->
      ([t], M.empty)
    (_ :< Atom atom) : t2 : rest
      | Just key <- asAttrKey atom -> do
          let (rest', attrs) = splitAttrs rest
          (rest', M.insert key t2 attrs)
    t1 : t2 : rest -> do
      let (tree, attrs) = splitAttrs $ t2 : rest
      (t1 : tree, attrs)

toNode :: Tree -> Either Error (Hint, [Tree])
toNode tree =
  case tree of
    m :< Node ts ->
      return (m, ts)
    m :< _ ->
      Left $ newError m $ "a node is expected, but found:\n" <> showTree tree

toList :: Tree -> Either Error (Hint, [[Tree]])
toList tree =
  case tree of
    m :< List ts ->
      return (m, ts)
    m :< _ ->
      Left $ newError m $ "a list is expected, but found:\n" <> showTree tree

toList1 :: Tree -> Either Error (Hint, [Tree])
toList1 tree =
  case tree of
    m :< List tss -> do
      tss' <- mapM (getSingleListElem' m) tss
      return (m, tss')
    m :< _ ->
      Left $ newError m $ "a symbol is expected, but found:\n" <> showTree tree

getSingleListElem' :: Hint -> [Tree] -> Either Error Tree
getSingleListElem' m ts =
  case ts of
    [t] ->
      return t
    _ ->
      Left $ newError m $ "a list of size 1 is expected, but found a list of size " <> T.pack (show (length ts))

toDictionary :: [Tree] -> Either Error (M.HashMap T.Text TreeList)
toDictionary ts = do
  kvp <- mapM extractKeyValuePair ts
  return $ M.fromList kvp

extractKeyValuePair :: Tree -> Either Error (RI.RawIdent, TreeList)
extractKeyValuePair t =
  case t of
    _ :< Node ((m :< Atom (AT.Symbol key)) : cdr) ->
      return (key, (m, cdr))
    m :< _ ->
      Left $ newError m $ "a key-value pair must be of the form `(key value)`, but found:\n" <> showTree t

raiseKeyNotFoundError :: Hint -> T.Text -> Either Error a
raiseKeyNotFoundError m k =
  Left $
    newError m $
      "couldn't find the required key `"
        <> k
        <> "`."

showWithOffset :: Int -> T.Text -> T.Text
showWithOffset n text =
  T.replicate n " " <> text

isAtomic :: Cofree TreeF a -> Bool
isAtomic t =
  case t of
    _ :< Atom _ ->
      True
    _ :< Node _ ->
      False
    _ :< List _ ->
      False

ppAtom :: AT.Atom -> T.Text
ppAtom atom =
  case atom of
    AT.Symbol x ->
      x
    AT.String x ->
      T.pack $ show x

ppNode :: Int -> [Cofree TreeF a] -> T.Text
ppNode n ts = do
  case ts of
    [] ->
      "()"
    [t] ->
      "(" <> ppTree n t <> ")"
    [t1, t2]
      | isAtomic t1,
        isAtomic t2 ->
          "(" <> ppTree n t1 <> " " <> ppTree n t2 <> ")"
    t : rest -> do
      let header = "("
      let rest' = map (showWithOffset (n + 2) . ppTree (n + 2)) rest
      let footer = ")"
      header <> ppTree n t <> "\n" <> T.intercalate "\n" rest' <> footer

ppList :: Int -> [[Cofree TreeF a]] -> T.Text
ppList n tss = do
  case tss of
    [] ->
      "[]"
    [] : rest ->
      ppList n rest
    hs : rest -> do
      let header = "["
      let rest' = map (showWithOffset (n + 1) . ppList' (n + 1)) rest
      let footer = "]"
      header <> ppList' n hs <> ",\n" <> T.intercalate ",\n" rest' <> footer

ppList' :: Int -> [Cofree TreeF a] -> T.Text
ppList' n ts =
  T.intercalate " " (map (ppTree n) ts)

ppDictionaryEntry :: Int -> T.Text -> Cofree TreeF a -> T.Text
ppDictionaryEntry n key value = do
  showWithOffset n $ key <> " = " <> ppTree n value

ppTree :: Int -> Cofree TreeF a -> T.Text
ppTree n entity = do
  case entity of
    _ :< Atom x ->
      ppAtom x
    _ :< Node ts ->
      ppNode n ts
    _ :< List ts ->
      ppList n ts

ppTreeList :: (a, [Cofree TreeF a]) -> T.Text
ppTreeList (_, ts) = do
  if null ts
    then "(empty)"
    else do
      let ts' = map (showWithOffset 0 . showTree) ts
      T.intercalate "\n" ts' <> "\n"
