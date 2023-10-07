module Entity.Tree where

import Control.Comonad.Cofree
import Data.Binary
import Data.HashMap.Strict qualified as M
import Data.Text qualified as T
import Entity.Atom (asAttrKey)
import Entity.Atom qualified as AT
import Entity.Error
import Entity.Hint
import Entity.RawIdent qualified as RI
import GHC.Generics
import Text.Read (readMaybe)

data TreeF a
  = Atom AT.Atom
  | Node [a]
  | List [a]
  deriving (Generic)

type Tree = Cofree TreeF Hint

type MiniTree = Cofree TreeF ()

type TreeListF a = (a, [Tree])

type TreeList = (Hint, [Tree])

instance Binary a => Binary (TreeF a)

instance Binary a => Binary (Cofree TreeF a)

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

toList :: Tree -> Either Error (Hint, [Tree])
toList tree =
  case tree of
    m :< List ts ->
      return (m, ts)
    m :< _ ->
      Left $ newError m $ "a list is expected, but found:\n" <> showTree tree

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

treeUncons :: Hint -> [Tree] -> EE (Tree, [Tree])
treeUncons m ts =
  case ts of
    [] ->
      Left $ newError m "expected an non-empty list, but found an empty list"
    h : rest ->
      return (h, rest)

getTreeListOfSize1 :: TreeList -> EE Tree
getTreeListOfSize1 (m, ts) =
  case ts of
    [t] ->
      return t
    _ ->
      Left $ newError m $ "expected 1 tree, but found " <> T.pack (show (length ts)) <> "."

getTreeListOfSize2 :: TreeList -> EE (Tree, Tree)
getTreeListOfSize2 (m, ts) =
  case ts of
    [t1, t2] ->
      return (t1, t2)
    _ ->
      Left $ newError m $ "expected 2 trees, but found " <> T.pack (show (length ts)) <> "."

getTreeListOfSize3 :: TreeList -> EE (Tree, Tree, Tree)
getTreeListOfSize3 (m, ts) =
  case ts of
    [t1, t2, t3] ->
      return (t1, t2, t3)
    _ ->
      Left $ newError m $ "expected 3 trees, but found " <> T.pack (show (length ts)) <> "."

ppList :: Int -> [Cofree TreeF a] -> T.Text
ppList n ts = do
  let header = "["
  let rest' = map (showWithOffset (n + 1) . ppTree (n + 1)) ts
  let footer = "]"
  header <> T.intercalate ",\n" rest' <> footer

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

isArrow :: Tree -> Bool
isArrow =
  isSym "->"

isSym :: T.Text -> Tree -> Bool
isSym sym t =
  case t of
    (_ :< Atom (AT.Symbol sym'))
      | sym == sym' ->
          True
    _ ->
      False

breakAtSym :: Hint -> T.Text -> [Tree] -> EE ([Tree], TreeList)
breakAtSym m sym ts =
  case ts of
    [] ->
      Left $ newError m $ "couldn't find `" <> sym <> "`"
    t : rest -> do
      if isSym sym t
        then do
          case rest of
            [] ->
              Left $ newError m $ "unexpected end of input after `" <> sym <> "`"
            (mHead :< _) : _ -> do
              return ([], (mHead, rest))
        else do
          (before, after) <- breakAtSym m sym rest
          return (t : before, after)

reflSepArgs :: Hint -> T.Text -> [Tree] -> EE ([Tree], Tree, Tree)
reflSepArgs m sym ts = do
  (beforeSym, afterSym) <- breakAtSym m sym ts
  (t1, t2) <- getTreeListOfSize2 afterSym
  return (beforeSym, t1, t2)

reflSepArgs' :: Hint -> T.Text -> [Tree] -> EE ([Tree], Tree)
reflSepArgs' m sym ts = do
  (beforeSym, afterSym) <- breakAtSym m sym ts
  t <- getTreeListOfSize1 afterSym
  return (beforeSym, t)

reflSepArgs'' :: Hint -> T.Text -> [Tree] -> EE ([Tree], [Tree])
reflSepArgs'' m sym ts = do
  (beforeSym, afterSym) <- breakAtSym m sym ts
  return (beforeSym, snd afterSym)

reflArrowArgs :: Hint -> [Tree] -> EE ([Tree], Tree, Tree)
reflArrowArgs m ts = do
  reflSepArgs m "->" ts

reflArrowArgs' :: Hint -> [Tree] -> EE ([Tree], Tree)
reflArrowArgs' m ts = do
  reflSepArgs' m "->" ts

reflArrowArgs'' :: Hint -> [Tree] -> EE ([Tree], [Tree])
reflArrowArgs'' m ts = do
  reflSepArgs'' m "->" ts

miniShow :: Tree -> T.Text
miniShow t =
  case t of
    _ :< Atom atom ->
      ppAtom atom
    _ :< Node ts ->
      "(" <> T.intercalate " " (map miniShow ts) <> ")"
    _ :< List ts ->
      "[" <> T.intercalate " " (map miniShow ts) <> "]"
