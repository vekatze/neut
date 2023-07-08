module Entity.Tree where

import Control.Comonad.Cofree
import Data.HashMap.Strict qualified as M
import Data.Text qualified as T
import Entity.Error
import Entity.Hint
import Entity.RawIdent qualified as RI

data TreeF a
  = Symbol RI.RawIdent
  | Node [a]
  | String T.Text
  deriving (Show)

type Tree = Cofree TreeF Hint

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

extract :: TreeList -> Either Error Tree
extract (m, ts) = do
  case ts of
    [t] ->
      return t
    _ ->
      Left $ newError m $ "an atomic tree is expected, but found:\n" <> ppTreeList (m, ts)

toString :: Tree -> Either Error (Hint, T.Text)
toString tree =
  case tree of
    m :< String s ->
      return (m, s)
    m :< _ ->
      Left $ newError m $ "a string is expected, but found:\n" <> showTree tree

toDictionary :: [Tree] -> Either Error (M.HashMap T.Text TreeList)
toDictionary ts = do
  kvp <- mapM extractKeyValuePair ts
  return $ M.fromList kvp

extractKeyValuePair :: Tree -> Either Error (RI.RawIdent, TreeList)
extractKeyValuePair t =
  case t of
    _ :< Node ((m :< Symbol key) : cdr) ->
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
  T.replicate n "  " <> text

isAtomic :: Cofree TreeF a -> Bool
isAtomic t =
  case t of
    _ :< Symbol _ ->
      True
    _ :< String _ ->
      True
    _ :< Node _ ->
      False

ppSymbol :: RI.RawIdent -> T.Text
ppSymbol x =
  x

ppString :: T.Text -> T.Text
ppString x =
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
      let rest' = map (showWithOffset (n + 1) . ppTree (n + 1)) rest
      let footer = ")"
      header <> ppTree n t <> "\n" <> T.intercalate "\n" rest' <> footer

ppDictionaryEntry :: Int -> T.Text -> Cofree TreeF a -> T.Text
ppDictionaryEntry n key value = do
  showWithOffset n $ key <> " = " <> ppTree n value

ppTree :: Int -> Cofree TreeF a -> T.Text
ppTree n entity = do
  case entity of
    _ :< Symbol x ->
      ppSymbol x
    _ :< Node ts ->
      ppNode n ts
    _ :< String s ->
      ppString s

ppTreeList :: (a, [Cofree TreeF a]) -> T.Text
ppTreeList (_, ts) = do
  let ts' = map (showWithOffset 0 . showTree) ts
  T.intercalate "\n" ts' <> "\n"
