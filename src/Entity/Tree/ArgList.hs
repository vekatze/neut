module Entity.Tree.ArgList (getArgList) where

import Control.Comonad.Cofree
import Data.Text qualified as T
import Entity.Atom qualified as AT
import Entity.Error
import Entity.Hint
import Entity.Tree

getArgList :: Tree -> Either Error (Hint, [(Tree, Tree)])
getArgList tree =
  case tree of
    m :< List tss -> do
      tss' <- mapM (getPairListElem m) tss
      return (m, tss')
    m :< _ ->
      Left $ newError m $ "a symbol is expected, but found:\n" <> showTree tree

getPairListElem :: Hint -> [Tree] -> Either Error (Tree, Tree)
getPairListElem m ts =
  case ts of
    [t] ->
      return (t, m :< Atom (AT.Symbol "_"))
    [t1, t2] ->
      return (t1, t2)
    _ ->
      Left $ newError m $ "a list of size 1 is expected, but found a list of size " <> T.pack (show (length ts))
