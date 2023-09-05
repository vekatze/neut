module Entity.DecisionTree
  ( DecisionTree (..),
    CaseList,
    Case (..),
    getConstructors,
    isUnreachable,
  )
where

import Data.Binary
import Entity.Binder
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.Hint
import Entity.Ident
import GHC.Generics (Generic)

data DecisionTree a
  = Leaf [Ident] a
  | Unreachable
  | Switch (Ident, a) (CaseList a)
  deriving (Show, Generic)

type CaseList a = (DecisionTree a, [Case a])

data Case a
  = Cons Hint DD.DefiniteDescription D.Discriminant [(a, a)] [BinderF a] (DecisionTree a)
  deriving (Show, Generic)

instance (Binary a) => Binary (DecisionTree a)

instance (Binary a) => Binary (Case a)

getConstructors :: [Case a] -> [DD.DefiniteDescription]
getConstructors clauseList = do
  map getConstructor clauseList

getConstructor :: Case a -> DD.DefiniteDescription
getConstructor decisionCase =
  case decisionCase of
    Cons _ name _ _ _ _ ->
      name

isUnreachable :: DecisionTree a -> Bool
isUnreachable tree =
  case tree of
    Unreachable ->
      True
    _ ->
      False
