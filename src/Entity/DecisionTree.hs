module Entity.DecisionTree
  ( DecisionTree (..),
    CaseList,
    Case (..),
    getConstructors,
  )
where

import Data.Binary
import Entity.Binder
import qualified Entity.DefiniteDescription as DD
import qualified Entity.Discriminant as D
import Entity.Ident
import GHC.Generics (Generic)

data DecisionTree a
  = Leaf [Ident] a
  | Unreachable
  | Switch (Ident, a) (CaseList a)
  deriving (Show, Generic)

data Case a
  = Cons DD.DefiniteDescription D.Discriminant [a] [BinderF a] (DecisionTree a)
  deriving (Show, Generic)

type CaseList a = (DecisionTree a, [Case a])

instance (Binary a) => Binary (DecisionTree a)

instance (Binary a) => Binary (Case a)

getConstructors :: [Case a] -> [DD.DefiniteDescription]
getConstructors clauseList = do
  map getConstructor clauseList

getConstructor :: Case a -> DD.DefiniteDescription
getConstructor (Cons name _ _ _ _) = name
