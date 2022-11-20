module Entity.DecisionTree where

import Data.Binary
import Entity.Arity
import Entity.Binder
import qualified Entity.DefiniteDescription as DD
import Entity.Ident
import GHC.Generics (Generic)

data DecisionTree a
  = Leaf [Ident] a
  | Unreachable
  | Switch (CaseList a)
  deriving (Show, Generic)

data Case a
  = Cons DD.DefiniteDescription Arity [BinderF a] (DecisionTree a)
  deriving (Show, Generic)

type DefaultDecisionTree a = DecisionTree a

type CaseList a = (DecisionTree a, [Case a])

instance (Binary a) => Binary (DecisionTree a)

instance (Binary a) => Binary (Case a)
