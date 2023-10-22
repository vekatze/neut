module Entity.DecisionTree
  ( DecisionTree (..),
    CaseList,
    Case (..),
    getConstructors,
    isUnreachable,
    findCase,
  )
where

import Data.Binary
import Entity.Binder
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.Hint
import Entity.Ident
import Entity.IsConstLike
import GHC.Generics (Generic)

data DecisionTree a
  = Leaf [Ident] a
  | Unreachable
  | Switch (Ident, a) (CaseList a)
  deriving (Show, Generic)

type CaseList a = (DecisionTree a, [Case a])

data Case a = Case
  { mCons :: Hint,
    consDD :: DD.DefiniteDescription,
    isConstLike :: IsConstLike,
    disc :: D.Discriminant,
    dataArgs :: [(a, a)],
    consArgs :: [BinderF a],
    cont :: DecisionTree a
  }
  deriving (Show, Generic)

instance (Binary a) => Binary (DecisionTree a)

instance (Binary a) => Binary (Case a)

getConstructors :: [Case a] -> [DD.DefiniteDescription]
getConstructors clauseList = do
  map consDD clauseList

isUnreachable :: DecisionTree a -> Bool
isUnreachable tree =
  case tree of
    Unreachable ->
      True
    _ ->
      False

findCase :: D.Discriminant -> Case a -> Maybe ([(Ident, a)], DecisionTree a)
findCase consDisc decisionCase =
  if consDisc == disc decisionCase
    then return (map (\(_, x, t) -> (x, t)) (consArgs decisionCase), cont decisionCase)
    else Nothing
