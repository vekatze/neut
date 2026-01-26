module Language.Common.DecisionTree
  ( DecisionTree (..),
    CaseList,
    ConsCaseRecord (..),
    Case (..),
    getConstructors,
    isUnreachable,
    findCase,
  )
where

import Data.Binary
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Language.Common.Binder
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.Ident
import Language.Common.IsConstLike
import Language.Common.Literal qualified as L
import Logger.Hint

-- t: type parameter, a: term parameter
data DecisionTree t a
  = Leaf [Ident] [(BinderF t, a)] a
  | Unreachable
  | Switch (Ident, t) (CaseList t a)
  deriving (Generic)

type CaseList t a = (DecisionTree t a, [Case t a])

data ConsCaseRecord t a = ConsCaseRecord
  { mCons :: Hint,
    consDD :: DD.DefiniteDescription,
    isConstLike :: IsConstLike,
    disc :: D.Discriminant,
    dataArgs :: [(t, t)],
    consArgs :: [BinderF t],
    cont :: DecisionTree t a
  }
  deriving (Generic)

data Case t a
  = ConsCase (ConsCaseRecord t a)
  | LiteralCase Hint L.Literal (DecisionTree t a)
  deriving (Generic)

instance (Binary t, Binary a) => Binary (ConsCaseRecord t a)

instance (Binary t, Binary a) => Binary (DecisionTree t a)

instance (Binary t, Binary a) => Binary (Case t a)

getConstructors :: [Case t a] -> [(DD.DefiniteDescription, IsConstLike)]
getConstructors clauseList = do
  catMaybes $ flip map clauseList $ \c -> do
    case c of
      ConsCase (ConsCaseRecord {..}) ->
        Just (consDD, isConstLike)
      LiteralCase {} ->
        Nothing

isUnreachable :: DecisionTree t a -> Bool
isUnreachable tree =
  case tree of
    Unreachable ->
      True
    _ ->
      False

findCase :: D.Discriminant -> Case t a -> Maybe ([(Ident, t)], DecisionTree t a)
findCase consDisc decisionCase =
  case decisionCase of
    LiteralCase {} ->
      Nothing
    ConsCase (ConsCaseRecord {..}) -> do
      if consDisc == disc
        then return (map (\(_, _, x, t) -> (x, t)) consArgs, cont)
        else Nothing
