module Entity.Constraint where

import Data.PQueue.Min qualified as Q
import Data.Set qualified as S
import Entity.HoleID qualified as HID
import Entity.WeakTerm

data Constraint
  = Eq WeakTerm WeakTerm -- (expected-type, actual-type)
  | Immutable WeakTerm -- doesn't contain cells

data ConstraintKind
  = Delta Constraint
  | Other

type MetaVarSet =
  S.Set HID.HoleID

newtype SuspendedConstraint
  = SuspendedConstraint (MetaVarSet, ConstraintKind, (Constraint, Constraint))

instance Eq SuspendedConstraint where
  (SuspendedConstraint (_, kind1, _)) == (SuspendedConstraint (_, kind2, _)) =
    kindToInt kind1 == kindToInt kind2

instance Ord SuspendedConstraint where
  (SuspendedConstraint (_, kind1, _)) `compare` (SuspendedConstraint (_, kind2, _)) =
    kindToInt kind1 `compare` kindToInt kind2

type SuspendedConstraintQueue =
  Q.MinQueue SuspendedConstraint

kindToInt :: ConstraintKind -> Int
kindToInt k =
  case k of
    Delta {} ->
      0
    Other {} ->
      1
