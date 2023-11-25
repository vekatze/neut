module Entity.Constraint where

import Data.Set qualified as S
import Entity.HoleID qualified as HID
import Entity.WeakTerm

data Constraint
  = Eq WeakTerm WeakTerm -- (expected-type, actual-type)
  | Actual WeakTerm

type MetaVarSet =
  S.Set HID.HoleID

newtype SuspendedConstraint
  = SuspendedConstraint (MetaVarSet, (Constraint, Constraint))
