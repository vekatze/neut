module Entity.Constraint
  ( Constraint (..),
    MetaVarSet,
    SuspendedConstraint (..),
  )
where

import Data.Set qualified as S
import Entity.HoleID qualified as HID
import Entity.WeakTerm qualified as WT

data Constraint
  = Eq WT.WeakTerm WT.WeakTerm -- (expected-type, actual-type)
  | Actual WT.WeakTerm
  | Integer WT.WeakTerm

type MetaVarSet =
  S.Set HID.HoleID

newtype SuspendedConstraint
  = SuspendedConstraint (MetaVarSet, (Constraint, Constraint))
