module Entity.Constraint where

import Control.Comonad.Cofree
import Data.Set qualified as S
import Entity.Hint
import Entity.HoleID qualified as HID
import Entity.WeakTerm

data Constraint
  = Eq WeakTerm WeakTerm -- (expected-type, actual-type)
  | Actual WeakTerm

type MetaVarSet =
  S.Set HID.HoleID

newtype SuspendedConstraint
  = SuspendedConstraint (MetaVarSet, (Constraint, Constraint))

getLoc :: Constraint -> Hint
getLoc c = do
  case c of
    Eq _ (m :< _) ->
      m
    Actual (m :< _) ->
      m
