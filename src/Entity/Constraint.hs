module Entity.Constraint where

import Control.Comonad.Cofree
import Data.Set qualified as S
import Entity.Hint
import Entity.HoleID qualified as HID
import Entity.WeakTerm qualified as WT

data Constraint
  = Eq WT.WeakTerm WT.WeakTerm -- (expected-type, actual-type)
  | Actual WT.WeakTerm
  | Affine WT.WeakTerm
  | Integer WT.WeakTerm

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
    Affine (m :< _) ->
      m
    Integer (m :< _) ->
      m
