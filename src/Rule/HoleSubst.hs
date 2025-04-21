module Rule.HoleSubst
  ( HoleSubst (..),
    insert,
    lookup,
    empty,
    singleton,
    fillable,
  )
where

import Data.IntMap qualified as IntMap
import Rule.HoleID qualified as HID
import Rule.Ident
import Rule.WeakTerm
import Rule.WeakTerm.Holes
import Prelude hiding (lookup)

newtype HoleSubst = HoleSubst (IntMap.IntMap ([Ident], WeakTerm))
  deriving (Semigroup, Monoid)

insert :: HID.HoleID -> [Ident] -> WeakTerm -> HoleSubst -> HoleSubst
insert (HID.HoleID i) xs e (HoleSubst sub) =
  HoleSubst $ IntMap.insert i (xs, e) sub

lookup :: HID.HoleID -> HoleSubst -> Maybe ([Ident], WeakTerm)
lookup (HID.HoleID i) (HoleSubst sub) =
  IntMap.lookup i sub

empty :: HoleSubst
empty =
  HoleSubst IntMap.empty

singleton :: HID.HoleID -> [Ident] -> WeakTerm -> HoleSubst
singleton (HID.HoleID h) xs e =
  HoleSubst $ IntMap.singleton h (xs, e)

fillable :: WeakTerm -> HoleSubst -> Bool
fillable e (HoleSubst sub) = do
  let fmvs = holes e
  any (\(HID.HoleID fmv) -> IntMap.member fmv sub) fmvs
