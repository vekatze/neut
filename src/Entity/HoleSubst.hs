module Entity.HoleSubst where

import qualified Data.IntMap as IntMap
import qualified Entity.HoleID as HID
import Entity.Ident
import Entity.WeakTerm

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
