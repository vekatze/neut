module Entity.HoleSubst where

import qualified Data.IntMap as IntMap
import qualified Entity.HoleID as HID
import Entity.WeakTerm

newtype HoleSubst = HoleSubst (IntMap.IntMap WeakTerm)

insert :: HID.HoleID -> WeakTerm -> HoleSubst -> HoleSubst
insert (HID.HoleID i) e (HoleSubst sub) =
  HoleSubst $ IntMap.insert i e sub

lookup :: HID.HoleID -> HoleSubst -> Maybe WeakTerm
lookup (HID.HoleID i) (HoleSubst sub) =
  IntMap.lookup i sub

empty :: HoleSubst
empty =
  HoleSubst IntMap.empty

singleton :: HID.HoleID -> WeakTerm -> HoleSubst
singleton (HID.HoleID h) e =
  HoleSubst $ IntMap.singleton h e
