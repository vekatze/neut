module Kernel.Elaborate.TypeHoleSubst
  ( TypeHoleSubst (..),
    insert,
    lookup,
    empty,
    singleton,
    fillableType,
    toText,
  )
where

import Data.IntMap qualified as IntMap
import Data.Text qualified as T
import Language.Common.HoleID qualified as HID
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.WeakTerm.Holes
import Language.WeakTerm.ToText qualified as ToText
import Language.WeakTerm.WeakTerm
import Prelude hiding (lookup)

newtype TypeHoleSubst = TypeHoleSubst (IntMap.IntMap ([Ident], WeakType))
  deriving (Semigroup, Monoid)

insert :: HID.HoleID -> [Ident] -> WeakType -> TypeHoleSubst -> TypeHoleSubst
insert (HID.HoleID i) xs e (TypeHoleSubst sub) =
  TypeHoleSubst $ IntMap.insert i (xs, e) sub

lookup :: HID.HoleID -> TypeHoleSubst -> Maybe ([Ident], WeakType)
lookup (HID.HoleID i) (TypeHoleSubst sub) =
  IntMap.lookup i sub

empty :: TypeHoleSubst
empty =
  TypeHoleSubst IntMap.empty

singleton :: HID.HoleID -> [Ident] -> WeakType -> TypeHoleSubst
singleton (HID.HoleID h) xs e =
  TypeHoleSubst $ IntMap.singleton h (xs, e)

fillableType :: WeakType -> TypeHoleSubst -> Bool
fillableType e (TypeHoleSubst sub) = do
  let fmvs = holesType e
  any (\(HID.HoleID fmv) -> IntMap.member fmv sub) fmvs

toText :: TypeHoleSubst -> T.Text
toText (TypeHoleSubst sub) =
  "TypeHoleSubst {\n"
    <> T.intercalate "\n" (map showEntry (IntMap.toList sub))
    <> "\n}"
  where
    showEntry (holeId, (idents, weakType)) =
      "  ?"
        <> T.pack (show holeId)
        <> " => ("
        <> showIdents idents
        <> ", "
        <> ToText.toTextType weakType
        <> ")"
    -- showIdents [] = ""
    showIdents xs = "[" <> T.intercalate ", " (map Ident.toText xs) <> "]"
