module Entity.WeakTerm where

import Control.Comonad.Cofree
import Data.Binary
import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import Entity.Binder
import Entity.Discriminant
import Entity.EnumCase
import Entity.EnumTypeName
import Entity.Hint
import Entity.Ident
import Entity.LamKind
import Entity.Magic
import Entity.Pattern
import Entity.PrimNumSize
import Entity.PrimNumSize.ToText
import GHC.Generics

type WeakTerm = Cofree WeakTermF Hint

data WeakTermF a
  = WeakTermTau
  | WeakTermVar Ident
  | WeakTermVarGlobal T.Text
  | WeakTermPi [BinderF a] a
  | WeakTermPiIntro (LamKindF a) [BinderF a] a
  | WeakTermPiElim a [a]
  | WeakTermSigma [BinderF a]
  | WeakTermSigmaIntro [a]
  | WeakTermSigmaElim [BinderF a] a a
  | WeakTermLet (BinderF a) a a -- let x = e1 in e2 (with no context extension)
  | WeakTermAster Int
  | WeakTermConst T.Text
  | WeakTermInt a Integer
  | WeakTermFloat a Double
  | WeakTermEnum EnumTypeName
  | WeakTermEnumIntro (EnumTypeName, Discriminant) T.Text -- EnumIntro (enum-type, actual-int-value) ebnum-label
  | WeakTermEnumElim (a, a) [(EnumCase, a)]
  | WeakTermQuestion a a -- e : t (output the type `t` as note)
  | WeakTermMagic (Magic a) -- (magic kind arg-1 ... arg-n)
  | WeakTermMatch
      (Maybe a) -- noetic subject (this is for `case-noetic`)
      (a, a) -- (pattern-matched value, its type)
      [(PatternF a, a)]
  | WeakTermNoema a a
  | WeakTermNoemaIntro Ident a
  | WeakTermNoemaElim Ident a
  | WeakTermArray a
  | WeakTermArrayIntro a [a]
  | WeakTermArrayAccess a a a a
  | WeakTermText
  | WeakTermTextIntro T.Text
  | WeakTermCell a -- cell(list(i64))
  | WeakTermCellIntro a a -- cell-new(v) (the first argument is the type of `v`)
  | WeakTermCellRead a -- cell-read(ptr)
  | WeakTermCellWrite a a -- cell-write(ptr, value)
  deriving (Generic)

instance (Binary a) => Binary (WeakTermF a)

instance Binary WeakTerm

type DefInfo =
  ((Hint, T.Text), [BinderF WeakTerm], WeakTerm, WeakTerm)

type TopDefInfo =
  ((Hint, T.Text), [BinderF WeakTerm], [BinderF WeakTerm], WeakTerm, WeakTerm)

type SubstWeakTerm =
  IntMap.IntMap WeakTerm

toVar :: Hint -> Ident -> WeakTerm
toVar m x =
  m :< WeakTermVar x

i8 :: Hint -> WeakTerm
i8 m =
  m :< WeakTermConst (intSizeToText $ IntSize 8)

i64 :: Hint -> WeakTerm
i64 m =
  m :< WeakTermConst (intSizeToText $ IntSize 64)

metaOf :: WeakTerm -> Hint
metaOf (m :< _) =
  m

asVar :: WeakTerm -> Maybe Ident
asVar term =
  case term of
    (_ :< WeakTermVar x) ->
      Just x
    _ ->
      Nothing
