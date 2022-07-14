module Entity.WeakTerm where

import Control.Comonad.Cofree
import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import Entity.Binder
import qualified Entity.DefiniteDescription as DD
import Entity.EnumCase
import Entity.EnumTypeName
import Entity.Hint
import Entity.Ident
import Entity.LamKind
import Entity.Magic
import Entity.Pattern
import qualified Entity.Prim as Prim
import Entity.PrimNum
import Entity.PrimNumSize

type WeakTerm = Cofree WeakTermF Hint

data WeakTermF a
  = WeakTermTau
  | WeakTermVar Ident
  | WeakTermVarGlobal DD.DefiniteDescription
  | WeakTermPi [BinderF a] a
  | WeakTermPiIntro (LamKindF a) [BinderF a] a
  | WeakTermPiElim a [a]
  | WeakTermSigma [BinderF a]
  | WeakTermSigmaIntro [a]
  | WeakTermSigmaElim [BinderF a] a a
  | WeakTermLet (BinderF a) a a -- let x = e1 in e2 (with no context extension)
  | WeakTermAster Int
  | WeakTermPrim Prim.Prim
  | WeakTermInt a Integer
  | WeakTermFloat a Double
  | WeakTermEnum EnumTypeName
  | WeakTermEnumIntro EnumLabel
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
  | WeakTermResourceType DD.DefiniteDescription

type SubstWeakTerm =
  IntMap.IntMap WeakTerm

toVar :: Hint -> Ident -> WeakTerm
toVar m x =
  m :< WeakTermVar x

i8 :: Hint -> WeakTerm
i8 m =
  m :< WeakTermPrim (Prim.Type $ PrimNumInt $ IntSize 8)

i64 :: Hint -> WeakTerm
i64 m =
  m :< WeakTermPrim (Prim.Type $ PrimNumInt $ IntSize 64)

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
