module Entity.Term where

import Control.Comonad.Cofree
import Data.Binary
import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import Entity.Arity
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
import GHC.Generics (Generic)

type Term = Cofree TermF Hint

data TermF a
  = TermTau
  | TermVar Ident
  | TermVarGlobal DD.DefiniteDescription Arity
  | TermPi [BinderF a] a
  | TermPiIntro (LamKindF a) [BinderF a] a
  | TermPiElim a [a]
  | TermSigma [BinderF a]
  | TermSigmaIntro [a]
  | TermSigmaElim [BinderF a] a a
  | TermLet (BinderF a) a a -- let x = e1 in e2 (with no context extension)
  | TermPrim Prim.Prim
  | TermInt IntSize Integer
  | TermFloat FloatSize Double
  | TermEnum EnumTypeName
  | TermEnumIntro EnumLabel
  | TermEnumElim (a, a) [(EnumCase, a)]
  | TermMagic (Magic a)
  | TermMatch
      (Maybe a) -- noetic subject (this is for `case-noetic`)
      (a, a) -- (pattern-matched value, its type)
      [(PatternF a, a)]
  | TermNoema a a
  | TermNoemaIntro Ident a
  | TermNoemaElim Ident a
  | TermArray PrimNum
  | TermArrayIntro PrimNum [a]
  | TermArrayAccess a PrimNum a a
  | TermText
  | TermTextIntro T.Text
  | TermCell a -- cell(list(i64))
  | TermCellIntro a a -- cell-new(v) (the first argument is the type of `v`)
  | TermCellRead a -- cell-read(ptr)
  | TermCellWrite a a -- cell-write(ptr, value)
  | TermResourceType DD.DefiniteDescription
  deriving (Show, Generic)

instance (Binary a) => Binary (TermF a)

instance Binary Term

type TypeEnv =
  IntMap.IntMap Term

-- asVar :: Term -> Maybe Ident
-- asVar term =
--   case term of
--     _ :< TermVar x ->
--       Just x
--     _ ->
--       Nothing
