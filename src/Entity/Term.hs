module Entity.Term where

import Control.Comonad.Cofree
import Data.Binary
import qualified Data.IntMap as IntMap
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
  | TermMatch (a, a) [(PatternF a, a)]
  deriving (Show, Generic)

instance (Binary a) => Binary (TermF a)

instance Binary Term

type TypeEnv =
  IntMap.IntMap Term
