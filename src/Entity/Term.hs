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
  = Tau
  | Var Ident
  | VarGlobal DD.DefiniteDescription Arity
  | Pi [BinderF a] a
  | PiIntro (LamKindF a) [BinderF a] a
  | PiElim a [a]
  | Sigma [BinderF a]
  | SigmaIntro [a]
  | SigmaElim [BinderF a] a a
  | Let (BinderF a) a a -- let x = e1 in e2 (with no context extension)
  | Prim Prim.Prim
  | Int IntSize Integer
  | Float FloatSize Double
  | Enum EnumTypeName
  | EnumIntro EnumLabel
  | EnumElim (a, a) [(EnumCase, a)]
  | Magic (Magic a)
  | Match (a, a) [(PatternF a, a)]
  deriving (Show, Generic)

instance (Binary a) => Binary (TermF a)

instance Binary Term

type TypeEnv =
  IntMap.IntMap Term
