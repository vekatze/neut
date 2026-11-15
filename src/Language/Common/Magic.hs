module Language.Common.Magic
  ( Magic (..),
    WeakMagic (..),
  )
where

import Binary.Derive (deriveBinary)
import Language.Common.LowMagic qualified as LM
import Language.Common.ModuleID qualified as MID

data Magic lt ty a
  = LowMagic (LM.LowMagic lt ty a)
  | Calloc ty a a -- Calloc sizeType num size
  | Malloc ty a -- Malloc sizeType size
  | MallocOpaque ty a -- MallocOpaque sizeType size
  | Realloc ty a a -- Realloc sizeType ptr size
  | Free ty a -- Free unitType ptr
  | InspectType MID.ModuleID ty ty -- typeValueExpr, e (both types)
  | EqType MID.ModuleID ty ty
  | ShowType ty -- typeExpr
  | TextCons a a -- rune, text
  | TextUncons MID.ModuleID a -- text
  | MakeSwitch MID.ModuleID a a a -- key, fallback, clauses
  | FreeVariables MID.ModuleID a -- code
  | CompileError a -- msg
  | GetOriginFileName
  | GetOriginLine
  | GetOriginColumn
  deriving (Show, Eq, Functor, Foldable, Traversable)

newtype WeakMagic lt ty a = WeakMagic (Magic lt ty a)

deriveBinary [''Magic]
