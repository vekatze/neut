module Entity.LowComp where

import qualified Entity.DefiniteDescription as DD
import qualified Entity.ExternalName as EN
import Entity.Ident
import Entity.LowType
import Entity.PrimNumSize
import Entity.PrimOp

data Value
  = VarLocal Ident
  | VarGlobal DD.DefiniteDescription
  | VarExternal EN.ExternalName
  | Int Integer
  | Float FloatSize Double
  | Null
  deriving (Show)

data Comp
  = Return Value -- UpIntro
  | Let Ident Op Comp -- UpElim
  -- `CompCont` is `CompLet` that discards the result of Op. This `CompCont` is required separately
  -- since LLVM doesn't allow us to write something like `%foo = store i32 3, i32* %ptr`.
  | Cont Op Comp
  | Switch (Value, LowType) Comp [(Integer, Comp)] -- EnumElim
  | TailCall Value [Value] -- tail call
  | Unreachable -- for empty case analysis
  deriving (Show)

data Op
  = Call Value [Value] -- non-tail call
  | GetElementPtr
      (Value, LowType) -- (base pointer, the type of base pointer)
      [(Value, LowType)] -- [(index, the-type-of-index)]
  | Bitcast
      Value
      LowType -- cast from
      LowType -- cast to
  | IntToPointer Value LowType LowType
  | PointerToInt Value LowType LowType
  | Load Value LowType
  | Store LowType Value Value
  | Alloc Value SizeInfo
  | Free Value SizeInfo Int -- (var, size-of-var, name-of-free)   (name-of-free is only for optimization)
  | PrimOp PrimOp [Value]
  | Syscall
      Integer -- syscall number
      [Value] -- arguments
  deriving (Show)

type SizeInfo =
  LowType

type Def =
  (DD.DefiniteDescription, ([Ident], Comp))
