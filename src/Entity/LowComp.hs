module Entity.LowComp where

import qualified Entity.DefiniteDescription as DD
import qualified Entity.ExternalName as EN
import Entity.Ident
import Entity.LowType
import Entity.PrimNumSize
import Entity.PrimOp

data LowValue
  = LowValueVarLocal Ident
  | LowValueVarGlobal DD.DefiniteDescription
  | LowValueVarExternal EN.ExternalName
  | LowValueInt Integer
  | LowValueFloat FloatSize Double
  | LowValueNull
  deriving (Show)

data LowComp
  = LowCompReturn LowValue -- UpIntro
  | LowCompLet Ident LowOp LowComp -- UpElim
  -- `LowCompCont` is `LowCompLet` that discards the result of LowOp. This `LowCompCont` is required separately
  -- since LLVM doesn't allow us to write something like `%foo = store i32 3, i32* %ptr`.
  | LowCompCont LowOp LowComp
  | LowCompSwitch (LowValue, LowType) LowComp [(Integer, LowComp)] -- EnumElim
  | LowCompCall LowValue [LowValue] -- tail call
  | LowCompUnreachable -- for empty case analysis
  deriving (Show)

data LowOp
  = LowOpCall LowValue [LowValue] -- non-tail call
  | LowOpGetElementPtr
      (LowValue, LowType) -- (base pointer, the type of base pointer)
      [(LowValue, LowType)] -- [(index, the-type-of-index)]
  | LowOpBitcast
      LowValue
      LowType -- cast from
      LowType -- cast to
  | LowOpIntToPointer LowValue LowType LowType
  | LowOpPointerToInt LowValue LowType LowType
  | LowOpLoad LowValue LowType
  | LowOpStore LowType LowValue LowValue
  | LowOpAlloc LowValue SizeInfo
  | LowOpFree LowValue SizeInfo Int -- (var, size-of-var, name-of-free)   (name-of-free is only for optimization)
  | LowOpPrimOp PrimOp [LowValue]
  | LowOpSyscall
      Integer -- syscall number
      [LowValue] -- arguments
  deriving (Show)

type SizeInfo =
  LowType

type LowDef =
  (DD.DefiniteDescription, ([Ident], LowComp))
