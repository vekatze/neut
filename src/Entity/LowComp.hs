module Entity.LowComp where

import Data.Text qualified as T
import Entity.DefiniteDescription qualified as DD
import Entity.ExternalName qualified as EN
import Entity.Ident
import Entity.Ident.Reify
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

instance Show Value where
  show v =
    case v of
      VarLocal x ->
        T.unpack $ toText' x
      VarGlobal dd ->
        T.unpack $ DD.reify dd
      VarExternal x ->
        T.unpack $ EN.reify x
      Int i ->
        show i
      Float _ f ->
        show f
      Null ->
        "null"

data Comp
  = Return Value -- UpIntro
  | Let Ident Op Comp -- UpElim
  -- `CompCont` is `CompLet` that discards the result of Op. This `CompCont` is required separately
  -- since LLVM doesn't allow us to write something like `%foo = store i32 3, i32* %ptr`.
  | Cont Op Comp
  | Switch (Value, LowType) Comp [(Integer, Comp)] (Ident, Comp)
  | TailCall Value [Value] -- tail call
  | Unreachable -- for empty case analysis
  deriving (Show)

type AllocID =
  Int

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
  | Alloc Value Int AllocID
  | Free Value Int
  | PrimOp PrimOp [Value]
  | Syscall
      Integer -- syscall number
      [Value] -- arguments
  deriving (Show)

type SizeInfo =
  LowType

type Def =
  (DD.DefiniteDescription, DefContent)

type DefContent =
  ([Ident], Comp)
