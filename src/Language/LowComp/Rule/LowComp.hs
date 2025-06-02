module Language.LowComp.Rule.LowComp
  ( Value (..),
    Comp (..),
    Op (..),
    AllocID,
    FreeID,
    LowCode (..),
    LowCodeInfo,
    Def,
    DefContent,
    Label,
    nop,
    getPhiList,
  )
where

import Data.ByteString.Builder
import Data.Text qualified as T
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.ExternalName qualified as EN
import Language.Common.Rule.Ident
import Language.Common.Rule.Ident.Reify
import Language.Common.Rule.LowType
import Language.Common.Rule.PrimNumSize
import Language.Common.Rule.PrimOp
import Language.LowComp.Rule.DeclarationName qualified as DN

data Value
  = VarLocal Ident
  | VarGlobal DD.DefiniteDescription
  | VarExternal EN.ExternalName
  | VarTextName T.Text
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
      VarTextName name ->
        T.unpack name
      Int i ->
        show i
      Float _ f ->
        show f
      Null ->
        "null"

type Label =
  Ident

data Comp
  = Return Value -- UpIntro
  | Let Ident Op Comp -- UpElim
  -- `CompCont` is `CompLet` that discards the result of Op. This `CompCont` is required separately
  -- since LLVM doesn't allow us to write something like `%foo = store i32 3, i32* %ptr`.
  | Cont Op Comp
  | Switch (Value, LowType) Comp [(Integer, Comp)] ([Ident], Comp)
  | TailCall LowType Value [(LowType, Value)] -- tail call
  | Unreachable -- for empty case analysis
  | Phi [Value]
  deriving (Show)

type AllocID =
  Int

type FreeID =
  Int

data Op
  = Call LowType Value [(LowType, Value)] -- non-tail call
  | MagicCall LowType Value [(LowType, Value)] -- non-tail call (external)
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
  | StackAlloc LowType LowType Value
  | Alloc Value Int AllocID
  | Free Value Int FreeID
  | PrimOp PrimOp [Value]
  deriving (Show)

type Def =
  (DD.DefiniteDescription, DefContent)

type DefContent =
  ([Ident], Comp)

type LowCodeInfo =
  (DN.DeclEnv, [Def], [StaticTextInfo])

data LowCode
  = LowCodeMain DefContent LowCodeInfo
  | LowCodeNormal LowCodeInfo

type StaticTextInfo = (T.Text, (Builder, Int))

nop :: Value -> Op
nop v =
  Bitcast v Pointer Pointer

getPhiList :: Comp -> Maybe [Value]
getPhiList comp =
  case comp of
    Phi vs ->
      return vs
    Return _ ->
      Nothing
    Let _ _ cont ->
      getPhiList cont
    Cont _ cont ->
      getPhiList cont
    Switch _ _ _ (_, cont) ->
      getPhiList cont
    TailCall {} ->
      Nothing
    Unreachable ->
      Nothing
