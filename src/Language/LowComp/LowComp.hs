module Language.LowComp.LowComp
  ( Value (..),
    Comp (..),
    Op (..),
    AllocID,
    FreeID,
    StackSlotID,
    StackAllocInfo (..),
    LowCode (..),
    LowCodeInfo,
    Def,
    DefContent (..),
    Label,
    nop,
    getPhiList,
    getReturnValue,
  )
where

import Data.ByteString.Builder
import Data.Text qualified as T
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.ExternalName qualified as EN
import Language.Common.Ident
import Language.Common.Ident.Reify
import Language.Common.LowType
import Language.Common.PrimNumSize
import Language.Common.PrimOp
import Language.LowComp.DeclarationName qualified as DN

data Value
  = VarLocal Ident
  | VarGlobal DD.DefiniteDescription
  | VarExternal EN.ExternalName
  | VarTextName T.Text
  | Int Integer
  | Float FloatSize Double
  | Address Integer
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
      Address a ->
        "inttoptr (i64 " <> show a <> " to ptr)"
      Null ->
        "null"

type Label =
  Ident

data Comp
  = Return Value -- UpIntro
  | ReturnVoid -- UpIntroVoid
  | Let Ident Op Comp -- UpElim
  -- `CompCont` is `CompLet` that discards the result of Op. This `CompCont` is required separately
  -- since LLVM doesn't allow us to write something like `%foo = store i32 3, i32* %ptr`.
  | Cont Op Comp
  | Switch Value LowType Comp [(Integer, Comp)] [Ident] Comp
  | TailCall LowType Value [(LowType, Value)] -- tail call
  | Unreachable -- for empty case analysis
  | Phi [Value]
  deriving (Show)

type AllocID =
  Int

type FreeID =
  Int

type StackSlotID =
  Int

data StackAllocInfo = StackAllocInfo
  { stackSlotID :: StackSlotID,
    stackElemType :: LowType,
    stackIndexType :: LowType,
    stackSize :: Either Integer Value
  }
  deriving (Show)

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
  | IntToPointer Value LowType
  | PointerToInt Value LowType
  | Load Value LowType
  | Store LowType Value Value
  | StackAlloc StackAllocInfo
  | StackLifetimeStart StackSlotID
  | StackLifetimeEnd StackSlotID
  | Alloc (Either Integer Value) AllocID
  | Free Value (Maybe Int) FreeID
  | PrimOp PrimOp [Value]
  deriving (Show)

type Def =
  (DD.DefiniteDescription, DefContent)

data DefContent = DefContent
  { codType :: LowType,
    args :: [Ident],
    body :: Comp
  }

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
    Return {} ->
      Nothing
    ReturnVoid ->
      Nothing
    Let _ _ cont ->
      getPhiList cont
    Cont _ cont ->
      getPhiList cont
    Switch _ _ _ _ _ cont ->
      getPhiList cont
    TailCall {} ->
      Nothing
    Unreachable ->
      Nothing

getReturnValue :: Comp -> Maybe Value
getReturnValue comp =
  case comp of
    Return v ->
      return v
    ReturnVoid ->
      Nothing
    Let _ _ cont ->
      getReturnValue cont
    Cont _ cont ->
      getReturnValue cont
    Switch _ _ _ _ _ cont ->
      getReturnValue cont
    TailCall {} ->
      Nothing
    Unreachable ->
      Nothing
    Phi {} ->
      Nothing
