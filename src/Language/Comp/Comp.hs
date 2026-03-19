module Language.Comp.Comp
  ( Value (..),
    ForceInline,
    Comp (..),
    Primitive (..),
    CompStmt (..),
    SubstValue,
    IsReducible,
    DefMap,
    Label,
    fromDefTuple,
    fromCompStmt,
    intValue0,
    intValue1,
    mulInt64,
    isUnreachable,
    null,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IntMap qualified as IntMap
import Data.List (intercalate)
import Data.Text qualified as T
import Language.Common.ArgNum
import Language.Common.BaseLowType
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Foreign qualified as F
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.Ident
import Language.Common.Ident.Reify
import Language.Common.LowMagic
import Language.Common.Opacity
import Language.Common.PrimNumSize
import Language.Common.PrimOp
import Language.Common.PrimOp.BinaryOp qualified as BOp
import Language.Common.PrimType qualified as PT
import Language.Comp.EnumCase hiding (Int)
import Prelude hiding (null)

data Value
  = VarLocal Ident
  | VarGlobal DD.DefiniteDescription ArgNum (FCT.ForeignCodType BaseLowType)
  | VarStaticText T.Text
  | SigmaIntro [Value]
  | SigmaDataIntro Int [Value]
  | Int IntSize Integer
  | Float FloatSize Double
  deriving (Eq)

instance Show Value where
  show v =
    case v of
      VarLocal x ->
        T.unpack $ toText' x
      VarGlobal dd _ _ ->
        T.unpack $ DD.reify dd
      VarStaticText dd ->
        T.unpack $ "\"" <> dd <> "\""
      SigmaIntro vs ->
        "(" ++ intercalate ", " (map show vs) ++ ")"
      SigmaDataIntro size vs ->
        "data[" ++ show size ++ "](" ++ intercalate ", " (map show vs) ++ ")"
      Language.Comp.Comp.Int _ i ->
        show i
      Language.Comp.Comp.Float _ f ->
        show f

type IsReducible = Bool

type ForceInline = Bool

type Label =
  Ident

data Comp
  = PiElimDownElim ForceInline Value [Value] -- ((force v) v1 ... vn)
  | SigmaElim ShouldDeallocate [Ident] Value Comp
  | UpIntro Value
  | UpIntroVoid
  | UpElim IsReducible Ident Comp Comp
  | UpElimCallVoid Value [Value] Comp
  | EnumElim [(Int, Value)] Value Comp [(EnumCase, Comp)]
  | DestCall Comp Value [Value]
  | WriteToDest Value Comp Comp Comp
  | Primitive Primitive
  | Free Value Int Comp
  | Unreachable

instance Show Comp where
  show c =
    case c of
      PiElimDownElim _ v vs ->
        show v ++ "@(" ++ intercalate "," (map show vs) ++ ")"
      SigmaElim b xs v cont -> do
        let h = if b then "let" else "let-noetic"
        h ++ " (" ++ intercalate "," (map show xs) ++ ") = " ++ show v ++ "\n" ++ show cont
      UpIntro v ->
        "return " ++ show v
      UpIntroVoid ->
        "return-void"
      UpElim isReducible x c1 c2 -> do
        let modifier = if isReducible then "" else "*"
        "let" ++ modifier ++ " " ++ show x ++ " = " ++ show c1 ++ "\n" ++ show c2
      UpElimCallVoid f vs cont ->
        "call-void " ++ show f ++ "@(" ++ intercalate "," (map show vs) ++ ")\n" ++ show cont
      EnumElim sub v c1 caseList -> do
        "switch "
          ++ show sub
          ++ " "
          ++ show v
          ++ "\n<default>\n"
          ++ show c1
          ++ unwords (map showEnumCase caseList)
      DestCall sizeComp f vs ->
        "dest-call\n<size-comp>\n" ++ show sizeComp ++ "\n" ++ show f ++ "@(" ++ intercalate "," (map show vs) ++ ")"
      WriteToDest dest sizeComp result cont ->
        "write-to-dest(" ++ show dest ++ ")\n<size-comp>\n" ++ show sizeComp ++ "\n<result>\n" ++ show result ++ "\n<cont>\n" ++ show cont
      Primitive prim ->
        "(" ++ show prim ++ ")"
      Free x size cont ->
        "free(" ++ show x ++ ", " ++ show size ++ ")\n" ++ show cont
      Unreachable ->
        "⊥"

showEnumCase :: (EnumCase, Comp) -> String
showEnumCase (ec, c) = do
  "\n<" ++ show ec ++ ">\n" ++ show c

type ShouldDeallocate = Bool

data Primitive
  = PrimOp PrimOp [Value]
  | ShiftPointer Value Integer Integer -- (ptr, num-of-elems, index)
  | Alloc Value -- number of words to allocate
  | Memcpy Value Value Value -- (dest, src, size-in-words)
  | Magic (LowMagic BaseLowType Value Value)
  deriving (Show)

type SubstValue =
  IntMap.IntMap Value

data CompStmt
  = Def DD.DefiniteDescription Opacity [Ident] Comp
  | DefVoid DD.DefiniteDescription Opacity [Ident] Comp
  | Foreign [F.Foreign]

fromCompStmt :: CompStmt -> Maybe (Opacity, [Ident], Comp)
fromCompStmt cs =
  case cs of
    Def _ opacity xs body ->
      Just (opacity, xs, body)
    DefVoid _ opacity xs body ->
      Just (opacity, xs, body)
    Foreign {} ->
      Nothing

fromDefTuple :: (DD.DefiniteDescription, (Opacity, [Ident], Comp)) -> CompStmt
fromDefTuple (dd, (opacity, args, body)) =
  Def dd opacity args body

type DefMap =
  Map.HashMap DD.DefiniteDescription (Opacity, [Ident], Comp)

intValue0 :: Value
intValue0 =
  Int IntSize1 0

intValue1 :: Value
intValue1 =
  Int IntSize1 1

mulInt64 :: Value -> Value -> Primitive
mulInt64 x y =
  PrimOp (PrimBinaryOp BOp.Mul (PT.Int IntSize64) (PT.Int IntSize64)) [x, y]

isUnreachable :: Comp -> Bool
isUnreachable comp =
  case comp of
    PiElimDownElim {} ->
      False
    SigmaElim _ _ _ cont ->
      isUnreachable cont
    UpIntro {} ->
      False
    UpIntroVoid ->
      False
    UpElim _ _ e1 e2 ->
      isUnreachable e1 || isUnreachable e2
    UpElimCallVoid _ _ e ->
      isUnreachable e
    EnumElim _ _ defaultBranch branchList ->
      isUnreachable defaultBranch && all (isUnreachable . snd) branchList
    DestCall {} ->
      False
    WriteToDest _ _ result cont ->
      isUnreachable result || isUnreachable cont
    Primitive {} ->
      False
    Free _ _ cont ->
      isUnreachable cont
    Unreachable ->
      True

null :: Value
null =
  SigmaIntro []
