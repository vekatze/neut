module Main.Rule.Comp
  ( Value (..),
    Comp (..),
    Primitive (..),
    CompStmt (..),
    SubstValue,
    IsReducible,
    fromDefTuple,
  )
where

import Data.IntMap qualified as IntMap
import Data.List (intercalate)
import Data.Text qualified as T
import Main.Rule.ArgNum
import Main.Rule.BaseLowType
import Main.Rule.DefiniteDescription qualified as DD
import Main.Rule.EnumCase
import Main.Rule.Foreign qualified as F
import Main.Rule.Ident
import Main.Rule.Ident.Reify
import Main.Rule.Magic
import Main.Rule.Opacity
import Main.Rule.PrimNumSize
import Main.Rule.PrimOp

data Value
  = VarLocal Ident
  | VarGlobal DD.DefiniteDescription ArgNum
  | VarStaticText T.Text
  | SigmaIntro [Value]
  | Int IntSize Integer
  | Float FloatSize Double

instance Show Value where
  show v =
    case v of
      VarLocal x ->
        T.unpack $ toText' x
      VarGlobal dd _ ->
        T.unpack $ DD.reify dd
      VarStaticText dd ->
        T.unpack $ "\"" <> dd <> "\""
      SigmaIntro vs ->
        "(" ++ intercalate ", " (map show vs) ++ ")"
      Main.Rule.Comp.Int _ i ->
        show i
      Main.Rule.Comp.Float _ f ->
        show f

type IsReducible = Bool

data Comp
  = PiElimDownElim Value [Value] -- ((force v) v1 ... vn)
  | SigmaElim ShouldDeallocate [Ident] Value Comp
  | UpIntro Value
  | UpElim IsReducible Ident Comp Comp
  | EnumElim [(Int, Value)] Value Comp [(EnumCase, Comp)]
  | Primitive Primitive
  | Free Value Int Comp
  | Unreachable

instance Show Comp where
  show c =
    case c of
      PiElimDownElim v vs ->
        show v ++ "@(" ++ intercalate "," (map show vs) ++ ")"
      SigmaElim b xs v cont -> do
        let h = if b then "let" else "let-noetic"
        h ++ " (" ++ intercalate "," (map show xs) ++ ") = " ++ show v ++ "\n" ++ show cont
      UpIntro v ->
        "return " ++ show v
      UpElim isReducible x c1 c2 -> do
        let modifier = if isReducible then "" else "*"
        "let" ++ modifier ++ " " ++ show x ++ " = " ++ show c1 ++ "\n" ++ show c2
      EnumElim sub v c1 caseList -> do
        "switch " ++ show sub ++ " " ++ show v ++ "\n<default>\n" ++ show c1 ++ unwords (map showEnumCase caseList)
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
  | Magic (Magic BaseLowType Value)
  deriving (Show)

type SubstValue =
  IntMap.IntMap Value

data CompStmt
  = Def DD.DefiniteDescription Opacity [Ident] Comp
  | Foreign [F.Foreign]

fromDefTuple :: (DD.DefiniteDescription, (Opacity, [Ident], Comp)) -> CompStmt
fromDefTuple (dd, (opacity, args, body)) =
  Def dd opacity args body
