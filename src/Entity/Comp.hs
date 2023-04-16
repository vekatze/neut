module Entity.Comp where

import Data.IntMap qualified as IntMap
import Data.List (intercalate)
import Data.Text qualified as T
import Entity.Arity
import Entity.DefiniteDescription qualified as DD
import Entity.EnumCase
import Entity.Ident
import Entity.Ident.Reify
import Entity.Magic
import Entity.Opacity
import Entity.PrimNumSize
import Entity.PrimOp

data Value
  = VarLocal Ident
  | VarGlobal DD.DefiniteDescription Arity
  | VarStaticText DD.DefiniteDescription Int
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
      VarStaticText dd _ ->
        T.unpack $ DD.reify dd
      SigmaIntro vs ->
        "(" ++ intercalate ", " (map show vs) ++ ")"
      Entity.Comp.Int _ i ->
        show i
      Entity.Comp.Float _ f ->
        show f

type IsReducible = Bool

data Comp
  = PiElimDownElim Value [Value] -- ((force v) v1 ... vn)
  | SigmaElim ShouldDeallocate [Ident] Value Comp
  | UpIntro Value
  | UpElim IsReducible Ident Comp Comp
  | EnumElim Value Comp [(EnumCase, Comp)]
  | Primitive Primitive
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
      EnumElim v c1 caseList -> do
        "switch " ++ show v ++ "\n<default>\n" ++ show c1 ++ unwords (map showEnumCase caseList)
      Primitive prim ->
        "(" ++ show prim ++ ")"
      Unreachable ->
        "⊥"

showEnumCase :: (EnumCase, Comp) -> String
showEnumCase (ec, c) = do
  "\n<" ++ show ec ++ ">\n" ++ show c

type ShouldDeallocate = Bool

data Primitive
  = PrimOp PrimOp [Value]
  | Magic (Magic Value)
  deriving (Show)

type SubstValue =
  IntMap.IntMap Value

type CompDef =
  (DD.DefiniteDescription, (Opacity, [Ident], Comp))
