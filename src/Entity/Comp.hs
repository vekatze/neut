module Entity.Comp where

import qualified Data.IntMap as IntMap
import Data.List (intercalate)
import qualified Data.Text as T
import Entity.Arity
import qualified Entity.DefiniteDescription as DD
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
  | SigmaIntro [Value]
  | Int IntSize Integer
  | Float FloatSize Double
  | EnumIntro EnumLabel

instance Show Value where
  show v =
    case v of
      VarLocal x ->
        T.unpack $ toText' x
      VarGlobal dd _ ->
        T.unpack $ DD.reify dd
      SigmaIntro vs ->
        "(" ++ intercalate ", " (map show vs) ++ ")"
      Entity.Comp.Int _ i ->
        show i
      Entity.Comp.Float _ f ->
        show f
      EnumIntro (EnumLabel _ discriminant _) ->
        show discriminant

data Comp
  = PiElimDownElim Value [Value] -- ((force v) v1 ... vn)
  | SigmaElim ShouldDeallocate [Ident] Value Comp
  | UpIntro Value
  | UpElim Ident Comp Comp
  | EnumElim Value Comp [(CompEnumCase, Comp)]
  | Primitive Primitive
  | Unreachable

instance Show Comp where
  show c =
    case c of
      PiElimDownElim v vs ->
        show v ++ "(" ++ intercalate "," (map show vs) ++ ")"
      SigmaElim b xs v cont -> do
        let h = if b then "let" else "let-noetic"
        h ++ " (" ++ intercalate "," (map show xs) ++ ") = " ++ show v ++ "\n" ++ show cont
      UpIntro v ->
        "return " ++ show v
      UpElim x c1 c2 ->
        "let " ++ show x ++ " = " ++ show c1 ++ "\n" ++ show c2
      EnumElim v c1 caseList -> do
        "switch " ++ show v ++ "\n<default>\n" ++ show c1 ++ unwords (map showEnumCase caseList)
      Primitive prim ->
        "(" ++ show prim ++ ")"
      Unreachable ->
        "⊥"

showEnumCase :: (CompEnumCase, Comp) -> String
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
