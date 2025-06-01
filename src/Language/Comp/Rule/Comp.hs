module Language.Comp.Rule.Comp
  ( Value (..),
    Comp (..),
    Primitive (..),
    CompStmt (..),
    SubstValue,
    IsReducible,
    DefMap,
    Label,
    fromDefTuple,
    intValue0,
    intValue1,
    getPhiList,
    graft,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IntMap qualified as IntMap
import Data.List (intercalate)
import Data.Text qualified as T
import Language.Common.Rule.ArgNum
import Language.Common.Rule.BaseLowType
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.Foreign qualified as F
import Language.Common.Rule.Ident
import Language.Common.Rule.Ident.Reify
import Language.Common.Rule.Ident.Reify qualified as Ident
import Language.Common.Rule.Magic
import Language.Common.Rule.Opacity
import Language.Common.Rule.PrimNumSize
import Language.Common.Rule.PrimOp
import Language.Comp.Rule.EnumCase hiding (Int)

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
      Language.Comp.Rule.Comp.Int _ i ->
        show i
      Language.Comp.Rule.Comp.Float _ f ->
        show f

type IsReducible = Bool

type Label =
  Ident

data Comp
  = PiElimDownElim Value [Value] -- ((force v) v1 ... vn)
  | SigmaElim ShouldDeallocate [Ident] Value Comp
  | UpIntro Value
  | UpElim IsReducible Ident Comp Comp
  | EnumElim [(Int, Value)] Value Comp [(EnumCase, Comp)] [Ident] Label Comp
  | Primitive Primitive
  | Free Value Int Comp
  | Unreachable
  | Phi Label [Value]

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
      EnumElim sub v c1 caseList phiVarList label cont -> do
        "switch " ++ show sub ++ " " ++ show v ++ "\n<default>\n" ++ show c1 ++ unwords (map showEnumCase caseList) ++ "\n" <> show label <> ":\nphi " <> show phiVarList <> " = (parent) in\n" <> show cont
      Primitive prim ->
        "(" ++ show prim ++ ")"
      Free x size cont ->
        "free(" ++ show x ++ ", " ++ show size ++ ")\n" ++ show cont
      Unreachable ->
        "⊥"
      Phi label ds ->
        "phi" <> "<" <> T.unpack (Ident.toText' label) <> ">" <> "(" ++ intercalate "," (map show ds) ++ ")"

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

type DefMap =
  Map.HashMap DD.DefiniteDescription (Opacity, [Ident], Comp)

intValue0 :: Value
intValue0 =
  Int (IntSize 1) 0

intValue1 :: Value
intValue1 =
  Int (IntSize 1) 1

getPhiList :: Comp -> Maybe [Value]
getPhiList e =
  case e of
    Phi _ ds ->
      return ds
    PiElimDownElim {} ->
      Nothing
    SigmaElim _ _ _ cont ->
      getPhiList cont
    UpIntro {} ->
      Nothing
    UpElim _ _ _ cont ->
      getPhiList cont
    EnumElim _ _ _ _ _ _ cont ->
      getPhiList cont
    Primitive {} ->
      Nothing
    Free _ _ cont ->
      getPhiList cont
    Unreachable ->
      Nothing

graft :: Comp -> [Ident] -> Comp -> Maybe Comp
graft e1 phiVarList cont =
  case e1 of
    Phi _ vs -> do
      return $ bind (zip phiVarList vs) cont
    PiElimDownElim {} ->
      Nothing
    SigmaElim flag ys v e2 -> do
      e2' <- graft e2 phiVarList cont
      return $ SigmaElim flag ys v e2'
    UpIntro {} ->
      Nothing
    UpElim flag x e2 e3 -> do
      e3' <- graft e3 phiVarList cont
      return $ UpElim flag x e2 e3'
    EnumElim fvInfo v defaultBranch branchList ys goalLabel e2 -> do
      e2' <- graft e2 phiVarList cont
      return $ EnumElim fvInfo v defaultBranch branchList ys goalLabel e2'
    Primitive {} ->
      Nothing
    Free v size e2 -> do
      e2' <- graft e2 phiVarList cont
      return $ Free v size e2'
    Unreachable ->
      Nothing

bind :: [(Ident, Value)] -> Comp -> Comp
bind xvs e =
  case xvs of
    [] ->
      e
    (x, v) : rest ->
      UpElim True x (UpIntro v) $ bind rest e
