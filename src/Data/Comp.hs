module Data.Comp where

import Data.Basic
import qualified Data.IntMap as IntMap
import Data.LowType
import qualified Data.Set as S
import qualified Data.Text as T

data Value
  = ValueVarLocal Ident
  | ValueVarGlobal T.Text
  | ValueSigmaIntro [Value]
  | ValueInt IntSize Integer
  | ValueFloat FloatSize Double
  | ValueEnumIntro FilePath T.Text
  deriving (Show)

data Comp
  = CompPiElimDownElim Value [Value] -- ((force v) v1 ... vn)
  | CompSigmaElim Bool [Ident] Value Comp
  | CompUpIntro Value
  | CompUpElim Ident Comp Comp
  | CompEnumElim Value [(EnumCase, Comp)]
  | CompPrimitive Primitive
  | CompIgnore Comp
  deriving (Show)

data Primitive
  = PrimitivePrimOp PrimOp [Value]
  | PrimitiveDerangement Derangement [Value]
  deriving (Show)

-- type Value =
--   (Hint, Value)

-- type Comp =
--   (Hint, Comp)

type SubstValue =
  IntMap.IntMap Value

varValue :: Value -> S.Set Ident
varValue v =
  case v of
    ValueVarLocal x ->
      S.singleton x
    ValueSigmaIntro vs ->
      S.unions $ map varValue vs
    _ ->
      S.empty

varComp :: Comp -> S.Set Ident
varComp c =
  case c of
    CompPiElimDownElim v vs ->
      S.unions $ map varValue (v : vs)
    CompSigmaElim _ xs v e -> do
      let s1 = varValue v
      let s2 = S.filter (`notElem` xs) $ varComp e
      S.union s1 s2
    CompUpIntro v ->
      varValue v
    CompUpElim x e1 e2 -> do
      let s1 = varComp e1
      let s2 = S.filter (/= x) $ varComp e2
      S.union s1 s2
    CompEnumElim v caseList -> do
      let s1 = varValue v
      let (_, es) = unzip caseList
      let s2 = S.unions (map varComp es)
      S.union s1 s2
    CompPrimitive prim ->
      case prim of
        PrimitivePrimOp _ vs ->
          S.unions $ map varValue vs
        PrimitiveDerangement _ vs ->
          S.unions $ map varValue vs
    CompIgnore e ->
      varComp e

dummyComp :: Comp
dummyComp =
  CompUpIntro (ValueInt 64 0)
