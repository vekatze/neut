module Entity.Comp.FreeVars (freeVars) where

import qualified Data.Set as S
import Entity.Basic
import Entity.Comp

freeVars :: Comp -> S.Set Ident
freeVars =
  freeVarsComp

freeVarsComp :: Comp -> S.Set Ident
freeVarsComp c =
  case c of
    CompPiElimDownElim v vs ->
      S.unions $ map freeVarsValue (v : vs)
    CompSigmaElim _ xs v e -> do
      let s1 = freeVarsValue v
      let s2 = S.filter (`notElem` xs) $ freeVarsComp e
      S.union s1 s2
    CompUpIntro v ->
      freeVarsValue v
    CompUpElim x e1 e2 -> do
      let s1 = freeVarsComp e1
      let s2 = S.filter (/= x) $ freeVarsComp e2
      S.union s1 s2
    CompEnumElim v caseList -> do
      let s1 = freeVarsValue v
      let (_, es) = unzip caseList
      let s2 = S.unions (map freeVarsComp es)
      S.union s1 s2
    CompArrayAccess _ array index ->
      S.union (freeVarsValue array) (freeVarsValue index)
    CompPrimitive prim ->
      case prim of
        PrimitivePrimOp _ vs ->
          S.unions $ map freeVarsValue vs
        PrimitiveMagic der ->
          foldMap freeVarsValue der

freeVarsValue :: Value -> S.Set Ident
freeVarsValue v =
  case v of
    ValueVarLocal x ->
      S.singleton x
    ValueVarLocalIdeal x ->
      S.singleton x
    ValueSigmaIntro vs ->
      S.unions $ map freeVarsValue vs
    ValueArrayIntro _ vs ->
      S.unions $ map freeVarsValue vs
    _ ->
      S.empty
