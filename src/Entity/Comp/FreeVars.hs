module Entity.Comp.FreeVars (freeVars) where

import qualified Data.Set as S
import qualified Entity.Comp as C
import Entity.Ident

freeVars :: C.Comp -> S.Set Ident
freeVars =
  freeVarsComp

freeVarsComp :: C.Comp -> S.Set Ident
freeVarsComp c =
  case c of
    C.PiElimDownElim v vs ->
      S.unions $ map freeVarsValue (v : vs)
    C.SigmaElim _ xs v e -> do
      let s1 = freeVarsValue v
      let s2 = S.filter (`notElem` xs) $ freeVarsComp e
      S.union s1 s2
    C.UpIntro v ->
      freeVarsValue v
    C.UpElim x e1 e2 -> do
      let s1 = freeVarsComp e1
      let s2 = S.filter (/= x) $ freeVarsComp e2
      S.union s1 s2
    C.EnumElim v caseList -> do
      let s1 = freeVarsValue v
      let (_, es) = unzip caseList
      let s2 = S.unions (map freeVarsComp es)
      S.union s1 s2
    C.ArrayAccess _ array index ->
      S.union (freeVarsValue array) (freeVarsValue index)
    C.Primitive prim ->
      case prim of
        C.PrimOp _ vs ->
          S.unions $ map freeVarsValue vs
        C.Magic der ->
          foldMap freeVarsValue der

freeVarsValue :: C.Value -> S.Set Ident
freeVarsValue v =
  case v of
    C.VarLocal x ->
      S.singleton x
    C.VarLocalIdeal x ->
      S.singleton x
    C.SigmaIntro vs ->
      S.unions $ map freeVarsValue vs
    C.ArrayIntro _ vs ->
      S.unions $ map freeVarsValue vs
    _ ->
      S.empty
