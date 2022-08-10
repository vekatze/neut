module Entity.Comp.Reduce (reduce) where

import qualified Context.CompDefinition as CompDefinition
import qualified Context.Gensym as Gensym
import Control.Comonad.Cofree.Class
import qualified Data.IntMap as IntMap
import Entity.Comp
import Entity.Comp.Subst
import Entity.EnumCase
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import Entity.Opacity

class (CompDefinition.Context m, Gensym.Context m) => Context m

reduce :: Context m => Comp -> m Comp
reduce term =
  case term of
    CompPrimitive _ ->
      return term
    CompPiElimDownElim v ds -> do
      case v of
        ValueVarGlobal x _ -> do
          mDefValue <- CompDefinition.lookup x
          case mDefValue of
            Just (OpacityTransparent, xs, body) -> do
              let sub = IntMap.fromList (zip (map Ident.toInt xs) ds)
              subst sub IntMap.empty body >>= reduce
            _ ->
              return term
        _ ->
          return term
    CompSigmaElim isNoetic xs v e ->
      case v of
        ValueSigmaIntro ds
          | length ds == length xs -> do
            let sub = IntMap.fromList (zip (map Ident.toInt xs) ds)
            subst sub IntMap.empty e >>= reduce
        _ -> do
          e' <- reduce e
          case e' of
            CompUpIntro (ValueSigmaIntro ds)
              | Just ys <- mapM extractIdent ds,
                xs == ys ->
                return $ CompUpIntro v -- eta-reduce
            _ ->
              case xs of
                [] ->
                  return e'
                _ ->
                  return $ CompSigmaElim isNoetic xs v e'
    CompUpIntro _ ->
      return term
    CompUpElim x e1 e2 -> do
      e1' <- reduce e1
      case e1' of
        CompUpIntro (ValueVarLocalIdeal _) -> do
          e2' <- reduce e2
          return $ CompUpElim x e1' e2'
        CompUpIntro v -> do
          let sub = IntMap.fromList [(Ident.toInt x, v)]
          subst sub IntMap.empty e2 >>= reduce
        CompUpElim y ey1 ey2 ->
          reduce $ CompUpElim y ey1 $ CompUpElim x ey2 e2 -- commutative conversion
        CompSigmaElim b yts vy ey ->
          reduce $ CompSigmaElim b yts vy $ CompUpElim x ey e2 -- commutative conversion
        _ -> do
          e2' <- reduce e2
          case e2' of
            CompUpIntro (ValueVarLocal y)
              | x == y ->
                return e1' -- eta-reduce
            _ ->
              return $ CompUpElim x e1' e2'
    CompEnumElim v les -> do
      let (ls, es) = unzip les
      let les' = zip (map unwrap ls) es
      case v of
        ValueEnumIntro label
          | Just body <- lookup (EnumCaseLabel label) les' ->
            reduce body
          | Just body <- lookup EnumCaseDefault les' ->
            reduce body
        ValueInt _ l
          | Just body <- lookup (EnumCaseInt (fromInteger l)) les' ->
            reduce body
          | Just body <- lookup EnumCaseDefault les' ->
            reduce body
          | otherwise -> do
            -- putStrLn "other"
            -- print v
            -- print les
            -- let (ls, es) = unzip les
            es' <- mapM reduce es
            return $ CompEnumElim v (zip ls es')
        _ -> do
          -- p "other"
          -- p' v
          -- p' les
          -- let (ls, es) = unzip les
          es' <- mapM reduce es
          return $ CompEnumElim v (zip ls es')
    CompArrayAccess {} ->
      return term

extractIdent :: Value -> Maybe Ident
extractIdent term =
  case term of
    ValueVarLocal x ->
      Just x
    _ ->
      Nothing
