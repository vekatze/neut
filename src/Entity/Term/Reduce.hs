module Entity.Term.Reduce (reduce) where

import Control.Comonad.Cofree
import Control.Monad
import qualified Data.IntMap as IntMap
import qualified Entity.EnumCase as EC
import qualified Entity.Ident.Reify as Ident
import Entity.LamKind
import qualified Entity.Term as TM
import qualified Entity.Term.Subst as Subst

reduce :: Subst.Context m => TM.Term -> m TM.Term
reduce term =
  case term of
    (m :< TM.Pi xts cod) -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM reduce ts
      cod' <- reduce cod
      return (m :< TM.Pi (zip3 ms xs ts') cod')
    (m :< TM.PiIntro kind xts e) -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM reduce ts
      e' <- reduce e
      case kind of
        LamKindFix (mx, x, t) -> do
          t' <- reduce t
          return (m :< TM.PiIntro (LamKindFix (mx, x, t')) (zip3 ms xs ts') e')
        _ ->
          return (m :< TM.PiIntro kind (zip3 ms xs ts') e')
    (m :< TM.PiElim e es) -> do
      e' <- reduce e
      es' <- mapM reduce es
      let app = TM.PiElim e' es'
      case e' of
        -- (_ :< TM.PiIntro opacity LamKindNormal xts body)
        (_ :< TM.PiIntro LamKindNormal xts (_ :< body))
          | length xts == length es' -> do
              let xs = map (\(_, x, _) -> Ident.toInt x) xts
              let sub = IntMap.fromList $ zip xs es'
              Subst.subst sub (m :< body) >>= reduce
        _ ->
          return (m :< app)
    m :< TM.Sigma xts -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM reduce ts
      return $ m :< TM.Sigma (zip3 ms xs ts')
    m :< TM.SigmaIntro es -> do
      es' <- mapM reduce es
      return $ m :< TM.SigmaIntro es'
    m :< TM.SigmaElim xts e1 e2 -> do
      e1' <- reduce e1
      case e1' of
        _ :< TM.SigmaIntro es
          | length xts == length es -> do
              let xs = map (\(_, x, _) -> Ident.toInt x) xts
              let sub = IntMap.fromList $ zip xs es
              Subst.subst sub e2 >>= reduce
        _ -> do
          e2' <- reduce e2
          return $ m :< TM.SigmaElim xts e1' e2'
    _ :< TM.Let (_, x, _) e1 e2 -> do
      e1' <- reduce e1
      let sub = IntMap.fromList [(Ident.toInt x, e1')]
      Subst.subst sub e2
    (m :< TM.EnumElim (e, t) les) -> do
      e' <- reduce e
      let (ls, es) = unzip les
      es' <- mapM reduce es
      let les' = zip ls es'
      let les'' = zip (map unwrap ls) es'
      t' <- reduce t
      case e' of
        (_ :< TM.EnumIntro label) ->
          case lookup (EC.Label label) les'' of
            Just (_ :< body) ->
              reduce (m :< body)
            Nothing ->
              case lookup EC.Default les'' of
                Just (_ :< body) ->
                  reduce (m :< body)
                Nothing ->
                  return (m :< TM.EnumElim (e', t') les')
        _ ->
          return (m :< TM.EnumElim (e', t') les')
    (m :< TM.Magic der) -> do
      der' <- traverse reduce der
      return (m :< TM.Magic der')
    (m :< TM.Match (e, t) clauseList) -> do
      e' <- reduce e
      -- let lamList = map (toLamList m) clauseList
      -- dataEnv <- readIORef dataEnvRef
      -- case e' of
      -- (_ :< TM.PiIntro (LamKindCons dataName consName _ _) _ _)
      --   | Just consNameList <- Map.lookup dataName dataEnv,
      --     consName `elem` consNameList,
      --     checkClauseListSanity consNameList clauseList -> do
      --     let app = m :< TM.PiElim e' lamList
      --     reduce app
      -- _ -> do
      t' <- reduce t
      clauseList' <- forM clauseList $ \((mPat, name, arity, xts), body) -> do
        body' <- reduce body
        return ((mPat, name, arity, xts), body')
      return (m :< TM.Match (e', t') clauseList')
    _ ->
      return term

-- checkClauseListSanity :: [T.Text] -> [(PatternF Term, Term)] -> Bool
-- checkClauseListSanity consNameList clauseList =
--   case (consNameList, clauseList) of
--     ([], []) ->
--       True
--     (consName : restConsNameList, ((_, name, _), _) : restClauseList)
--       | consName == name ->
--         checkClauseListSanity restConsNameList restClauseList
--     _ ->
--       False

-- toLamList :: Hint -> (PatternF Term, Term) -> Term
-- toLamList m ((_, _, xts), body) =
--   m :< TM.PiIntro LamKindNormal xts body
