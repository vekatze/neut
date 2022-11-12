module Entity.WeakTerm.Reduce (reduce) where

import Control.Comonad.Cofree
import Control.Monad
import qualified Data.IntMap as IntMap
import qualified Entity.EnumCase as EC
import qualified Entity.Ident.Reify as Ident
import qualified Entity.LamKind as LK
import qualified Entity.WeakTerm as WT
import Entity.WeakTerm.FreeVars
import qualified Entity.WeakTerm.Subst as Subst

reduce :: Subst.Context m => WT.WeakTerm -> m WT.WeakTerm
reduce term =
  case term of
    m :< WT.Pi xts cod -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM reduce ts
      cod' <- reduce cod
      return $ m :< WT.Pi (zip3 ms xs ts') cod'
    m :< WT.PiIntro kind xts e
      | LK.Fix (_, x, _) <- kind,
        x `notElem` freeVars e ->
          reduce $ m :< WT.PiIntro LK.Normal xts e
      | otherwise -> do
          let (ms, xs, ts) = unzip3 xts
          ts' <- mapM reduce ts
          e' <- reduce e
          case kind of
            LK.Fix (mx, x, t) -> do
              t' <- reduce t
              return (m :< WT.PiIntro (LK.Fix (mx, x, t')) (zip3 ms xs ts') e')
            _ ->
              return (m :< WT.PiIntro kind (zip3 ms xs ts') e')
    m :< WT.PiElim e es -> do
      e' <- reduce e
      es' <- mapM reduce es
      case e' of
        (_ :< WT.PiIntro LK.Normal xts body)
          | length xts == length es' -> do
              let xs = map (\(_, x, _) -> Ident.toInt x) xts
              let sub = IntMap.fromList $ zip xs es'
              Subst.subst sub body >>= reduce
        _ ->
          return $ m :< WT.PiElim e' es'
    m :< WT.Sigma xts -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM reduce ts
      return $ m :< WT.Sigma (zip3 ms xs ts')
    m :< WT.SigmaIntro es -> do
      es' <- mapM reduce es
      return $ m :< WT.SigmaIntro es'
    m :< WT.SigmaElim xts e1 e2 -> do
      e1' <- reduce e1
      case e1' of
        _ :< WT.SigmaIntro es
          | length xts == length es -> do
              let xs = map (\(_, x, _) -> Ident.toInt x) xts
              let sub = IntMap.fromList $ zip xs es
              Subst.subst sub e2 >>= reduce
        _ -> do
          e2' <- reduce e2
          return $ m :< WT.SigmaElim xts e1' e2'
    _ :< WT.Let (_, x, _) e1 e2 -> do
      e1' <- reduce e1
      let sub = IntMap.fromList [(Ident.toInt x, e1')]
      Subst.subst sub e2
    m :< WT.EnumElim (e, t) les -> do
      e' <- reduce e
      let (ls, es) = unzip les
      es' <- mapM reduce es
      let les' = zip ls es'
      let les'' = zip (map unwrap ls) es'
      t' <- reduce t
      case e' of
        (_ :< WT.EnumIntro label) ->
          case lookup (EC.Label label) les'' of
            Just body ->
              reduce body
            Nothing ->
              case lookup EC.Default les'' of
                Just body ->
                  reduce body
                Nothing ->
                  return $ m :< WT.EnumElim (e', t') les'
        _ ->
          return $ m :< WT.EnumElim (e', t') les'
    _ :< WT.Question e _ ->
      reduce e
    m :< WT.Magic der -> do
      der' <- mapM reduce der
      return $ m :< WT.Magic der'
    m :< WT.Match (e, t) clauseList -> do
      e' <- reduce e
      -- let lamList = map (toLamList m) clauseList
      -- dataEnv <- readIORef dataEnvRef
      -- case e' of
      --   (_ :< WeakTermPiIntro (LamKindCons dataName consName _ _) _ _)
      --     | Just consNameList <- Map.lookup dataName dataEnv,
      --       consName `elem` consNameList,
      --       checkClauseListSanity consNameList clauseList -> do
      --       reduce $ m :< WeakTermPiElim e' (resultType : lamList)
      --   _ -> do
      -- resultType' <- reduce resultType
      t' <- reduce t
      clauseList' <- forM clauseList $ \((mPat, name, arity, xts), body) -> do
        body' <- reduce body
        return ((mPat, name, arity, xts), body')
      return $ m :< WT.Match (e', t') clauseList'
    _ ->
      return term

-- checkClauseListSanity :: [T.Text] -> [(PatternF WeakTerm, WeakTerm)] -> Bool
-- checkClauseListSanity consNameList clauseList =
--   case (consNameList, clauseList) of
--     ([], []) ->
--       True
--     (consName : restConsNameList, ((_, name, _), _) : restClauseList)
--       | consName == name ->
--         checkClauseListSanity restConsNameList restClauseList
--     _ ->
--       False

-- toLamList :: Hint -> (PatternF WeakTerm, WeakTerm) -> WeakTerm
-- toLamList m ((_, _, xts), body) =
--   m :< WeakTermPiIntro LamKindNormal xts body
