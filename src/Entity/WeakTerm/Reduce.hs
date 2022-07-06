module Entity.WeakTerm.Reduce (reduce) where

import Context.Gensym
import Control.Comonad.Cofree
import Control.Monad
import qualified Data.IntMap as IntMap
import Entity.EnumCase
import qualified Entity.Ident.Reify as Ident
import Entity.LamKind
import Entity.WeakTerm
import Entity.WeakTerm.FreeVars
import Entity.WeakTerm.Subst

reduce :: Context -> WeakTerm -> IO WeakTerm
reduce ctx term =
  case term of
    m :< WeakTermPi xts cod -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM (reduce ctx) ts
      cod' <- reduce ctx cod
      return $ m :< WeakTermPi (zip3 ms xs ts') cod'
    m :< WeakTermPiIntro kind xts e
      | LamKindFix (_, x, _) <- kind,
        x `notElem` freeVars e ->
        reduce ctx $ m :< WeakTermPiIntro LamKindNormal xts e
      | otherwise -> do
        let (ms, xs, ts) = unzip3 xts
        ts' <- mapM (reduce ctx) ts
        e' <- reduce ctx e
        case kind of
          LamKindFix (mx, x, t) -> do
            t' <- reduce ctx t
            return (m :< WeakTermPiIntro (LamKindFix (mx, x, t')) (zip3 ms xs ts') e')
          _ ->
            return (m :< WeakTermPiIntro kind (zip3 ms xs ts') e')
    m :< WeakTermPiElim e es -> do
      e' <- reduce ctx e
      es' <- mapM (reduce ctx) es
      case e' of
        (_ :< WeakTermPiIntro LamKindNormal xts body)
          | length xts == length es' -> do
            let xs = map (\(_, x, _) -> Ident.toInt x) xts
            let sub = IntMap.fromList $ zip xs es'
            subst ctx sub body >>= reduce ctx
        _ ->
          return $ m :< WeakTermPiElim e' es'
    m :< WeakTermSigma xts -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM (reduce ctx) ts
      return $ m :< WeakTermSigma (zip3 ms xs ts')
    m :< WeakTermSigmaIntro es -> do
      es' <- mapM (reduce ctx) es
      return $ m :< WeakTermSigmaIntro es'
    m :< WeakTermSigmaElim xts e1 e2 -> do
      e1' <- reduce ctx e1
      case e1' of
        _ :< WeakTermSigmaIntro es
          | length xts == length es -> do
            let xs = map (\(_, x, _) -> Ident.toInt x) xts
            let sub = IntMap.fromList $ zip xs es
            subst ctx sub e2 >>= reduce ctx
        _ -> do
          e2' <- reduce ctx e2
          return $ m :< WeakTermSigmaElim xts e1' e2'
    _ :< WeakTermLet (_, x, _) e1 e2 -> do
      e1' <- reduce ctx e1
      let sub = IntMap.fromList [(Ident.toInt x, e1')]
      subst ctx sub e2
    m :< WeakTermEnumElim (e, t) les -> do
      e' <- reduce ctx e
      let (ls, es) = unzip les
      es' <- mapM (reduce ctx) es
      let les' = zip ls es'
      let les'' = zip (map unwrap ls) es'
      t' <- reduce ctx t
      case e' of
        (_ :< WeakTermEnumIntro labelInfo l) ->
          case lookup (EnumCaseLabel labelInfo l) les'' of
            Just body ->
              reduce ctx body
            Nothing ->
              case lookup EnumCaseDefault les'' of
                Just body ->
                  reduce ctx body
                Nothing ->
                  return $ m :< WeakTermEnumElim (e', t') les'
        _ ->
          return $ m :< WeakTermEnumElim (e', t') les'
    _ :< WeakTermQuestion e _ ->
      reduce ctx e
    m :< WeakTermMagic der -> do
      der' <- mapM (reduce ctx) der
      return $ m :< WeakTermMagic der'
    m :< WeakTermMatch mSubject (e, t) clauseList -> do
      e' <- reduce ctx e
      -- let lamList = map (toLamList m) clauseList
      -- dataEnv <- readIORef dataEnvRef
      -- case e' of
      --   (_ :< WeakTermPiIntro (LamKindCons dataName consName _ _) _ _)
      --     | Just consNameList <- Map.lookup dataName dataEnv,
      --       consName `elem` consNameList,
      --       checkClauseListSanity consNameList clauseList -> do
      --       reduce ctx $ m :< WeakTermPiElim e' (resultType : lamList)
      --   _ -> do
      -- resultType' <- reduce ctx resultType
      mSubject' <- mapM (reduce ctx) mSubject
      t' <- reduce ctx t
      clauseList' <- forM clauseList $ \((mPat, name, xts), body) -> do
        body' <- reduce ctx body
        return ((mPat, name, xts), body')
      return $ m :< WeakTermMatch mSubject' (e', t') clauseList'
    m :< WeakTermNoema s e -> do
      s' <- reduce ctx s
      e' <- reduce ctx e
      return $ m :< WeakTermNoema s' e'
    m :< WeakTermNoemaIntro s e -> do
      e' <- reduce ctx e
      return $ m :< WeakTermNoemaIntro s e'
    m :< WeakTermNoemaElim s e -> do
      e' <- reduce ctx e
      return $ m :< WeakTermNoemaElim s e'
    m :< WeakTermArray elemType -> do
      elemType' <- reduce ctx elemType
      return $ m :< WeakTermArray elemType'
    m :< WeakTermArrayIntro elemType elems -> do
      elemType' <- reduce ctx elemType
      elems' <- mapM (reduce ctx) elems
      return $ m :< WeakTermArrayIntro elemType' elems'
    m :< WeakTermArrayAccess subject elemType array index -> do
      subject' <- reduce ctx subject
      elemType' <- reduce ctx elemType
      array' <- reduce ctx array
      index' <- reduce ctx index
      return $ m :< WeakTermArrayAccess subject' elemType' array' index'
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
