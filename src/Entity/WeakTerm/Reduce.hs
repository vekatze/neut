module Entity.WeakTerm.Reduce (reduce) where

import Control.Comonad.Cofree
import Control.Monad
import qualified Data.IntMap as IntMap
import Entity.EnumCase
import qualified Entity.Ident.Reify as Ident
import Entity.LamKind
import Entity.WeakTerm
import Entity.WeakTerm.FreeVars
import qualified Entity.WeakTerm.Subst as Subst

reduce :: Subst.Context m => WeakTerm -> m WeakTerm
reduce term =
  case term of
    m :< WeakTermPi xts cod -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM reduce ts
      cod' <- reduce cod
      return $ m :< WeakTermPi (zip3 ms xs ts') cod'
    m :< WeakTermPiIntro kind xts e
      | LamKindFix (_, x, _) <- kind,
        x `notElem` freeVars e ->
        reduce $ m :< WeakTermPiIntro LamKindNormal xts e
      | otherwise -> do
        let (ms, xs, ts) = unzip3 xts
        ts' <- mapM reduce ts
        e' <- reduce e
        case kind of
          LamKindFix (mx, x, t) -> do
            t' <- reduce t
            return (m :< WeakTermPiIntro (LamKindFix (mx, x, t')) (zip3 ms xs ts') e')
          _ ->
            return (m :< WeakTermPiIntro kind (zip3 ms xs ts') e')
    m :< WeakTermPiElim e es -> do
      e' <- reduce e
      es' <- mapM reduce es
      case e' of
        (_ :< WeakTermPiIntro LamKindNormal xts body)
          | length xts == length es' -> do
            let xs = map (\(_, x, _) -> Ident.toInt x) xts
            let sub = IntMap.fromList $ zip xs es'
            Subst.subst sub body >>= reduce
        _ ->
          return $ m :< WeakTermPiElim e' es'
    m :< WeakTermSigma xts -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM reduce ts
      return $ m :< WeakTermSigma (zip3 ms xs ts')
    m :< WeakTermSigmaIntro es -> do
      es' <- mapM reduce es
      return $ m :< WeakTermSigmaIntro es'
    m :< WeakTermSigmaElim xts e1 e2 -> do
      e1' <- reduce e1
      case e1' of
        _ :< WeakTermSigmaIntro es
          | length xts == length es -> do
            let xs = map (\(_, x, _) -> Ident.toInt x) xts
            let sub = IntMap.fromList $ zip xs es
            Subst.subst sub e2 >>= reduce
        _ -> do
          e2' <- reduce e2
          return $ m :< WeakTermSigmaElim xts e1' e2'
    _ :< WeakTermLet (_, x, _) e1 e2 -> do
      e1' <- reduce e1
      let sub = IntMap.fromList [(Ident.toInt x, e1')]
      Subst.subst sub e2
    m :< WeakTermEnumElim (e, t) les -> do
      e' <- reduce e
      let (ls, es) = unzip les
      es' <- mapM reduce es
      let les' = zip ls es'
      let les'' = zip (map unwrap ls) es'
      t' <- reduce t
      case e' of
        (_ :< WeakTermEnumIntro label) ->
          case lookup (EnumCaseLabel label) les'' of
            Just body ->
              reduce body
            Nothing ->
              case lookup EnumCaseDefault les'' of
                Just body ->
                  reduce body
                Nothing ->
                  return $ m :< WeakTermEnumElim (e', t') les'
        _ ->
          return $ m :< WeakTermEnumElim (e', t') les'
    _ :< WeakTermQuestion e _ ->
      reduce e
    m :< WeakTermMagic der -> do
      der' <- mapM reduce der
      return $ m :< WeakTermMagic der'
    m :< WeakTermMatch mSubject (e, t) clauseList -> do
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
      mSubject' <- mapM reduce mSubject
      t' <- reduce t
      clauseList' <- forM clauseList $ \((mPat, name, arity, xts), body) -> do
        body' <- reduce body
        return ((mPat, name, arity, xts), body')
      return $ m :< WeakTermMatch mSubject' (e', t') clauseList'
    m :< WeakTermNoema s e -> do
      s' <- reduce s
      e' <- reduce e
      return $ m :< WeakTermNoema s' e'
    m :< WeakTermNoemaIntro s e -> do
      e' <- reduce e
      return $ m :< WeakTermNoemaIntro s e'
    m :< WeakTermNoemaElim s e -> do
      e' <- reduce e
      return $ m :< WeakTermNoemaElim s e'
    m :< WeakTermArray elemType -> do
      elemType' <- reduce elemType
      return $ m :< WeakTermArray elemType'
    m :< WeakTermArrayIntro elemType elems -> do
      elemType' <- reduce elemType
      elems' <- mapM reduce elems
      return $ m :< WeakTermArrayIntro elemType' elems'
    m :< WeakTermArrayAccess subject elemType array index -> do
      subject' <- reduce subject
      elemType' <- reduce elemType
      array' <- reduce array
      index' <- reduce index
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
