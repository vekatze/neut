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

reduce :: Axis -> WeakTerm -> IO WeakTerm
reduce axis term =
  case term of
    m :< WeakTermPi xts cod -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM (reduce axis) ts
      cod' <- reduce axis cod
      return $ m :< WeakTermPi (zip3 ms xs ts') cod'
    m :< WeakTermPiIntro kind xts e
      | LamKindFix (_, x, _) <- kind,
        x `notElem` freeVars e ->
        reduce axis $ m :< WeakTermPiIntro LamKindNormal xts e
      | otherwise -> do
        let (ms, xs, ts) = unzip3 xts
        ts' <- mapM (reduce axis) ts
        e' <- reduce axis e
        case kind of
          LamKindFix (mx, x, t) -> do
            t' <- reduce axis t
            return (m :< WeakTermPiIntro (LamKindFix (mx, x, t')) (zip3 ms xs ts') e')
          _ ->
            return (m :< WeakTermPiIntro kind (zip3 ms xs ts') e')
    m :< WeakTermPiElim e es -> do
      e' <- reduce axis e
      es' <- mapM (reduce axis) es
      case e' of
        (_ :< WeakTermPiIntro LamKindNormal xts body)
          | length xts == length es' -> do
            let xs = map (\(_, x, _) -> Ident.toInt x) xts
            let sub = IntMap.fromList $ zip xs es'
            subst axis sub body >>= reduce axis
        _ ->
          return $ m :< WeakTermPiElim e' es'
    m :< WeakTermSigma xts -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM (reduce axis) ts
      return $ m :< WeakTermSigma (zip3 ms xs ts')
    m :< WeakTermSigmaIntro es -> do
      es' <- mapM (reduce axis) es
      return $ m :< WeakTermSigmaIntro es'
    m :< WeakTermSigmaElim xts e1 e2 -> do
      e1' <- reduce axis e1
      case e1' of
        _ :< WeakTermSigmaIntro es
          | length xts == length es -> do
            let xs = map (\(_, x, _) -> Ident.toInt x) xts
            let sub = IntMap.fromList $ zip xs es
            subst axis sub e2 >>= reduce axis
        _ -> do
          e2' <- reduce axis e2
          return $ m :< WeakTermSigmaElim xts e1' e2'
    _ :< WeakTermLet (_, x, _) e1 e2 -> do
      e1' <- reduce axis e1
      let sub = IntMap.fromList [(Ident.toInt x, e1')]
      subst axis sub e2
    m :< WeakTermEnumElim (e, t) les -> do
      e' <- reduce axis e
      let (ls, es) = unzip les
      es' <- mapM (reduce axis) es
      let les' = zip ls es'
      let les'' = zip (map unwrap ls) es'
      t' <- reduce axis t
      case e' of
        (_ :< WeakTermEnumIntro l) ->
          case lookup (EnumCaseLabel l) les'' of
            Just body ->
              reduce axis body
            Nothing ->
              case lookup EnumCaseDefault les'' of
                Just body ->
                  reduce axis body
                Nothing ->
                  return $ m :< WeakTermEnumElim (e', t') les'
        _ ->
          return $ m :< WeakTermEnumElim (e', t') les'
    _ :< WeakTermQuestion e _ ->
      reduce axis e
    m :< WeakTermMagic der -> do
      der' <- mapM (reduce axis) der
      return $ m :< WeakTermMagic der'
    m :< WeakTermMatch mSubject (e, t) clauseList -> do
      e' <- reduce axis e
      -- let lamList = map (toLamList m) clauseList
      -- dataEnv <- readIORef dataEnvRef
      -- case e' of
      --   (_ :< WeakTermPiIntro (LamKindCons dataName consName _ _) _ _)
      --     | Just consNameList <- Map.lookup dataName dataEnv,
      --       consName `elem` consNameList,
      --       checkClauseListSanity consNameList clauseList -> do
      --       reduce axis $ m :< WeakTermPiElim e' (resultType : lamList)
      --   _ -> do
      -- resultType' <- reduce axis resultType
      mSubject' <- mapM (reduce axis) mSubject
      t' <- reduce axis t
      clauseList' <- forM clauseList $ \((mPat, name, xts), body) -> do
        body' <- reduce axis body
        return ((mPat, name, xts), body')
      return $ m :< WeakTermMatch mSubject' (e', t') clauseList'
    m :< WeakTermNoema s e -> do
      s' <- reduce axis s
      e' <- reduce axis e
      return $ m :< WeakTermNoema s' e'
    m :< WeakTermNoemaIntro s e -> do
      e' <- reduce axis e
      return $ m :< WeakTermNoemaIntro s e'
    m :< WeakTermNoemaElim s e -> do
      e' <- reduce axis e
      return $ m :< WeakTermNoemaElim s e'
    m :< WeakTermArray elemType -> do
      elemType' <- reduce axis elemType
      return $ m :< WeakTermArray elemType'
    m :< WeakTermArrayIntro elemType elems -> do
      elemType' <- reduce axis elemType
      elems' <- mapM (reduce axis) elems
      return $ m :< WeakTermArrayIntro elemType' elems'
    m :< WeakTermArrayAccess subject elemType array index -> do
      subject' <- reduce axis subject
      elemType' <- reduce axis elemType
      array' <- reduce axis array
      index' <- reduce axis index
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
