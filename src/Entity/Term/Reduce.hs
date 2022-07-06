module Entity.Term.Reduce (reduce) where

import Context.Gensym
import Control.Comonad.Cofree
import Control.Monad
import qualified Data.IntMap as IntMap
import Entity.EnumCase
import qualified Entity.Ident.Reify as Ident
import Entity.LamKind
import Entity.Term
import Entity.Term.Subst

-- reduce given term assuming its purity
reduce :: Context -> Term -> IO Term
reduce ctx term =
  case term of
    (m :< TermPi xts cod) -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM (reduce ctx) ts
      cod' <- reduce ctx cod
      return (m :< TermPi (zip3 ms xs ts') cod')
    (m :< TermPiIntro kind xts e) -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM (reduce ctx) ts
      e' <- reduce ctx e
      case kind of
        LamKindFix (mx, x, t) -> do
          t' <- reduce ctx t
          return (m :< TermPiIntro (LamKindFix (mx, x, t')) (zip3 ms xs ts') e')
        _ ->
          return (m :< TermPiIntro kind (zip3 ms xs ts') e')
    (m :< TermPiElim e es) -> do
      e' <- reduce ctx e
      es' <- mapM (reduce ctx) es
      let app = TermPiElim e' es'
      case e' of
        -- (_ :< TermPiIntro opacity LamKindNormal xts body)
        (_ :< TermPiIntro LamKindNormal xts (_ :< body))
          | length xts == length es' -> do
            let xs = map (\(_, x, _) -> Ident.toInt x) xts
            let sub = IntMap.fromList $ zip xs es'
            subst ctx sub (m :< body) >>= reduce ctx
        _ ->
          return (m :< app)
    m :< TermSigma xts -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM (reduce ctx) ts
      return $ m :< TermSigma (zip3 ms xs ts')
    m :< TermSigmaIntro es -> do
      es' <- mapM (reduce ctx) es
      return $ m :< TermSigmaIntro es'
    m :< TermSigmaElim xts e1 e2 -> do
      e1' <- reduce ctx e1
      case e1' of
        _ :< TermSigmaIntro es
          | length xts == length es -> do
            let xs = map (\(_, x, _) -> Ident.toInt x) xts
            let sub = IntMap.fromList $ zip xs es
            subst ctx sub e2 >>= reduce ctx
        _ -> do
          e2' <- reduce ctx e2
          return $ m :< TermSigmaElim xts e1' e2'
    _ :< TermLet (_, x, _) e1 e2 -> do
      e1' <- reduce ctx e1
      let sub = IntMap.fromList [(Ident.toInt x, e1')]
      subst ctx sub e2
    (m :< TermEnumElim (e, t) les) -> do
      e' <- reduce ctx e
      let (ls, es) = unzip les
      es' <- mapM (reduce ctx) es
      let les' = zip ls es'
      let les'' = zip (map unwrap ls) es'
      t' <- reduce ctx t
      case e' of
        (_ :< TermEnumIntro labelInfo l) ->
          case lookup (EnumCaseLabel labelInfo l) les'' of
            Just (_ :< body) ->
              reduce ctx (m :< body)
            Nothing ->
              case lookup EnumCaseDefault les'' of
                Just (_ :< body) ->
                  reduce ctx (m :< body)
                Nothing ->
                  return (m :< TermEnumElim (e', t') les')
        _ ->
          return (m :< TermEnumElim (e', t') les')
    (m :< TermMagic der) -> do
      der' <- traverse (reduce ctx) der
      return (m :< TermMagic der')
    (m :< TermMatch mSubject (e, t) clauseList) -> do
      e' <- reduce ctx e
      -- let lamList = map (toLamList m) clauseList
      -- dataEnv <- readIORef dataEnvRef
      -- case e' of
      -- (_ :< TermPiIntro (LamKindCons dataName consName _ _) _ _)
      --   | Just consNameList <- Map.lookup dataName dataEnv,
      --     consName `elem` consNameList,
      --     checkClauseListSanity consNameList clauseList -> do
      --     let app = m :< TermPiElim e' lamList
      --     reduce ctx app
      -- _ -> do
      mSubject' <- mapM (reduce ctx) mSubject
      t' <- reduce ctx t
      clauseList' <- forM clauseList $ \((mPat, name, xts), body) -> do
        body' <- reduce ctx body
        return ((mPat, name, xts), body')
      return (m :< TermMatch mSubject' (e', t') clauseList')
    m :< TermNoema s e -> do
      s' <- reduce ctx s
      e' <- reduce ctx e
      return $ m :< TermNoema s' e'
    m :< TermNoemaIntro s e -> do
      e' <- reduce ctx e
      return $ m :< TermNoemaIntro s e'
    m :< TermNoemaElim s e -> do
      e' <- reduce ctx e
      return $ m :< TermNoemaElim s e'
    _ :< TermArray _ ->
      return term
    m :< TermArrayIntro elemType elems -> do
      elems' <- mapM (reduce ctx) elems
      return $ m :< TermArrayIntro elemType elems'
    m :< TermArrayAccess subject elemType array index -> do
      subject' <- reduce ctx subject
      array' <- reduce ctx array
      index' <- reduce ctx index
      return $ m :< TermArrayAccess subject' elemType array' index'
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
--   m :< TermPiIntro LamKindNormal xts body
