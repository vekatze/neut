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
reduce :: Axis -> Term -> IO Term
reduce axis term =
  case term of
    (m :< TermPi xts cod) -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM (reduce axis) ts
      cod' <- reduce axis cod
      return (m :< TermPi (zip3 ms xs ts') cod')
    (m :< TermPiIntro kind xts e) -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM (reduce axis) ts
      e' <- reduce axis e
      case kind of
        LamKindFix (mx, x, t) -> do
          t' <- reduce axis t
          return (m :< TermPiIntro (LamKindFix (mx, x, t')) (zip3 ms xs ts') e')
        _ ->
          return (m :< TermPiIntro kind (zip3 ms xs ts') e')
    (m :< TermPiElim e es) -> do
      e' <- reduce axis e
      es' <- mapM (reduce axis) es
      let app = TermPiElim e' es'
      case e' of
        -- (_ :< TermPiIntro opacity LamKindNormal xts body)
        (_ :< TermPiIntro LamKindNormal xts (_ :< body))
          | length xts == length es' -> do
            let xs = map (\(_, x, _) -> Ident.toInt x) xts
            let sub = IntMap.fromList $ zip xs es'
            subst axis sub (m :< body) >>= reduce axis
        _ ->
          return (m :< app)
    m :< TermSigma xts -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM (reduce axis) ts
      return $ m :< TermSigma (zip3 ms xs ts')
    m :< TermSigmaIntro es -> do
      es' <- mapM (reduce axis) es
      return $ m :< TermSigmaIntro es'
    m :< TermSigmaElim xts e1 e2 -> do
      e1' <- reduce axis e1
      case e1' of
        _ :< TermSigmaIntro es
          | length xts == length es -> do
            let xs = map (\(_, x, _) -> Ident.toInt x) xts
            let sub = IntMap.fromList $ zip xs es
            subst axis sub e2 >>= reduce axis
        _ -> do
          e2' <- reduce axis e2
          return $ m :< TermSigmaElim xts e1' e2'
    _ :< TermLet (_, x, _) e1 e2 -> do
      e1' <- reduce axis e1
      let sub = IntMap.fromList [(Ident.toInt x, e1')]
      subst axis sub e2
    (m :< TermEnumElim (e, t) les) -> do
      e' <- reduce axis e
      let (ls, es) = unzip les
      es' <- mapM (reduce axis) es
      let les' = zip ls es'
      let les'' = zip (map unwrap ls) es'
      t' <- reduce axis t
      case e' of
        (_ :< TermEnumIntro l) ->
          case lookup (EnumCaseLabel l) les'' of
            Just (_ :< body) ->
              reduce axis (m :< body)
            Nothing ->
              case lookup EnumCaseDefault les'' of
                Just (_ :< body) ->
                  reduce axis (m :< body)
                Nothing ->
                  return (m :< TermEnumElim (e', t') les')
        _ ->
          return (m :< TermEnumElim (e', t') les')
    (m :< TermMagic der) -> do
      der' <- traverse (reduce axis) der
      return (m :< TermMagic der')
    (m :< TermMatch mSubject (e, t) clauseList) -> do
      e' <- reduce axis e
      -- let lamList = map (toLamList m) clauseList
      -- dataEnv <- readIORef dataEnvRef
      -- case e' of
      -- (_ :< TermPiIntro (LamKindCons dataName consName _ _) _ _)
      --   | Just consNameList <- Map.lookup dataName dataEnv,
      --     consName `elem` consNameList,
      --     checkClauseListSanity consNameList clauseList -> do
      --     let app = m :< TermPiElim e' lamList
      --     reduce axis app
      -- _ -> do
      mSubject' <- mapM (reduce axis) mSubject
      t' <- reduce axis t
      clauseList' <- forM clauseList $ \((mPat, name, xts), body) -> do
        body' <- reduce axis body
        return ((mPat, name, xts), body')
      return (m :< TermMatch mSubject' (e', t') clauseList')
    m :< TermNoema s e -> do
      s' <- reduce axis s
      e' <- reduce axis e
      return $ m :< TermNoema s' e'
    m :< TermNoemaIntro s e -> do
      e' <- reduce axis e
      return $ m :< TermNoemaIntro s e'
    m :< TermNoemaElim s e -> do
      e' <- reduce axis e
      return $ m :< TermNoemaElim s e'
    _ :< TermArray _ ->
      return term
    m :< TermArrayIntro elemType elems -> do
      elems' <- mapM (reduce axis) elems
      return $ m :< TermArrayIntro elemType elems'
    m :< TermArrayAccess subject elemType array index -> do
      subject' <- reduce axis subject
      array' <- reduce axis array
      index' <- reduce axis index
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
