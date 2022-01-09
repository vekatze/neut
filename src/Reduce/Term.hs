module Reduce.Term
  ( reduceTerm,
  )
where

import Control.Comonad.Cofree (Cofree (..), unwrap)
import Control.Monad (forM)
import Data.Basic
  ( BinderF,
    EnumCaseF (EnumCaseDefault, EnumCaseLabel),
    LamKindF (LamKindFix, LamKindNormal),
    asInt,
  )
import Data.Global (newIdentFromIdent)
import qualified Data.IntMap as IntMap
import Data.Term
  ( SubstTerm,
    Term,
    TermF
      ( TermConst,
        TermDerangement,
        TermEnum,
        TermEnumElim,
        TermEnumIntro,
        TermFloat,
        TermIgnore,
        TermInt,
        TermMatch,
        TermPi,
        TermPiElim,
        TermPiIntro,
        TermTau,
        TermVar,
        TermVarGlobal
      ),
  )

-- reduce given term assuming its purity
reduceTerm :: Term -> IO Term
reduceTerm term =
  case term of
    (m :< TermPi xts cod) -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM reduceTerm ts
      cod' <- reduceTerm cod
      return (m :< TermPi (zip3 ms xs ts') cod')
    (m :< TermPiIntro kind xts e) -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM reduceTerm ts
      e' <- reduceTerm e
      case kind of
        LamKindFix (mx, x, t) -> do
          t' <- reduceTerm t
          return (m :< TermPiIntro (LamKindFix (mx, x, t')) (zip3 ms xs ts') e')
        _ ->
          return (m :< TermPiIntro kind (zip3 ms xs ts') e')
    (m :< TermPiElim e es) -> do
      e' <- reduceTerm e
      es' <- mapM reduceTerm es
      let app = TermPiElim e' es'
      case e' of
        -- (_ :< TermPiIntro opacity LamKindNormal xts body)
        (_ :< TermPiIntro LamKindNormal xts (_ :< body))
          | length xts == length es' -> do
            let xs = map (\(_, x, _) -> asInt x) xts
            let sub = IntMap.fromList $ zip xs es'
            substTerm sub (m :< body) >>= reduceTerm
        _ ->
          return (m :< app)
    (m :< TermEnumElim (e, t) les) -> do
      e' <- reduceTerm e
      let (ls, es) = unzip les
      es' <- mapM reduceTerm es
      let les' = zip ls es'
      let les'' = zip (map unwrap ls) es'
      t' <- reduceTerm t
      case e' of
        (_ :< TermEnumIntro l) ->
          case lookup (EnumCaseLabel l) les'' of
            Just (_ :< body) ->
              reduceTerm (m :< body)
            Nothing ->
              case lookup EnumCaseDefault les'' of
                Just (_ :< body) ->
                  reduceTerm (m :< body)
                Nothing ->
                  return (m :< TermEnumElim (e', t') les')
        _ ->
          return (m :< TermEnumElim (e', t') les')
    (m :< TermDerangement i es) -> do
      es' <- mapM reduceTerm es
      return (m :< TermDerangement i es')
    (m :< TermMatch mSubject (e, t) clauseList) -> do
      e' <- reduceTerm e
      -- let lamList = map (toLamList m) clauseList
      -- dataEnv <- readIORef dataEnvRef
      -- case e' of
      -- (_ :< TermPiIntro (LamKindCons dataName consName _ _) _ _)
      --   | Just consNameList <- Map.lookup dataName dataEnv,
      --     consName `elem` consNameList,
      --     checkClauseListSanity consNameList clauseList -> do
      --     let app = m :< TermPiElim e' lamList
      --     reduceTerm app
      -- _ -> do
      mSubject' <- mapM reduceTerm mSubject
      t' <- reduceTerm t
      clauseList' <- forM clauseList $ \((mPat, name, xts), body) -> do
        body' <- reduceTerm body
        return ((mPat, name, xts), body')
      return (m :< TermMatch mSubject' (e', t') clauseList')
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

substTerm :: SubstTerm -> Term -> IO Term
substTerm sub term =
  case term of
    (_ :< TermTau) ->
      return term
    (_ :< TermVar x)
      | Just e <- IntMap.lookup (asInt x) sub ->
        return e
      | otherwise ->
        return term
    (_ :< TermVarGlobal {}) ->
      return term
    (m :< TermPi xts t) -> do
      (xts', t') <- substTerm' sub xts t
      return (m :< TermPi xts' t')
    (m :< TermPiIntro kind xts e) -> do
      case kind of
        LamKindFix xt -> do
          (xt' : xts', e') <- substTerm' sub (xt : xts) e
          return (m :< TermPiIntro (LamKindFix xt') xts' e')
        _ -> do
          (xts', e') <- substTerm' sub xts e
          return (m :< TermPiIntro kind xts' e')
    (m :< TermPiElim e es) -> do
      e' <- substTerm sub e
      es' <- mapM (substTerm sub) es
      return (m :< TermPiElim e' es')
    (_ :< TermConst _) ->
      return term
    (_ :< TermInt {}) ->
      return term
    (_ :< TermFloat {}) ->
      return term
    (_ :< TermEnum _) ->
      return term
    (_ :< TermEnumIntro _) ->
      return term
    (m :< TermEnumElim (e, t) branchList) -> do
      t' <- substTerm sub t
      e' <- substTerm sub e
      let (caseList, es) = unzip branchList
      es' <- mapM (substTerm sub) es
      return (m :< TermEnumElim (e', t') (zip caseList es'))
    (m :< TermDerangement i es) -> do
      es' <- mapM (substTerm sub) es
      return (m :< TermDerangement i es')
    (m :< TermMatch mSubject (e, t) clauseList) -> do
      mSubject' <- mapM (substTerm sub) mSubject
      e' <- substTerm sub e
      t' <- substTerm sub t
      clauseList' <- forM clauseList $ \((mPat, name, xts), body) -> do
        (xts', body') <- substTerm' sub xts body
        return ((mPat, name, xts'), body')
      return (m :< TermMatch mSubject' (e', t') clauseList')
    (m :< TermIgnore e) -> do
      e' <- substTerm sub e
      return (m :< TermIgnore e')

substTerm' ::
  SubstTerm ->
  [BinderF Term] ->
  Term ->
  IO ([BinderF Term], Term)
substTerm' sub binder e =
  case binder of
    [] -> do
      e' <- substTerm sub e
      return ([], e')
    ((m, x, t) : xts) -> do
      t' <- substTerm sub t
      x' <- newIdentFromIdent x
      let sub' = IntMap.insert (asInt x) (m :< TermVar x') sub
      (xts', e') <- substTerm' sub' xts e
      return ((m, x', t') : xts', e')
