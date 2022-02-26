module Reduce.WeakTerm
  ( reduceWeakTerm,
    substWeakTerm,
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
import Data.WeakTerm
  ( SubstWeakTerm,
    WeakTerm,
    WeakTermF (..),
    varWeakTerm,
  )

reduceWeakTerm :: WeakTerm -> IO WeakTerm
reduceWeakTerm term =
  case term of
    m :< WeakTermPi xts cod -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM reduceWeakTerm ts
      cod' <- reduceWeakTerm cod
      return $ m :< WeakTermPi (zip3 ms xs ts') cod'
    m :< WeakTermPiIntro kind xts e
      | LamKindFix (_, x, _) <- kind,
        x `notElem` varWeakTerm e ->
        reduceWeakTerm $ m :< WeakTermPiIntro LamKindNormal xts e
      | otherwise -> do
        let (ms, xs, ts) = unzip3 xts
        ts' <- mapM reduceWeakTerm ts
        e' <- reduceWeakTerm e
        case kind of
          LamKindFix (mx, x, t) -> do
            t' <- reduceWeakTerm t
            return (m :< WeakTermPiIntro (LamKindFix (mx, x, t')) (zip3 ms xs ts') e')
          _ ->
            return (m :< WeakTermPiIntro kind (zip3 ms xs ts') e')
    m :< WeakTermPiElim e es -> do
      e' <- reduceWeakTerm e
      es' <- mapM reduceWeakTerm es
      case e' of
        (_ :< WeakTermPiIntro LamKindNormal xts body)
          | length xts == length es' -> do
            let xs = map (\(_, x, _) -> asInt x) xts
            let sub = IntMap.fromList $ zip xs es'
            substWeakTerm sub body >>= reduceWeakTerm
        _ ->
          return $ m :< WeakTermPiElim e' es'
    m :< WeakTermSigma xts -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM reduceWeakTerm ts
      return $ m :< WeakTermSigma (zip3 ms xs ts')
    m :< WeakTermSigmaIntro es -> do
      es' <- mapM reduceWeakTerm es
      return $ m :< WeakTermSigmaIntro es'
    m :< WeakTermSigmaElim xts e1 e2 -> do
      e1' <- reduceWeakTerm e1
      case e1' of
        _ :< WeakTermSigmaIntro es
          | length xts == length es -> do
            let xs = map (\(_, x, _) -> asInt x) xts
            let sub = IntMap.fromList $ zip xs es
            substWeakTerm sub e2 >>= reduceWeakTerm
        _ -> do
          e2' <- reduceWeakTerm e2
          return $ m :< WeakTermSigmaElim xts e1' e2'
    _ :< WeakTermLet (_, x, _) e1 e2 -> do
      e1' <- reduceWeakTerm e1
      let sub = IntMap.fromList [(asInt x, e1')]
      substWeakTerm sub e2
    m :< WeakTermEnumElim (e, t) les -> do
      e' <- reduceWeakTerm e
      let (ls, es) = unzip les
      es' <- mapM reduceWeakTerm es
      let les' = zip ls es'
      let les'' = zip (map unwrap ls) es'
      t' <- reduceWeakTerm t
      case e' of
        (_ :< WeakTermEnumIntro l) ->
          case lookup (EnumCaseLabel l) les'' of
            Just body ->
              reduceWeakTerm body
            Nothing ->
              case lookup EnumCaseDefault les'' of
                Just body ->
                  reduceWeakTerm body
                Nothing ->
                  return $ m :< WeakTermEnumElim (e', t') les'
        _ ->
          return $ m :< WeakTermEnumElim (e', t') les'
    _ :< WeakTermQuestion e _ ->
      reduceWeakTerm e
    m :< WeakTermMagic der -> do
      der' <- mapM reduceWeakTerm der
      return $ m :< WeakTermMagic der'
    m :< WeakTermMatch mSubject (e, t) clauseList -> do
      e' <- reduceWeakTerm e
      -- let lamList = map (toLamList m) clauseList
      -- dataEnv <- readIORef dataEnvRef
      -- case e' of
      --   (_ :< WeakTermPiIntro (LamKindCons dataName consName _ _) _ _)
      --     | Just consNameList <- Map.lookup dataName dataEnv,
      --       consName `elem` consNameList,
      --       checkClauseListSanity consNameList clauseList -> do
      --       reduceWeakTerm $ m :< WeakTermPiElim e' (resultType : lamList)
      --   _ -> do
      -- resultType' <- reduceWeakTerm resultType
      mSubject' <- mapM reduceWeakTerm mSubject
      t' <- reduceWeakTerm t
      clauseList' <- forM clauseList $ \((mPat, name, xts), body) -> do
        body' <- reduceWeakTerm body
        return ((mPat, name, xts), body')
      return $ m :< WeakTermMatch mSubject' (e', t') clauseList'
    m :< WeakTermNoema s e -> do
      s' <- reduceWeakTerm s
      e' <- reduceWeakTerm e
      return $ m :< WeakTermNoema s' e'
    m :< WeakTermNoemaIntro s e -> do
      e' <- reduceWeakTerm e
      return $ m :< WeakTermNoemaIntro s e'
    m :< WeakTermNoemaElim s e -> do
      e' <- reduceWeakTerm e
      return $ m :< WeakTermNoemaElim s e'
    m :< WeakTermArray elemType -> do
      elemType' <- reduceWeakTerm elemType
      return $ m :< WeakTermArray elemType'
    m :< WeakTermArrayIntro elemType elems -> do
      elemType' <- reduceWeakTerm elemType
      elems' <- mapM reduceWeakTerm elems
      return $ m :< WeakTermArrayIntro elemType' elems'
    m :< WeakTermArrayAccess subject elemType array index -> do
      subject' <- reduceWeakTerm subject
      elemType' <- reduceWeakTerm elemType
      array' <- reduceWeakTerm array
      index' <- reduceWeakTerm index
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

substWeakTerm :: SubstWeakTerm -> WeakTerm -> IO WeakTerm
substWeakTerm sub term =
  case term of
    _ :< WeakTermTau ->
      return term
    _ :< WeakTermVar x
      | Just e <- IntMap.lookup (asInt x) sub ->
        return e
      | otherwise ->
        return term
    _ :< WeakTermVarGlobal {} ->
      return term
    m :< WeakTermPi xts t -> do
      (xts', t') <- substWeakTerm' sub xts t
      return $ m :< WeakTermPi xts' t'
    m :< WeakTermPiIntro kind xts e -> do
      case kind of
        LamKindFix xt -> do
          (xt' : xts', e') <- substWeakTerm' sub (xt : xts) e
          return $ m :< WeakTermPiIntro (LamKindFix xt') xts' e'
        _ -> do
          (xts', e') <- substWeakTerm' sub xts e
          return $ m :< WeakTermPiIntro kind xts' e'
    m :< WeakTermPiElim e es -> do
      e' <- substWeakTerm sub e
      es' <- mapM (substWeakTerm sub) es
      return $ m :< WeakTermPiElim e' es'
    m :< WeakTermSigma xts -> do
      (xts', _) <- substWeakTerm' sub xts (m :< WeakTermTau)
      return $ m :< WeakTermSigma xts'
    m :< WeakTermSigmaIntro es -> do
      es' <- mapM (substWeakTerm sub) es
      return $ m :< WeakTermSigmaIntro es'
    m :< WeakTermSigmaElim xts e1 e2 -> do
      e1' <- substWeakTerm sub e1
      (xts', e2') <- substWeakTerm' sub xts e2
      return $ m :< WeakTermSigmaElim xts' e1' e2'
    m :< WeakTermLet mxt e1 e2 -> do
      e1' <- substWeakTerm sub e1
      ([mxt'], e2') <- substWeakTerm' sub [mxt] e2
      return $ m :< WeakTermLet mxt' e1' e2'
    _ :< WeakTermConst _ ->
      return term
    _ :< WeakTermAster x ->
      case IntMap.lookup x sub of
        Nothing ->
          return term
        Just e2 ->
          return e2
    m :< WeakTermInt t x -> do
      t' <- substWeakTerm sub t
      return $ m :< WeakTermInt t' x
    m :< WeakTermFloat t x -> do
      t' <- substWeakTerm sub t
      return $ m :< WeakTermFloat t' x
    _ :< WeakTermEnum _ ->
      return term
    _ :< WeakTermEnumIntro _ ->
      return term
    m :< WeakTermEnumElim (e, t) branchList -> do
      t' <- substWeakTerm sub t
      e' <- substWeakTerm sub e
      let (caseList, es) = unzip branchList
      es' <- mapM (substWeakTerm sub) es
      return $ m :< WeakTermEnumElim (e', t') (zip caseList es')
    m :< WeakTermQuestion e t -> do
      e' <- substWeakTerm sub e
      t' <- substWeakTerm sub t
      return $ m :< WeakTermQuestion e' t'
    m :< WeakTermMagic der -> do
      der' <- mapM (substWeakTerm sub) der
      return $ m :< WeakTermMagic der'
    m :< WeakTermMatch mSubject (e, t) clauseList -> do
      mSubject' <- mapM (substWeakTerm sub) mSubject
      e' <- substWeakTerm sub e
      t' <- substWeakTerm sub t
      clauseList' <- forM clauseList $ \((mPat, name, xts), body) -> do
        (xts', body') <- substWeakTerm' sub xts body
        return ((mPat, name, xts'), body')
      return $ m :< WeakTermMatch mSubject' (e', t') clauseList'
    m :< WeakTermNoema s e -> do
      s' <- substWeakTerm sub s
      e' <- substWeakTerm sub e
      return $ m :< WeakTermNoema s' e'
    m :< WeakTermNoemaIntro s e -> do
      e' <- substWeakTerm sub e
      return $ m :< WeakTermNoemaIntro s e'
    m :< WeakTermNoemaElim s e -> do
      e' <- substWeakTerm sub e
      return $ m :< WeakTermNoemaElim s e'
    m :< WeakTermArray elemType -> do
      elemType' <- substWeakTerm sub elemType
      return $ m :< WeakTermArray elemType'
    m :< WeakTermArrayIntro elemType elems -> do
      elemType' <- substWeakTerm sub elemType
      elems' <- mapM (substWeakTerm sub) elems
      return $ m :< WeakTermArrayIntro elemType' elems'
    m :< WeakTermArrayAccess subject elemType array index -> do
      subject' <- substWeakTerm sub subject
      elemType' <- substWeakTerm sub elemType
      array' <- substWeakTerm sub array
      index' <- substWeakTerm sub index
      return $ m :< WeakTermArrayAccess subject' elemType' array' index'
    _ :< WeakTermText ->
      return term
    _ :< WeakTermTextIntro _ ->
      return term
    m :< WeakTermCell contentType -> do
      contentType' <- substWeakTerm sub contentType
      return $ m :< WeakTermCell contentType'
    m :< WeakTermCellIntro contentType content -> do
      contentType' <- substWeakTerm sub contentType
      content' <- substWeakTerm sub content
      return $ m :< WeakTermCellIntro contentType' content'
    m :< WeakTermCellRead cell -> do
      cell' <- substWeakTerm sub cell
      return $ m :< WeakTermCellRead cell'
    m :< WeakTermCellWrite cell newValue -> do
      cell' <- substWeakTerm sub cell
      newValue' <- substWeakTerm sub newValue
      return $ m :< WeakTermCellWrite cell' newValue'

substWeakTerm' ::
  SubstWeakTerm ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  IO ([BinderF WeakTerm], WeakTerm)
substWeakTerm' sub binder e =
  case binder of
    [] -> do
      e' <- substWeakTerm sub e
      return ([], e')
    ((m, x, t) : xts) -> do
      t' <- substWeakTerm sub t
      x' <- newIdentFromIdent x
      let sub' = IntMap.insert (asInt x) (m :< WeakTermVar x') sub
      (xts', e') <- substWeakTerm' sub' xts e
      return ((m, x', t') : xts', e')
