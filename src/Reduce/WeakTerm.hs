module Reduce.WeakTerm
  ( reduceWeakTerm,
    substWeakTerm,
  )
where

import Control.Comonad.Cofree (Cofree (..), unwrap)
import Control.Monad (forM)
import Data.Basic
  ( EnumCaseF (EnumCaseDefault, EnumCaseLabel),
    Hint,
    LamKind (LamKindCons, LamKindFix, LamKindNormal),
    asInt,
  )
import Data.Global (dataEnvRef, newIdentFromIdent)
import qualified Data.HashMap.Lazy as Map
import Data.IORef (readIORef)
import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import Data.WeakTerm
  ( SubstWeakTerm,
    WeakBinder,
    WeakPattern,
    WeakTerm,
    WeakTermF
      ( WeakTermAster,
        WeakTermCase,
        WeakTermConst,
        WeakTermDerangement,
        WeakTermEnum,
        WeakTermEnumElim,
        WeakTermEnumIntro,
        WeakTermFloat,
        WeakTermIgnore,
        WeakTermInt,
        WeakTermPi,
        WeakTermPiElim,
        WeakTermPiIntro,
        WeakTermQuestion,
        WeakTermTau,
        WeakTermVar,
        WeakTermVarGlobal
      ),
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
    m :< WeakTermEnumElim (e, t) les -> do
      e' <- reduceWeakTerm e
      let (ls, es) = unzip les
      es' <- mapM reduceWeakTerm es
      let les' = zip ls es'
      -- let les'' = zip (map snd ls) es'
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
    m :< WeakTermDerangement i es -> do
      es' <- mapM reduceWeakTerm es
      return $ m :< WeakTermDerangement i es'
    m :< WeakTermCase resultType mSubject (e, t) clauseList -> do
      e' <- reduceWeakTerm e
      let lamList = map (toLamList m) clauseList
      dataEnv <- readIORef dataEnvRef
      case e' of
        (_ :< WeakTermPiIntro (LamKindCons dataName consName) _ _)
          | Just consNameList <- Map.lookup dataName dataEnv,
            consName `elem` consNameList,
            checkClauseListSanity consNameList clauseList -> do
            let app = m :< WeakTermPiElim e' (resultType : lamList)
            reduceWeakTerm app
        _ -> do
          resultType' <- reduceWeakTerm resultType
          mSubject' <- mapM reduceWeakTerm mSubject
          t' <- reduceWeakTerm t
          clauseList' <- forM clauseList $ \((mPat, name, xts), body) -> do
            body' <- reduceWeakTerm body
            return ((mPat, name, xts), body')
          return $ m :< WeakTermCase resultType' mSubject' (e', t') clauseList'
    _ ->
      return term

checkClauseListSanity :: [T.Text] -> [(WeakPattern, WeakTerm)] -> Bool
checkClauseListSanity consNameList clauseList =
  case (consNameList, clauseList) of
    ([], []) ->
      True
    (consName : restConsNameList, ((_, name, _), _) : restClauseList)
      | consName == name ->
        checkClauseListSanity restConsNameList restClauseList
    _ ->
      False

toLamList :: Hint -> (WeakPattern, WeakTerm) -> WeakTerm
toLamList m ((_, _, xts), body) =
  m :< WeakTermPiIntro LamKindNormal xts body

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
    m :< WeakTermDerangement i es -> do
      es' <- mapM (substWeakTerm sub) es
      return $ m :< WeakTermDerangement i es'
    m :< WeakTermCase resultType mSubject (e, t) clauseList -> do
      resultType' <- substWeakTerm sub resultType
      mSubject' <- mapM (substWeakTerm sub) mSubject
      e' <- substWeakTerm sub e
      t' <- substWeakTerm sub t
      clauseList' <- forM clauseList $ \((mPat, name, xts), body) -> do
        (xts', body') <- substWeakTerm' sub xts body
        return ((mPat, name, xts'), body')
      return $ m :< WeakTermCase resultType' mSubject' (e', t') clauseList'
    m :< WeakTermIgnore e -> do
      e' <- substWeakTerm sub e
      return $ m :< WeakTermIgnore e'

substWeakTerm' ::
  SubstWeakTerm ->
  [WeakBinder] ->
  WeakTerm ->
  IO ([WeakBinder], WeakTerm)
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
