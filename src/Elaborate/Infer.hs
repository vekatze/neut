module Elaborate.Infer
  ( infer,
    inferType,
    insConstraintEnv,
    insWeakTypeEnv,
  )
where

import Control.Comonad.Cofree (Cofree (..))
import Control.Monad (forM, forM_, replicateM)
import Data.Basic
  ( BinderF,
    EnumCase,
    EnumCaseF (EnumCaseDefault, EnumCaseInt, EnumCaseLabel),
    Hint,
    Ident (..),
    LamKindF (LamKindCons, LamKindFix),
    asInt,
    asText',
  )
import Data.Global
  ( constraintListRef,
    constructorEnvRef,
    holeEnvRef,
    newAster,
    newIdentFromText,
    revEnumEnvRef,
    termTypeEnvRef,
    weakTypeEnvRef,
  )
import qualified Data.HashMap.Lazy as Map
import Data.IORef (modifyIORef', readIORef)
import qualified Data.IntMap as IntMap
import Data.Log (raiseCritical, raiseError)
import Data.LowType
  ( PrimOp (..),
    asLowFloat,
    asLowInt,
    asLowTypeMaybe,
    asPrimOp,
    cmpOpSet,
  )
import qualified Data.Set as S
import Data.Term
  ( Term,
    TermF (TermEnum, TermPi, TermTau),
    lowTypeToType,
    weaken,
  )
import qualified Data.Text as T
import Data.WeakTerm
  ( SubstWeakTerm,
    WeakTerm,
    WeakTermF
      ( WeakTermAster,
        WeakTermConst,
        WeakTermDerangement,
        WeakTermEnum,
        WeakTermEnumElim,
        WeakTermEnumIntro,
        WeakTermFloat,
        WeakTermIgnore,
        WeakTermInt,
        WeakTermMatch,
        WeakTermPi,
        WeakTermPiElim,
        WeakTermPiIntro,
        WeakTermQuestion,
        WeakTermTau,
        WeakTermVar,
        WeakTermVarGlobal
      ),
    metaOf,
  )
import Reduce.WeakTerm (substWeakTerm)

type Context = [BinderF WeakTerm]

infer :: WeakTerm -> IO (WeakTerm, WeakTerm)
infer =
  infer' []

inferType :: WeakTerm -> IO WeakTerm
inferType =
  inferType' []

infer' :: Context -> WeakTerm -> IO (WeakTerm, WeakTerm)
infer' ctx term =
  case term of
    m :< WeakTermTau ->
      return (m :< WeakTermTau, m :< WeakTermTau)
    m :< WeakTermVar x -> do
      _ :< t <- lookupWeakTypeEnv m x
      return (m :< WeakTermVar x, m :< t)
    m :< WeakTermVarGlobal name -> do
      _ :< t <- lookupTermTypeEnv m name
      return (m :< WeakTermVarGlobal name, m :< t)
    m :< WeakTermPi xts t -> do
      (xts', t') <- inferPi ctx xts t
      return (m :< WeakTermPi xts' t', m :< WeakTermTau)
    m :< WeakTermPiIntro kind xts e -> do
      case kind of
        LamKindFix (mx, x, t) -> do
          t' <- inferType' ctx t
          insWeakTypeEnv x t'
          (xts', (e', tCod)) <- inferBinder ctx xts e
          let piType = m :< WeakTermPi xts' tCod
          insConstraintEnv piType t'
          return (m :< WeakTermPiIntro (LamKindFix (mx, x, t')) xts' e', piType)
        LamKindCons dataName consName consNumber dataType -> do
          dataType' <- inferType' ctx dataType
          (xts', (e', _)) <- inferBinder ctx xts e
          return (m :< WeakTermPiIntro (LamKindCons dataName consName consNumber dataType') xts' e', dataType')
        _ -> do
          (xts', (e', t')) <- inferBinder ctx xts e
          return (m :< WeakTermPiIntro kind xts' e', m :< WeakTermPi xts' t')
    m :< WeakTermPiElim e es -> do
      etls <- mapM (infer' ctx) es
      etl <- infer' ctx e
      inferPiElim ctx m etl etls
    m :< WeakTermAster x -> do
      holeEnv <- readIORef holeEnvRef
      case IntMap.lookup x holeEnv of
        Just asterInfo ->
          return asterInfo
        Nothing -> do
          (app, higherApp) <- newAsterInCtx ctx m
          modifyIORef' holeEnvRef $ \env -> IntMap.insert x (app, higherApp) env
          return (app, higherApp)
    m :< WeakTermConst x
      -- i64, f16, etc.
      | Just _ <- asLowInt x ->
        return (m :< WeakTermConst x, m :< WeakTermTau)
      | Just _ <- asLowFloat x ->
        return (m :< WeakTermConst x, m :< WeakTermTau)
      | Just op <- asPrimOp x ->
        inferExternal m x (primOpToType m op)
      | otherwise -> do
        _ :< t <- weaken <$> lookupConstTypeEnv m x
        return (m :< WeakTermConst x, m :< t)
    m :< WeakTermInt t i -> do
      t' <- inferType' [] t -- ctx == [] since t' should be i64, i8, etc. (i.e. t must be closed)
      return (m :< WeakTermInt t' i, t')
    m :< WeakTermFloat t f -> do
      t' <- inferType' [] t -- t must be closed
      return (m :< WeakTermFloat t' f, t')
    m :< WeakTermEnum name ->
      return (m :< WeakTermEnum name, m :< WeakTermTau)
    m :< WeakTermEnumIntro l -> do
      k <- lookupKind m l
      let t = m :< WeakTermEnum k
      return (m :< WeakTermEnumIntro l, t)
    m :< WeakTermEnumElim (e, _) ces -> do
      (e', t') <- infer' ctx e
      let (cs, es) = unzip ces
      (cs', tcs) <- unzip <$> mapM (inferEnumCase ctx) cs
      forM_ (zip tcs (repeat t')) $ uncurry insConstraintEnv
      (es', ts) <- unzip <$> mapM (infer' ctx) es
      h <- newTypeAsterInCtx ctx m
      forM_ (zip (repeat h) ts) $ uncurry insConstraintEnv
      return (m :< WeakTermEnumElim (e', t') (zip cs' es'), h)
    m :< WeakTermQuestion e _ -> do
      (e', te) <- infer' ctx e
      return (m :< WeakTermQuestion e' te, te)
    m :< WeakTermDerangement kind es -> do
      resultType <- newTypeAsterInCtx ctx m
      (es', _) <- unzip <$> mapM (infer' ctx) es
      return (m :< WeakTermDerangement kind es', resultType)
    m :< WeakTermMatch mSubject (e, _) clauseList -> do
      resultType <- newTypeAsterInCtx ctx m
      (e', t') <- infer' ctx e
      mSubject' <- mapM (inferSubject m ctx) mSubject
      case clauseList of
        [] ->
          return (m :< WeakTermMatch mSubject' (e', t') [], resultType) -- ex falso quodlibet
        ((_, constructorName, _), _) : _ -> do
          constructorEnv <- readIORef constructorEnvRef
          case Map.lookup constructorName constructorEnv of
            Nothing ->
              raiseCritical m $ "no such constructor defined (infer): " <> constructorName
            Just dataArgNum -> do
              holeList <- replicateM dataArgNum (newAsterInCtx ctx m)
              clauseList' <- forM clauseList $ \((mPat, name, xts), body) -> do
                (xts', (body', tBody)) <- inferBinder ctx xts body
                insConstraintEnv resultType tBody
                let xs = map (\(mx, x, t) -> (mx :< WeakTermVar x, t)) xts'
                tCons <- lookupTermTypeEnv m name
                case holeList ++ xs of
                  [] ->
                    insConstraintEnv tCons t'
                  _ -> do
                    (_, tPat) <- inferPiElim ctx m (m :< WeakTermVarGlobal name, tCons) (holeList ++ xs)
                    insConstraintEnv tPat t'
                return ((mPat, name, xts'), body')
              return (m :< WeakTermMatch mSubject' (e', t') clauseList', resultType)
    m :< WeakTermIgnore e -> do
      (e', t') <- infer' ctx e
      return (m :< WeakTermIgnore e', t')

inferSubject :: Hint -> Context -> WeakTerm -> IO WeakTerm
inferSubject m ctx subject = do
  (subject', tSub) <- infer' ctx subject
  insConstraintEnv (m :< WeakTermTau) tSub
  return subject'

inferArgs ::
  SubstWeakTerm ->
  Hint ->
  [(WeakTerm, WeakTerm)] ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  IO WeakTerm
inferArgs sub m args1 args2 cod =
  case (args1, args2) of
    ([], []) ->
      substWeakTerm sub cod
    ((e, t) : ets, (_, x, tx) : xts) -> do
      tx' <- substWeakTerm sub tx
      t' <- substWeakTerm sub t
      insConstraintEnv tx' t'
      inferArgs (IntMap.insert (asInt x) e sub) m ets xts cod
    _ ->
      raiseCritical m "invalid argument passed to inferArgs"

inferExternal :: Hint -> T.Text -> IO Term -> IO (WeakTerm, WeakTerm)
inferExternal m x comp = do
  _ :< t <- weaken <$> comp
  return (m :< WeakTermConst x, m :< t)

inferType' :: Context -> WeakTerm -> IO WeakTerm
inferType' ctx t = do
  (t', u) <- infer' ctx t
  insConstraintEnv (metaOf t :< WeakTermTau) u
  return t'

inferPi ::
  Context ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  IO ([BinderF WeakTerm], WeakTerm)
inferPi ctx binder cod =
  case binder of
    [] -> do
      (cod' :< mlPiCod) <- inferType' ctx cod
      return ([], cod' :< mlPiCod)
    ((mx, x, t) : xts) -> do
      t' <- inferType' ctx t
      insWeakTypeEnv x t'
      (xtls', tlCod) <- inferPi (ctx ++ [(mx, x, t')]) xts cod
      return ((mx, x, t') : xtls', tlCod)

inferBinder ::
  Context ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  IO ([BinderF WeakTerm], (WeakTerm, WeakTerm))
inferBinder ctx binder e =
  case binder of
    [] -> do
      etl' <- infer' ctx e
      return ([], etl')
    ((mx, x, t) : xts) -> do
      t' <- inferType' ctx t
      insWeakTypeEnv x t'
      (xts', etl') <- inferBinder (ctx ++ [(mx, x, t')]) xts e
      return ((mx, x, t') : xts', etl')

inferPiElim ::
  Context ->
  Hint ->
  (WeakTerm, WeakTerm) ->
  [(WeakTerm, WeakTerm)] ->
  IO (WeakTerm, WeakTerm)
inferPiElim ctx m (e, t) ets = do
  let es = map fst ets
  case t of
    (_ :< WeakTermPi xts (_ :< cod))
      | length xts == length ets -> do
        cod' <- inferArgs IntMap.empty m ets xts (m :< cod)
        -- cod' <- inferArgs m ets xts (m, cod)
        return (m :< WeakTermPiElim e es, cod')
    _ -> do
      ys <- mapM (const $ newIdentFromText "arg") es
      yts <- newTypeAsterListInCtx ctx $ zip ys (map metaOf es)
      cod <- newTypeAsterInCtx (ctx ++ yts) m
      insConstraintEnv (metaOf e :< WeakTermPi yts cod) t
      cod' <- inferArgs IntMap.empty m ets yts cod
      -- cod' <- inferArgs m ets yts cod
      return (m :< WeakTermPiElim e es, cod')

-- In a context (x1 : A1, ..., xn : An), this function creates metavariables
--   ?M  : Pi (x1 : A1, ..., xn : An). ?Mt @ (x1, ..., xn)
--   ?Mt : Pi (x1 : A1, ..., xn : An). Univ
-- and return ?M @ (x1, ..., xn) : ?Mt @ (x1, ..., xn).
-- Note that we can't just set `?M : Pi (x1 : A1, ..., xn : An). Univ` since
-- WeakTermAster might be used as an ordinary term, that is, a term which is not a type.
newAsterInCtx :: Context -> Hint -> IO (WeakTerm, WeakTerm)
newAsterInCtx ctx m = do
  higherAster <- newAster m
  let varSeq = map (\(mx, x, _) -> mx :< WeakTermVar x) ctx
  let higherApp = m :< WeakTermPiElim higherAster varSeq
  aster <- newAster m
  let app = m :< WeakTermPiElim aster varSeq
  return (app, higherApp)

-- In a context (x1 : A1, ..., xn : An), this function creates a metavariable
--   ?M  : Pi (x1 : A1, ..., xn : An). Univ{i}
-- and return ?M @ (x1, ..., xn) : Univ{i}.
newTypeAsterInCtx :: Context -> Hint -> IO WeakTerm
newTypeAsterInCtx ctx m = do
  let varSeq = map (\(mx, x, _) -> mx :< WeakTermVar x) ctx
  aster <- newAster m
  return (m :< WeakTermPiElim aster varSeq)

-- In context ctx == [x1, ..., xn], `newTypeAsterListInCtx ctx [y1, ..., ym]` generates
-- the following list:
--
--   [(y1,   ?M1   @ (x1, ..., xn)),
--    (y2,   ?M2   @ (x1, ..., xn, y1),
--    ...,
--    (y{m}, ?M{m} @ (x1, ..., xn, y1, ..., y{m-1}))]
--
-- inserting type information `yi : ?Mi @ (x1, ..., xn, y1, ..., y{i-1})
newTypeAsterListInCtx :: Context -> [(Ident, Hint)] -> IO [BinderF WeakTerm]
newTypeAsterListInCtx ctx ids =
  case ids of
    [] ->
      return []
    ((x, m) : rest) -> do
      t <- newTypeAsterInCtx ctx m
      insWeakTypeEnv x t
      ts <- newTypeAsterListInCtx (ctx ++ [(m, x, t)]) rest
      return $ (m, x, t) : ts

inferEnumCase :: Context -> EnumCase -> IO (EnumCase, WeakTerm)
inferEnumCase ctx weakCase =
  case weakCase of
    m :< EnumCaseLabel name -> do
      k <- lookupKind m name
      return (weakCase, m :< WeakTermEnum k)
    m :< EnumCaseDefault -> do
      h <- newTypeAsterInCtx ctx m
      return (m :< EnumCaseDefault, h)
    m :< EnumCaseInt _ ->
      raiseCritical m "enum-case-int shouldn't be used in the target language"

insConstraintEnv :: WeakTerm -> WeakTerm -> IO ()
insConstraintEnv t1 t2 =
  modifyIORef' constraintListRef $ (:) (t1, t2)

insWeakTypeEnv :: Ident -> WeakTerm -> IO ()
insWeakTypeEnv (I (_, i)) t =
  modifyIORef' weakTypeEnvRef $ IntMap.insert i t

lookupWeakTypeEnv :: Hint -> Ident -> IO WeakTerm
lookupWeakTypeEnv m s = do
  mt <- lookupWeakTypeEnvMaybe s
  case mt of
    Just t ->
      return t
    Nothing ->
      raiseCritical m $
        asText' s <> " is not found in the weak type environment."

lookupTermTypeEnv :: Hint -> T.Text -> IO WeakTerm
lookupTermTypeEnv m name = do
  termTypeEnv <- readIORef termTypeEnvRef
  case Map.lookup name termTypeEnv of
    Nothing ->
      raiseCritical m $ name <> " is not found in the term type environment."
    Just t ->
      return t

lookupWeakTypeEnvMaybe :: Ident -> IO (Maybe WeakTerm)
lookupWeakTypeEnvMaybe (I (_, s)) = do
  weakTypeEnv <- readIORef weakTypeEnvRef
  case IntMap.lookup s weakTypeEnv of
    Nothing ->
      return Nothing
    Just t ->
      return $ Just t

lookupKind :: Hint -> T.Text -> IO T.Text
lookupKind m name = do
  revEnumEnv <- readIORef revEnumEnvRef
  case Map.lookup name revEnumEnv of
    Nothing ->
      raiseError m $ "no such enum-intro is defined: " <> name
    Just (j, _) ->
      return j

lookupConstTypeEnv :: Hint -> T.Text -> IO Term
lookupConstTypeEnv m x
  | Just _ <- asLowTypeMaybe x =
    return $ m :< TermTau
  | Just op <- asPrimOp x =
    primOpToType m op
  | otherwise =
    raiseCritical m $
      "the constant `" <> x <> "` is not found in the type environment."

primOpToType :: Hint -> PrimOp -> IO Term
primOpToType m (PrimOp op domList cod) = do
  domList' <- mapM (lowTypeToType m) domList
  xs <- mapM (const (newIdentFromText "_")) domList'
  let xts = zipWith (\x t -> (m, x, t)) xs domList'
  if S.member op cmpOpSet
    then do
      let cod' = m :< TermEnum "bool"
      return $ m :< TermPi xts cod'
    else do
      cod' <- lowTypeToType m cod
      return $ m :< TermPi xts cod'
