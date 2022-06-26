module Scene.Elaborate.Infer
  ( infer,
    inferType,
    insConstraintEnv,
    insWeakTypeEnv,
    inferBinder,
    arrangeBinder,
  )
where

import Control.Comonad.Cofree
import Control.Monad
import qualified Data.HashMap.Lazy as Map
import Data.IORef
import qualified Data.IntMap as IntMap
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.Basic
import Entity.Global
import Entity.Log
import Entity.LowType
import Entity.Term
import Entity.WeakTerm
import Entity.WeakTerm.Reduce

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
    _ :< WeakTermTau ->
      return (term, term)
    m :< WeakTermVar x -> do
      _ :< t <- lookupWeakTypeEnv m x
      return (term, m :< t)
    m :< WeakTermVarGlobal name -> do
      _ :< t <- lookupTermTypeEnv m name
      return (term, m :< t)
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
    m :< WeakTermPiElim e@(_ :< WeakTermVarGlobal name) es -> do
      etls <- mapM (infer' ctx) es
      t <- lookupTermTypeEnv m name
      impArgEnv <- readIORef impArgEnvRef
      case Map.lookup name impArgEnv of
        Nothing -> do
          inferPiElim ctx m (e, t) etls
        Just i -> do
          holes <- forM [1 .. i] $ const $ newAsterInCtx ctx m
          inferPiElim ctx m (e, t) $ holes ++ etls
    m :< WeakTermPiElim hole@(_ :< WeakTermAster i) es -> do
      etls <- mapM (infer' ctx) es
      t <- lookupWeakTypeEnv' m i
      inferPiElim ctx m (hole, t) etls
    m :< WeakTermPiElim e es -> do
      etls <- mapM (infer' ctx) es
      etl <- infer' ctx e
      inferPiElim ctx m etl etls
    m :< WeakTermSigma xts -> do
      (xts', _) <- inferPi ctx xts (m :< WeakTermTau)
      return (m :< WeakTermSigma xts', m :< WeakTermTau)
    m :< WeakTermSigmaIntro es -> do
      ets <- mapM (infer' ctx) es
      ys <- mapM (const $ newIdentFromText "arg") es
      yts <- newTypeAsterListInCtx ctx $ zip ys (map metaOf es)
      _ <- inferArgs IntMap.empty m ets yts (m :< WeakTermTau)
      return (m :< WeakTermSigmaIntro (map fst ets), m :< WeakTermSigma yts)
    m :< WeakTermSigmaElim xts e1 e2 -> do
      (e1', t1') <- infer' ctx e1
      (xts', (e2', t)) <- inferBinder ctx xts e2
      insConstraintEnv (m :< WeakTermSigma xts') t1'
      return (m :< WeakTermSigmaElim xts' e1' e2', t)
    m :< WeakTermLet (mx, x, t) e1 e2 -> do
      (e1', t1') <- infer' ctx e1
      t' <- inferType' ctx t
      insConstraintEnv t' t1'
      insWeakTypeEnv x t'
      (e2', t2') <- infer' ctx e2 -- no context extension
      return (m :< WeakTermLet (mx, x, t') e1' e2', t2')
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
        return (term, m :< WeakTermTau)
      | Just _ <- asLowFloat x ->
        return (term, m :< WeakTermTau)
      | Just op <- asPrimOp x ->
        inferExternal m x (primOpToType m op)
      | otherwise -> do
        _ :< t <- weaken <$> lookupConstTypeEnv m x
        return (term, m :< t)
    m :< WeakTermInt t i -> do
      t' <- inferType' [] t -- ctx == [] since t' should be i64, i8, etc. (i.e. t must be closed)
      return (m :< WeakTermInt t' i, t')
    m :< WeakTermFloat t f -> do
      t' <- inferType' [] t -- t must be closed
      return (m :< WeakTermFloat t' f, t')
    m :< WeakTermEnum _ ->
      return (term, m :< WeakTermTau)
    m :< WeakTermEnumIntro l -> do
      k <- lookupKind m l
      return (term, m :< WeakTermEnum k)
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
    m :< WeakTermMagic der -> do
      case der of
        MagicCast from to value -> do
          from' <- inferType' ctx from
          to' <- inferType' ctx to
          (value', t) <- infer' ctx value
          insConstraintEnv t from'
          return (m :< WeakTermMagic (MagicCast from' to' value'), to')
        _ -> do
          der' <- mapM (infer' ctx >=> return . fst) der
          resultType <- newTypeAsterInCtx ctx m
          return (m :< WeakTermMagic der', resultType)
    m :< WeakTermMatch mSubject (e, _) clauseList -> do
      resultType <- newTypeAsterInCtx ctx m
      (e', t') <- infer' ctx e
      mSubject' <- mapM (inferSubject m ctx) mSubject
      clauseList' <- forM clauseList $ \(pat@(mPat, name, xts), body) -> do
        (xts', (body', tBody)) <- inferBinder ctx xts body
        insConstraintEnv resultType tBody
        (_, tPat) <- infer' ctx $ patternToTerm pat
        insConstraintEnv tPat t'
        return ((mPat, name, xts'), body')
      return (m :< WeakTermMatch mSubject' (e', t') clauseList', resultType)
    m :< WeakTermNoema s t -> do
      s' <- inferType' ctx s
      t' <- inferType' ctx t
      return (m :< WeakTermNoema s' t', m :< WeakTermTau)
    m :< WeakTermNoemaIntro s e -> do
      (e', t') <- infer' ctx e
      return (m :< WeakTermNoemaIntro s e', m :< WeakTermNoema (m :< WeakTermVar s) t')
    m :< WeakTermNoemaElim s e -> do
      insWeakTypeEnv s (m :< WeakTermTau)
      (e', t) <- infer' (ctx ++ [(m, s, m :< WeakTermTau)]) e
      return (m :< WeakTermNoemaElim s e', t)
    m :< WeakTermArray elemType -> do
      elemType' <- inferType' ctx elemType
      return (m :< WeakTermArray elemType', m :< WeakTermTau)
    m :< WeakTermArrayIntro _ elems -> do
      elemType <- newTypeAsterInCtx ctx m
      (elems', ts') <- unzip <$> mapM (infer' ctx) elems
      forM_ ts' $ insConstraintEnv elemType
      return (m :< WeakTermArrayIntro elemType elems', m :< WeakTermArray elemType)
    m :< WeakTermArrayAccess _ _ array index -> do
      subject <- newTypeAsterInCtx ctx m
      elemType <- newTypeAsterInCtx ctx m
      (array', tArray) <- infer' ctx array
      (index', tIndex) <- infer' ctx index
      insConstraintEnv (m :< WeakTermConst "i64") tIndex
      let noeticArrayType = m :< WeakTermNoema subject (m :< WeakTermArray elemType)
      insConstraintEnv noeticArrayType tArray
      return (m :< WeakTermArrayAccess subject elemType array' index', elemType)
    m :< WeakTermText ->
      return (term, m :< WeakTermTau)
    m :< WeakTermTextIntro _ -> do
      return (term, m :< WeakTermText)
    m :< WeakTermCell contentType -> do
      contentType' <- inferType' ctx contentType
      return (m :< WeakTermCell contentType', m :< WeakTermTau)
    m :< WeakTermCellIntro _ content -> do
      (content', contentType) <- infer' ctx content
      return (m :< WeakTermCellIntro contentType content', m :< WeakTermCell contentType)
    m :< WeakTermCellRead cell -> do
      (cell', cellType) <- infer' ctx cell
      contentType <- newTypeAsterInCtx ctx m
      subject <- newTypeAsterInCtx ctx m
      insConstraintEnv (m :< WeakTermNoema subject (m :< WeakTermCell contentType)) cellType
      return (m :< WeakTermCellRead cell', contentType)
    m :< WeakTermCellWrite cell newValue -> do
      (cell', cellType) <- infer' ctx cell
      (newValue', newValueType) <- infer' ctx newValue
      subject <- newTypeAsterInCtx ctx m
      insConstraintEnv (m :< WeakTermNoema subject (m :< WeakTermCell newValueType)) cellType
      return (m :< WeakTermCellWrite cell' newValue', m :< WeakTermEnum constTop)

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
      insConstraintEnv tx' t
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
        return (m :< WeakTermPiElim e es, cod')
      | otherwise -> do
        raiseArityMismatchError e (length xts) (length ets)
    _ -> do
      ys <- mapM (const $ newIdentFromText "arg") es
      yts <- newTypeAsterListInCtx ctx $ zip ys (map metaOf es)
      cod <- newTypeAsterInCtx (ctx ++ yts) m
      insConstraintEnv (metaOf e :< WeakTermPi yts cod) t
      cod' <- inferArgs IntMap.empty m ets yts cod
      return (m :< WeakTermPiElim e es, cod')

raiseArityMismatchError :: WeakTerm -> Int -> Int -> IO a
raiseArityMismatchError function expected actual = do
  impArgEnv <- readIORef impArgEnvRef
  case function of
    m :< WeakTermVarGlobal name
      | Just k <- Map.lookup name impArgEnv ->
        raiseError m $
          "the function `"
            <> name
            <> "` expects "
            <> T.pack (show (expected - k))
            <> " arguments, but found "
            <> T.pack (show (actual - k))
            <> "."
    m :< _ ->
      raiseError m $
        "this function expects "
          <> T.pack (show expected)
          <> " arguments, but found "
          <> T.pack (show actual)
          <> "."

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
  aster@(_ :< WeakTermAster i) <- newAster m
  modifyIORef' weakTypeEnvRef $ IntMap.insert i $ m :< WeakTermPi ctx higherApp
  let app = m :< WeakTermPiElim aster varSeq
  return (app, higherApp)

-- In a context (x1 : A1, ..., xn : An), this function creates a metavariable
--   ?M  : Pi (x1 : A1, ..., xn : An). Univ{i}
-- and return ?M @ (x1, ..., xn) : Univ{i}.
newTypeAsterInCtx :: Context -> Hint -> IO WeakTerm
newTypeAsterInCtx ctx m = do
  let varSeq = map (\(mx, x, _) -> mx :< WeakTermVar x) ctx
  aster@(_ :< WeakTermAster i) <- newAster m
  modifyIORef' weakTypeEnvRef $ IntMap.insert i $ m :< WeakTermPi ctx (m :< WeakTermTau)
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
  mt <- lookupWeakTypeEnvMaybe $ asInt s
  case mt of
    Just t ->
      return t
    Nothing ->
      raiseCritical m $
        asText' s <> " is not found in the weak type environment."

lookupWeakTypeEnv' :: Hint -> Int -> IO WeakTerm
lookupWeakTypeEnv' m s = do
  mt <- lookupWeakTypeEnvMaybe s
  case mt of
    Just t ->
      return t
    Nothing ->
      raiseCritical m $
        "the hole ?M" <> T.pack (show s) <> " is not found in the weak type environment."

lookupTermTypeEnv :: Hint -> T.Text -> IO WeakTerm
lookupTermTypeEnv m name = do
  termTypeEnv <- readIORef termTypeEnvRef
  case Map.lookup name termTypeEnv of
    Nothing ->
      raiseCritical m $ name <> " is not found in the term type environment."
    Just t ->
      return t

lookupWeakTypeEnvMaybe :: Int -> IO (Maybe WeakTerm)
lookupWeakTypeEnvMaybe s = do
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
  | Just _ <- asPrimNumMaybe x =
    return $ m :< TermTau
  | Just op <- asPrimOp x =
    primOpToType m op
  | otherwise =
    raiseCritical m $
      "the constant `" <> x <> "` is not found in the type environment."

primOpToType :: Hint -> PrimOp -> IO Term
primOpToType m (PrimOp op domList cod) = do
  let domList' = map (primNumToType m) domList
  xs <- mapM (const (newIdentFromText "_")) domList'
  let xts = zipWith (\x t -> (m, x, t)) xs domList'
  if S.member op cmpOpSet
    then do
      let cod' = m :< TermEnum constBool
      return $ m :< TermPi xts cod'
    else do
      let cod' = primNumToType m cod
      return $ m :< TermPi xts cod'

-- ?M ~> ?M @ ctx
arrange :: Context -> WeakTerm -> IO WeakTerm
arrange ctx term =
  case term of
    _ :< WeakTermTau ->
      return term
    _ :< WeakTermVar {} ->
      return term
    _ :< WeakTermVarGlobal {} ->
      return term
    m :< WeakTermPi xts t -> do
      (xts', t') <- arrangeBinder ctx xts t
      return $ m :< WeakTermPi xts' t'
    m :< WeakTermPiIntro kind xts e -> do
      case kind of
        LamKindFix xt -> do
          (xt' : xts', e') <- arrangeBinder ctx (xt : xts) e
          return $ m :< WeakTermPiIntro (LamKindFix xt') xts' e'
        LamKindCons dataName consName consNumber dataType -> do
          dataType' <- arrange ctx dataType
          (xts', e') <- arrangeBinder ctx xts e
          return $ m :< WeakTermPiIntro (LamKindCons dataName consName consNumber dataType') xts' e'
        _ -> do
          (xts', e') <- arrangeBinder ctx xts e
          return $ m :< WeakTermPiIntro kind xts' e'
    m :< WeakTermPiElim e es -> do
      es' <- mapM (arrange ctx) es
      e' <- arrange ctx e
      return $ m :< WeakTermPiElim e' es'
    m :< WeakTermSigma xts -> do
      (xts', _) <- arrangeBinder ctx xts (m :< WeakTermTau)
      return $ m :< WeakTermSigma xts'
    m :< WeakTermSigmaIntro es -> do
      es' <- mapM (arrange ctx) es
      return $ m :< WeakTermSigmaIntro es'
    m :< WeakTermSigmaElim xts e1 e2 -> do
      e1' <- arrange ctx e1
      (xts', e2') <- arrangeBinder ctx xts e2
      return $ m :< WeakTermSigmaElim xts' e1' e2'
    m :< WeakTermLet mxt e1 e2 -> do
      e1' <- arrange ctx e1
      ([mxt'], e2') <- arrangeBinder ctx [mxt] e2
      return $ m :< WeakTermLet mxt' e1' e2'
    _ :< WeakTermConst _ ->
      return term
    m :< WeakTermAster _ ->
      newTypeAsterInCtx ctx m
    m :< WeakTermInt t x -> do
      t' <- arrange ctx t
      return $ m :< WeakTermInt t' x
    m :< WeakTermFloat t x -> do
      t' <- arrange ctx t
      return $ m :< WeakTermFloat t' x
    _ :< WeakTermEnum _ ->
      return term
    _ :< WeakTermEnumIntro _ ->
      return term
    m :< WeakTermEnumElim (e, t) caseList -> do
      e' <- arrange ctx e
      t' <- arrange ctx t
      caseList' <-
        forM caseList $ \(enumCase, body) -> do
          body' <- arrange ctx body
          return (enumCase, body')
      return $ m :< WeakTermEnumElim (e', t') caseList'
    m :< WeakTermQuestion e t -> do
      e' <- arrange ctx e
      t' <- arrange ctx t
      return $ m :< WeakTermQuestion e' t'
    m :< WeakTermMagic der -> do
      der' <- traverse (arrange ctx) der
      return $ m :< WeakTermMagic der'
    m :< WeakTermMatch mSubject (e, t) clauseList -> do
      mSubject' <- mapM (arrange ctx) mSubject
      e' <- arrange ctx e
      t' <- arrange ctx t
      clauseList' <- forM clauseList $ \((mCons, constructorName, xts), body) -> do
        (xts', body') <- arrangeBinder ctx xts body
        return ((mCons, constructorName, xts'), body')
      return $ m :< WeakTermMatch mSubject' (e', t') clauseList'
    m :< WeakTermNoema s e -> do
      s' <- arrange ctx s
      e' <- arrange ctx e
      return $ m :< WeakTermNoema s' e'
    m :< WeakTermNoemaIntro x e -> do
      e' <- arrange ctx e
      return $ m :< WeakTermNoemaIntro x e'
    m :< WeakTermNoemaElim s e -> do
      e' <- arrange ctx e
      return $ m :< WeakTermNoemaElim s e'
    m :< WeakTermArray elemType -> do
      elemType' <- arrange ctx elemType
      return $ m :< WeakTermArray elemType'
    m :< WeakTermArrayIntro elemType elems -> do
      elemType' <- arrange ctx elemType
      elems' <- mapM (arrange ctx) elems
      return $ m :< WeakTermArrayIntro elemType' elems'
    m :< WeakTermArrayAccess subject elemType array index -> do
      subject' <- arrange ctx subject
      elemType' <- arrange ctx elemType
      array' <- arrange ctx array
      index' <- arrange ctx index
      return $ m :< WeakTermArrayAccess subject' elemType' array' index'
    _ :< WeakTermText ->
      return term
    _ :< WeakTermTextIntro _ ->
      return term
    m :< WeakTermCell contentType -> do
      contentType' <- arrange ctx contentType
      return $ m :< WeakTermCell contentType'
    m :< WeakTermCellIntro contentType content -> do
      contentType' <- arrange ctx contentType
      content' <- arrange ctx content
      return $ m :< WeakTermCellIntro contentType' content'
    m :< WeakTermCellRead cell -> do
      cell' <- arrange ctx cell
      return $ m :< WeakTermCellRead cell'
    m :< WeakTermCellWrite cell newValue -> do
      cell' <- arrange ctx cell
      newValue' <- arrange ctx newValue
      return $ m :< WeakTermCellWrite cell' newValue'

arrangeBinder ::
  Context ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  IO ([BinderF WeakTerm], WeakTerm)
arrangeBinder ctx binder cod =
  case binder of
    [] -> do
      cod' <- arrange ctx cod
      return ([], cod')
    ((mx, x, t) : xts) -> do
      t' <- arrange ctx t
      (xts', cod') <- arrangeBinder (ctx ++ [(mx, x, t')]) xts cod
      return ((mx, x, t') : xts', cod')

patternToTerm :: PatternF WeakTerm -> WeakTerm
patternToTerm (m, name, args) = do
  let args' = map (\(mx, x, _) -> mx :< WeakTermVar x) args
  m :< WeakTermPiElim (m :< WeakTermVarGlobal name) args'
