module Scene.Elaborate.Infer
  ( infer,
    inferType,
    insConstraintEnv,
    insWeakTypeEnv,
    inferBinder,
    arrangeBinder,
  )
where

import Context.App
import qualified Context.Gensym as Gensym
import qualified Context.Throw as Throw
import Control.Comonad.Cofree
import Control.Monad
import Data.Function
import qualified Data.HashMap.Lazy as Map
import Data.IORef
import qualified Data.IntMap as IntMap
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.Binder
import Entity.EnumCase
import Entity.Global
import Entity.Hint
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import Entity.LamKind
import Entity.Magic
import Entity.Pattern
import qualified Entity.PrimNum.FromText as PrimNum
import Entity.PrimOp
import qualified Entity.PrimOp.FromText as PrimOp
import Entity.PrimOp.OpSet
import Entity.Term
import qualified Entity.Term.FromPrimNum as Term
import Entity.Term.Weaken
import Entity.WeakTerm
import Entity.WeakTerm.Subst

type Context = [BinderF WeakTerm]

infer :: Axis -> WeakTerm -> IO (WeakTerm, WeakTerm)
infer axis =
  infer' axis []

inferType :: Axis -> WeakTerm -> IO WeakTerm
inferType axis =
  inferType' axis []

infer' :: Axis -> Context -> WeakTerm -> IO (WeakTerm, WeakTerm)
infer' axis ctx term =
  case term of
    _ :< WeakTermTau ->
      return (term, term)
    m :< WeakTermVar x -> do
      _ :< t <- lookupWeakTypeEnv axis m x
      return (term, m :< t)
    m :< WeakTermVarGlobal name -> do
      _ :< t <- lookupTermTypeEnv axis m name
      return (term, m :< t)
    m :< WeakTermPi xts t -> do
      (xts', t') <- inferPi axis ctx xts t
      return (m :< WeakTermPi xts' t', m :< WeakTermTau)
    m :< WeakTermPiIntro kind xts e -> do
      case kind of
        LamKindFix (mx, x, t) -> do
          t' <- inferType' axis ctx t
          insWeakTypeEnv x t'
          (xts', (e', tCod)) <- inferBinder axis ctx xts e
          let piType = m :< WeakTermPi xts' tCod
          insConstraintEnv piType t'
          return (m :< WeakTermPiIntro (LamKindFix (mx, x, t')) xts' e', piType)
        LamKindCons dataName consName consNumber dataType -> do
          dataType' <- inferType' axis ctx dataType
          (xts', (e', _)) <- inferBinder axis ctx xts e
          return (m :< WeakTermPiIntro (LamKindCons dataName consName consNumber dataType') xts' e', dataType')
        _ -> do
          (xts', (e', t')) <- inferBinder axis ctx xts e
          return (m :< WeakTermPiIntro kind xts' e', m :< WeakTermPi xts' t')
    m :< WeakTermPiElim e@(_ :< WeakTermVarGlobal name) es -> do
      etls <- mapM (infer' axis ctx) es
      t <- lookupTermTypeEnv axis m name
      impArgEnv <- readIORef impArgEnvRef
      case Map.lookup name impArgEnv of
        Nothing -> do
          inferPiElim axis ctx m (e, t) etls
        Just i -> do
          holes <- forM [1 .. i] $ const $ newAsterInCtx (axis & gensym) ctx m
          inferPiElim axis ctx m (e, t) $ holes ++ etls
    m :< WeakTermPiElim hole@(_ :< WeakTermAster i) es -> do
      etls <- mapM (infer' axis ctx) es
      t <- lookupWeakTypeEnv' axis m i
      inferPiElim axis ctx m (hole, t) etls
    m :< WeakTermPiElim e es -> do
      etls <- mapM (infer' axis ctx) es
      etl <- infer' axis ctx e
      inferPiElim axis ctx m etl etls
    m :< WeakTermSigma xts -> do
      (xts', _) <- inferPi axis ctx xts (m :< WeakTermTau)
      return (m :< WeakTermSigma xts', m :< WeakTermTau)
    m :< WeakTermSigmaIntro es -> do
      ets <- mapM (infer' axis ctx) es
      ys <- mapM (const $ Gensym.newIdentFromText (gensym axis) "arg") es
      yts <- newTypeAsterListInCtx (axis & gensym) ctx $ zip ys (map metaOf es)
      _ <- inferArgs axis IntMap.empty m ets yts (m :< WeakTermTau)
      return (m :< WeakTermSigmaIntro (map fst ets), m :< WeakTermSigma yts)
    m :< WeakTermSigmaElim xts e1 e2 -> do
      (e1', t1') <- infer' axis ctx e1
      (xts', (e2', t)) <- inferBinder axis ctx xts e2
      insConstraintEnv (m :< WeakTermSigma xts') t1'
      return (m :< WeakTermSigmaElim xts' e1' e2', t)
    m :< WeakTermLet (mx, x, t) e1 e2 -> do
      (e1', t1') <- infer' axis ctx e1
      t' <- inferType' axis ctx t
      insConstraintEnv t' t1'
      insWeakTypeEnv x t'
      (e2', t2') <- infer' axis ctx e2 -- no context extension
      return (m :< WeakTermLet (mx, x, t') e1' e2', t2')
    m :< WeakTermAster x -> do
      holeEnv <- readIORef holeEnvRef
      case IntMap.lookup x holeEnv of
        Just asterInfo ->
          return asterInfo
        Nothing -> do
          (app, higherApp) <- newAsterInCtx (axis & gensym) ctx m
          modifyIORef' holeEnvRef $ \env -> IntMap.insert x (app, higherApp) env
          return (app, higherApp)
    m :< WeakTermConst x
      -- i64, f16, etc.
      | Just _ <- PrimNum.fromText x ->
        return (term, m :< WeakTermTau)
      | Just op <- PrimOp.fromText x ->
        inferExternal m x (primOpToType (axis & gensym) m op)
      | otherwise -> do
        _ :< t <- weaken <$> lookupConstTypeEnv axis m x
        return (term, m :< t)
    m :< WeakTermInt t i -> do
      t' <- inferType' axis [] t -- ctx == [] since t' should be i64, i8, etc. (i.e. t must be closed)
      return (m :< WeakTermInt t' i, t')
    m :< WeakTermFloat t f -> do
      t' <- inferType' axis [] t -- t must be closed
      return (m :< WeakTermFloat t' f, t')
    m :< WeakTermEnum _ ->
      return (term, m :< WeakTermTau)
    m :< WeakTermEnumIntro l -> do
      k <- lookupKind axis m l
      return (term, m :< WeakTermEnum k)
    m :< WeakTermEnumElim (e, _) ces -> do
      (e', t') <- infer' axis ctx e
      let (cs, es) = unzip ces
      (cs', tcs) <- unzip <$> mapM (inferEnumCase axis ctx) cs
      forM_ (zip tcs (repeat t')) $ uncurry insConstraintEnv
      (es', ts) <- unzip <$> mapM (infer' axis ctx) es
      h <- newTypeAsterInCtx (axis & gensym) ctx m
      forM_ (zip (repeat h) ts) $ uncurry insConstraintEnv
      return (m :< WeakTermEnumElim (e', t') (zip cs' es'), h)
    m :< WeakTermQuestion e _ -> do
      (e', te) <- infer' axis ctx e
      return (m :< WeakTermQuestion e' te, te)
    m :< WeakTermMagic der -> do
      case der of
        MagicCast from to value -> do
          from' <- inferType' axis ctx from
          to' <- inferType' axis ctx to
          (value', t) <- infer' axis ctx value
          insConstraintEnv t from'
          return (m :< WeakTermMagic (MagicCast from' to' value'), to')
        _ -> do
          der' <- mapM (infer' axis ctx >=> return . fst) der
          resultType <- newTypeAsterInCtx (axis & gensym) ctx m
          return (m :< WeakTermMagic der', resultType)
    m :< WeakTermMatch mSubject (e, _) clauseList -> do
      resultType <- newTypeAsterInCtx (axis & gensym) ctx m
      (e', t') <- infer' axis ctx e
      mSubject' <- mapM (inferSubject axis m ctx) mSubject
      clauseList' <- forM clauseList $ \(pat@(mPat, name, xts), body) -> do
        (xts', (body', tBody)) <- inferBinder axis ctx xts body
        insConstraintEnv resultType tBody
        (_, tPat) <- infer' axis ctx $ patternToTerm pat
        insConstraintEnv tPat t'
        return ((mPat, name, xts'), body')
      return (m :< WeakTermMatch mSubject' (e', t') clauseList', resultType)
    m :< WeakTermNoema s t -> do
      s' <- inferType' axis ctx s
      t' <- inferType' axis ctx t
      return (m :< WeakTermNoema s' t', m :< WeakTermTau)
    m :< WeakTermNoemaIntro s e -> do
      (e', t') <- infer' axis ctx e
      return (m :< WeakTermNoemaIntro s e', m :< WeakTermNoema (m :< WeakTermVar s) t')
    m :< WeakTermNoemaElim s e -> do
      insWeakTypeEnv s (m :< WeakTermTau)
      (e', t) <- infer' axis (ctx ++ [(m, s, m :< WeakTermTau)]) e
      return (m :< WeakTermNoemaElim s e', t)
    m :< WeakTermArray elemType -> do
      elemType' <- inferType' axis ctx elemType
      return (m :< WeakTermArray elemType', m :< WeakTermTau)
    m :< WeakTermArrayIntro _ elems -> do
      elemType <- newTypeAsterInCtx (axis & gensym) ctx m
      (elems', ts') <- unzip <$> mapM (infer' axis ctx) elems
      forM_ ts' $ insConstraintEnv elemType
      return (m :< WeakTermArrayIntro elemType elems', m :< WeakTermArray elemType)
    m :< WeakTermArrayAccess _ _ array index -> do
      subject <- newTypeAsterInCtx (axis & gensym) ctx m
      elemType <- newTypeAsterInCtx (axis & gensym) ctx m
      (array', tArray) <- infer' axis ctx array
      (index', tIndex) <- infer' axis ctx index
      insConstraintEnv (m :< WeakTermConst "i64") tIndex
      let noeticArrayType = m :< WeakTermNoema subject (m :< WeakTermArray elemType)
      insConstraintEnv noeticArrayType tArray
      return (m :< WeakTermArrayAccess subject elemType array' index', elemType)
    m :< WeakTermText ->
      return (term, m :< WeakTermTau)
    m :< WeakTermTextIntro _ -> do
      return (term, m :< WeakTermText)
    m :< WeakTermCell contentType -> do
      contentType' <- inferType' axis ctx contentType
      return (m :< WeakTermCell contentType', m :< WeakTermTau)
    m :< WeakTermCellIntro _ content -> do
      (content', contentType) <- infer' axis ctx content
      return (m :< WeakTermCellIntro contentType content', m :< WeakTermCell contentType)
    m :< WeakTermCellRead cell -> do
      (cell', cellType) <- infer' axis ctx cell
      contentType <- newTypeAsterInCtx (axis & gensym) ctx m
      subject <- newTypeAsterInCtx (axis & gensym) ctx m
      insConstraintEnv (m :< WeakTermNoema subject (m :< WeakTermCell contentType)) cellType
      return (m :< WeakTermCellRead cell', contentType)
    m :< WeakTermCellWrite cell newValue -> do
      (cell', cellType) <- infer' axis ctx cell
      (newValue', newValueType) <- infer' axis ctx newValue
      subject <- newTypeAsterInCtx (axis & gensym) ctx m
      insConstraintEnv (m :< WeakTermNoema subject (m :< WeakTermCell newValueType)) cellType
      return (m :< WeakTermCellWrite cell' newValue', m :< WeakTermEnum constTop)

inferSubject :: Axis -> Hint -> Context -> WeakTerm -> IO WeakTerm
inferSubject axis m ctx subject = do
  (subject', tSub) <- infer' axis ctx subject
  insConstraintEnv (m :< WeakTermTau) tSub
  return subject'

inferArgs ::
  Axis ->
  SubstWeakTerm ->
  Hint ->
  [(WeakTerm, WeakTerm)] ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  IO WeakTerm
inferArgs axis sub m args1 args2 cod =
  case (args1, args2) of
    ([], []) ->
      subst (axis & gensym) sub cod
    ((e, t) : ets, (_, x, tx) : xts) -> do
      tx' <- subst (axis & gensym) sub tx
      insConstraintEnv tx' t
      inferArgs axis (IntMap.insert (Ident.toInt x) e sub) m ets xts cod
    _ ->
      (axis & throw & Throw.raiseCritical) m "invalid argument passed to inferArgs"

inferExternal :: Hint -> T.Text -> IO Term -> IO (WeakTerm, WeakTerm)
inferExternal m x comp = do
  _ :< t <- weaken <$> comp
  return (m :< WeakTermConst x, m :< t)

inferType' :: Axis -> Context -> WeakTerm -> IO WeakTerm
inferType' axis ctx t = do
  (t', u) <- infer' axis ctx t
  insConstraintEnv (metaOf t :< WeakTermTau) u
  return t'

inferPi ::
  Axis ->
  Context ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  IO ([BinderF WeakTerm], WeakTerm)
inferPi axis ctx binder cod =
  case binder of
    [] -> do
      (cod' :< mlPiCod) <- inferType' axis ctx cod
      return ([], cod' :< mlPiCod)
    ((mx, x, t) : xts) -> do
      t' <- inferType' axis ctx t
      insWeakTypeEnv x t'
      (xtls', tlCod) <- inferPi axis (ctx ++ [(mx, x, t')]) xts cod
      return ((mx, x, t') : xtls', tlCod)

inferBinder ::
  Axis ->
  Context ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  IO ([BinderF WeakTerm], (WeakTerm, WeakTerm))
inferBinder axis ctx binder e =
  case binder of
    [] -> do
      etl' <- infer' axis ctx e
      return ([], etl')
    ((mx, x, t) : xts) -> do
      t' <- inferType' axis ctx t
      insWeakTypeEnv x t'
      (xts', etl') <- inferBinder axis (ctx ++ [(mx, x, t')]) xts e
      return ((mx, x, t') : xts', etl')

inferPiElim ::
  Axis ->
  Context ->
  Hint ->
  (WeakTerm, WeakTerm) ->
  [(WeakTerm, WeakTerm)] ->
  IO (WeakTerm, WeakTerm)
inferPiElim axis ctx m (e, t) ets = do
  let es = map fst ets
  case t of
    (_ :< WeakTermPi xts (_ :< cod))
      | length xts == length ets -> do
        cod' <- inferArgs axis IntMap.empty m ets xts (m :< cod)
        return (m :< WeakTermPiElim e es, cod')
      | otherwise -> do
        raiseArityMismatchError axis e (length xts) (length ets)
    _ -> do
      ys <- mapM (const $ Gensym.newIdentFromText (gensym axis) "arg") es
      yts <- newTypeAsterListInCtx (axis & gensym) ctx $ zip ys (map metaOf es)
      cod <- newTypeAsterInCtx (axis & gensym) (ctx ++ yts) m
      insConstraintEnv (metaOf e :< WeakTermPi yts cod) t
      cod' <- inferArgs axis IntMap.empty m ets yts cod
      return (m :< WeakTermPiElim e es, cod')

raiseArityMismatchError :: Axis -> WeakTerm -> Int -> Int -> IO a
raiseArityMismatchError axis function expected actual = do
  impArgEnv <- readIORef impArgEnvRef
  case function of
    m :< WeakTermVarGlobal name
      | Just k <- Map.lookup name impArgEnv ->
        (axis & throw & Throw.raiseCritical) m $
          "the function `"
            <> name
            <> "` expects "
            <> T.pack (show (expected - k))
            <> " arguments, but found "
            <> T.pack (show (actual - k))
            <> "."
    m :< _ ->
      (axis & throw & Throw.raiseCritical) m $
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
newAsterInCtx :: Gensym.Axis -> Context -> Hint -> IO (WeakTerm, WeakTerm)
newAsterInCtx axis ctx m = do
  higherAster <- Gensym.newAster axis m
  let varSeq = map (\(mx, x, _) -> mx :< WeakTermVar x) ctx
  let higherApp = m :< WeakTermPiElim higherAster varSeq
  aster@(_ :< WeakTermAster i) <- Gensym.newAster axis m
  modifyIORef' weakTypeEnvRef $ IntMap.insert i $ m :< WeakTermPi ctx higherApp
  let app = m :< WeakTermPiElim aster varSeq
  return (app, higherApp)

-- In a context (x1 : A1, ..., xn : An), this function creates a metavariable
--   ?M  : Pi (x1 : A1, ..., xn : An). Univ{i}
-- and return ?M @ (x1, ..., xn) : Univ{i}.
newTypeAsterInCtx :: Gensym.Axis -> Context -> Hint -> IO WeakTerm
newTypeAsterInCtx axis ctx m = do
  let varSeq = map (\(mx, x, _) -> mx :< WeakTermVar x) ctx
  aster@(_ :< WeakTermAster i) <- Gensym.newAster axis m
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
newTypeAsterListInCtx :: Gensym.Axis -> Context -> [(Ident, Hint)] -> IO [BinderF WeakTerm]
newTypeAsterListInCtx axis ctx ids =
  case ids of
    [] ->
      return []
    ((x, m) : rest) -> do
      t <- newTypeAsterInCtx axis ctx m
      insWeakTypeEnv x t
      ts <- newTypeAsterListInCtx axis (ctx ++ [(m, x, t)]) rest
      return $ (m, x, t) : ts

inferEnumCase :: Axis -> Context -> EnumCase -> IO (EnumCase, WeakTerm)
inferEnumCase axis ctx weakCase =
  case weakCase of
    m :< EnumCaseLabel name -> do
      k <- lookupKind axis m name
      return (weakCase, m :< WeakTermEnum k)
    m :< EnumCaseDefault -> do
      h <- newTypeAsterInCtx (axis & gensym) ctx m
      return (m :< EnumCaseDefault, h)
    m :< EnumCaseInt _ ->
      (axis & throw & Throw.raiseCritical) m "enum-case-int shouldn't be used in the target language"

insConstraintEnv :: WeakTerm -> WeakTerm -> IO ()
insConstraintEnv t1 t2 =
  modifyIORef' constraintListRef $ (:) (t1, t2)

insWeakTypeEnv :: Ident -> WeakTerm -> IO ()
insWeakTypeEnv (I (_, i)) t =
  modifyIORef' weakTypeEnvRef $ IntMap.insert i t

lookupWeakTypeEnv :: Axis -> Hint -> Ident -> IO WeakTerm
lookupWeakTypeEnv axis m s = do
  mt <- lookupWeakTypeEnvMaybe $ Ident.toInt s
  case mt of
    Just t ->
      return t
    Nothing ->
      (axis & throw & Throw.raiseCritical) m $
        Ident.toText' s <> " is not found in the weak type environment."

lookupWeakTypeEnv' :: Axis -> Hint -> Int -> IO WeakTerm
lookupWeakTypeEnv' axis m s = do
  mt <- lookupWeakTypeEnvMaybe s
  case mt of
    Just t ->
      return t
    Nothing ->
      (axis & throw & Throw.raiseCritical) m $
        "the hole ?M" <> T.pack (show s) <> " is not found in the weak type environment."

lookupTermTypeEnv :: Axis -> Hint -> T.Text -> IO WeakTerm
lookupTermTypeEnv axis m name = do
  termTypeEnv <- readIORef termTypeEnvRef
  case Map.lookup name termTypeEnv of
    Nothing ->
      (axis & throw & Throw.raiseCritical) m $ name <> " is not found in the term type environment."
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

lookupKind :: Axis -> Hint -> T.Text -> IO T.Text
lookupKind axis m name = do
  revEnumEnv <- readIORef revEnumEnvRef
  case Map.lookup name revEnumEnv of
    Nothing ->
      (axis & throw & Throw.raiseError) m $ "no such enum-intro is defined: " <> name
    Just (j, _) ->
      return j

lookupConstTypeEnv :: Axis -> Hint -> T.Text -> IO Term
lookupConstTypeEnv axis m x
  | Just _ <- PrimNum.fromText x =
    return $ m :< TermTau
  | Just op <- PrimOp.fromText x =
    primOpToType (axis & gensym) m op
  | otherwise =
    (axis & throw & Throw.raiseCritical) m $
      "the constant `" <> x <> "` is not found in the type environment."

primOpToType :: Gensym.Axis -> Hint -> PrimOp -> IO Term
primOpToType axis m (PrimOp op domList cod) = do
  let domList' = map (Term.fromPrimNum m) domList
  xs <- mapM (const (Gensym.newIdentFromText axis "_")) domList'
  let xts = zipWith (\x t -> (m, x, t)) xs domList'
  if S.member op cmpOpSet
    then do
      let cod' = m :< TermEnum constBool
      return $ m :< TermPi xts cod'
    else do
      let cod' = Term.fromPrimNum m cod
      return $ m :< TermPi xts cod'

-- ?M ~> ?M @ ctx
arrange :: Gensym.Axis -> Context -> WeakTerm -> IO WeakTerm
arrange axis ctx term =
  case term of
    _ :< WeakTermTau ->
      return term
    _ :< WeakTermVar {} ->
      return term
    _ :< WeakTermVarGlobal {} ->
      return term
    m :< WeakTermPi xts t -> do
      (xts', t') <- arrangeBinder axis ctx xts t
      return $ m :< WeakTermPi xts' t'
    m :< WeakTermPiIntro kind xts e -> do
      case kind of
        LamKindFix xt -> do
          (xt' : xts', e') <- arrangeBinder axis ctx (xt : xts) e
          return $ m :< WeakTermPiIntro (LamKindFix xt') xts' e'
        LamKindCons dataName consName consNumber dataType -> do
          dataType' <- arrange axis ctx dataType
          (xts', e') <- arrangeBinder axis ctx xts e
          return $ m :< WeakTermPiIntro (LamKindCons dataName consName consNumber dataType') xts' e'
        _ -> do
          (xts', e') <- arrangeBinder axis ctx xts e
          return $ m :< WeakTermPiIntro kind xts' e'
    m :< WeakTermPiElim e es -> do
      es' <- mapM (arrange axis ctx) es
      e' <- arrange axis ctx e
      return $ m :< WeakTermPiElim e' es'
    m :< WeakTermSigma xts -> do
      (xts', _) <- arrangeBinder axis ctx xts (m :< WeakTermTau)
      return $ m :< WeakTermSigma xts'
    m :< WeakTermSigmaIntro es -> do
      es' <- mapM (arrange axis ctx) es
      return $ m :< WeakTermSigmaIntro es'
    m :< WeakTermSigmaElim xts e1 e2 -> do
      e1' <- arrange axis ctx e1
      (xts', e2') <- arrangeBinder axis ctx xts e2
      return $ m :< WeakTermSigmaElim xts' e1' e2'
    m :< WeakTermLet mxt e1 e2 -> do
      e1' <- arrange axis ctx e1
      ([mxt'], e2') <- arrangeBinder axis ctx [mxt] e2
      return $ m :< WeakTermLet mxt' e1' e2'
    _ :< WeakTermConst _ ->
      return term
    m :< WeakTermAster _ ->
      newTypeAsterInCtx axis ctx m
    m :< WeakTermInt t x -> do
      t' <- arrange axis ctx t
      return $ m :< WeakTermInt t' x
    m :< WeakTermFloat t x -> do
      t' <- arrange axis ctx t
      return $ m :< WeakTermFloat t' x
    _ :< WeakTermEnum _ ->
      return term
    _ :< WeakTermEnumIntro _ ->
      return term
    m :< WeakTermEnumElim (e, t) caseList -> do
      e' <- arrange axis ctx e
      t' <- arrange axis ctx t
      caseList' <-
        forM caseList $ \(enumCase, body) -> do
          body' <- arrange axis ctx body
          return (enumCase, body')
      return $ m :< WeakTermEnumElim (e', t') caseList'
    m :< WeakTermQuestion e t -> do
      e' <- arrange axis ctx e
      t' <- arrange axis ctx t
      return $ m :< WeakTermQuestion e' t'
    m :< WeakTermMagic der -> do
      der' <- traverse (arrange axis ctx) der
      return $ m :< WeakTermMagic der'
    m :< WeakTermMatch mSubject (e, t) clauseList -> do
      mSubject' <- mapM (arrange axis ctx) mSubject
      e' <- arrange axis ctx e
      t' <- arrange axis ctx t
      clauseList' <- forM clauseList $ \((mCons, constructorName, xts), body) -> do
        (xts', body') <- arrangeBinder axis ctx xts body
        return ((mCons, constructorName, xts'), body')
      return $ m :< WeakTermMatch mSubject' (e', t') clauseList'
    m :< WeakTermNoema s e -> do
      s' <- arrange axis ctx s
      e' <- arrange axis ctx e
      return $ m :< WeakTermNoema s' e'
    m :< WeakTermNoemaIntro x e -> do
      e' <- arrange axis ctx e
      return $ m :< WeakTermNoemaIntro x e'
    m :< WeakTermNoemaElim s e -> do
      e' <- arrange axis ctx e
      return $ m :< WeakTermNoemaElim s e'
    m :< WeakTermArray elemType -> do
      elemType' <- arrange axis ctx elemType
      return $ m :< WeakTermArray elemType'
    m :< WeakTermArrayIntro elemType elems -> do
      elemType' <- arrange axis ctx elemType
      elems' <- mapM (arrange axis ctx) elems
      return $ m :< WeakTermArrayIntro elemType' elems'
    m :< WeakTermArrayAccess subject elemType array index -> do
      subject' <- arrange axis ctx subject
      elemType' <- arrange axis ctx elemType
      array' <- arrange axis ctx array
      index' <- arrange axis ctx index
      return $ m :< WeakTermArrayAccess subject' elemType' array' index'
    _ :< WeakTermText ->
      return term
    _ :< WeakTermTextIntro _ ->
      return term
    m :< WeakTermCell contentType -> do
      contentType' <- arrange axis ctx contentType
      return $ m :< WeakTermCell contentType'
    m :< WeakTermCellIntro contentType content -> do
      contentType' <- arrange axis ctx contentType
      content' <- arrange axis ctx content
      return $ m :< WeakTermCellIntro contentType' content'
    m :< WeakTermCellRead cell -> do
      cell' <- arrange axis ctx cell
      return $ m :< WeakTermCellRead cell'
    m :< WeakTermCellWrite cell newValue -> do
      cell' <- arrange axis ctx cell
      newValue' <- arrange axis ctx newValue
      return $ m :< WeakTermCellWrite cell' newValue'

arrangeBinder ::
  Gensym.Axis ->
  Context ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  IO ([BinderF WeakTerm], WeakTerm)
arrangeBinder axis ctx binder cod =
  case binder of
    [] -> do
      cod' <- arrange axis ctx cod
      return ([], cod')
    ((mx, x, t) : xts) -> do
      t' <- arrange axis ctx t
      (xts', cod') <- arrangeBinder axis (ctx ++ [(mx, x, t')]) xts cod
      return ((mx, x, t') : xts', cod')

patternToTerm :: PatternF WeakTerm -> WeakTerm
patternToTerm (m, name, args) = do
  let args' = map (\(mx, x, _) -> mx :< WeakTermVar x) args
  m :< WeakTermPiElim (m :< WeakTermVarGlobal name) args'
