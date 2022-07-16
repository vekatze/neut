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
import qualified Data.HashMap.Strict as Map
import Data.IORef
import qualified Data.IntMap as IntMap
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.Binder
import qualified Entity.DefiniteDescription as DD
import Entity.EnumCase
import Entity.EnumInfo
import Entity.Global
import Entity.Hint
import qualified Entity.HoleID as HID
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import Entity.LamKind
import Entity.Magic
import Entity.Pattern
import qualified Entity.Prim as Prim
import Entity.PrimOp
import Entity.PrimOp.OpSet
import Entity.Term
import qualified Entity.Term.FromPrimNum as Term
import Entity.Term.Weaken
import Entity.WeakTerm
import Entity.WeakTerm.Subst

type BoundVarEnv = [BinderF WeakTerm]

infer :: Context -> WeakTerm -> IO (WeakTerm, WeakTerm)
infer ctx =
  infer' ctx []

inferType :: Context -> WeakTerm -> IO WeakTerm
inferType ctx =
  inferType' ctx []

infer' :: Context -> BoundVarEnv -> WeakTerm -> IO (WeakTerm, WeakTerm)
infer' ctx varEnv term =
  case term of
    _ :< WeakTermTau ->
      return (term, term)
    m :< WeakTermVar x -> do
      _ :< t <- lookupWeakTypeEnv ctx m x
      return (term, m :< t)
    m :< WeakTermVarGlobal name -> do
      _ :< t <- lookupTermTypeEnv ctx m name
      return (term, m :< t)
    m :< WeakTermPi xts t -> do
      (xts', t') <- inferPi ctx varEnv xts t
      return (m :< WeakTermPi xts' t', m :< WeakTermTau)
    m :< WeakTermPiIntro kind xts e -> do
      case kind of
        LamKindFix (mx, x, t) -> do
          t' <- inferType' ctx varEnv t
          insWeakTypeEnv x t'
          (xts', (e', tCod)) <- inferBinder ctx varEnv xts e
          let piType = m :< WeakTermPi xts' tCod
          insConstraintEnv piType t'
          return (m :< WeakTermPiIntro (LamKindFix (mx, x, t')) xts' e', piType)
        LamKindCons dataName consName discriminant dataType -> do
          dataType' <- inferType' ctx varEnv dataType
          (xts', (e', _)) <- inferBinder ctx varEnv xts e
          return (m :< WeakTermPiIntro (LamKindCons dataName consName discriminant dataType') xts' e', dataType')
        _ -> do
          (xts', (e', t')) <- inferBinder ctx varEnv xts e
          return (m :< WeakTermPiIntro kind xts' e', m :< WeakTermPi xts' t')
    m :< WeakTermPiElim e@(_ :< WeakTermVarGlobal name) es -> do
      etls <- mapM (infer' ctx varEnv) es
      t <- lookupTermTypeEnv ctx m name
      impArgEnv <- readIORef impArgEnvRef
      case Map.lookup name impArgEnv of
        Nothing -> do
          inferPiElim ctx varEnv m (e, t) etls
        Just i -> do
          holes <- forM [1 .. i] $ const $ newTypedAster (gensym ctx) varEnv m
          inferPiElim ctx varEnv m (e, t) $ holes ++ etls
    m :< WeakTermPiElim e es -> do
      etls <- mapM (infer' ctx varEnv) es
      etl <- infer' ctx varEnv e
      inferPiElim ctx varEnv m etl etls
    m :< WeakTermSigma xts -> do
      (xts', _) <- inferPi ctx varEnv xts (m :< WeakTermTau)
      return (m :< WeakTermSigma xts', m :< WeakTermTau)
    m :< WeakTermSigmaIntro es -> do
      ets <- mapM (infer' ctx varEnv) es
      ys <- mapM (const $ Gensym.newIdentFromText (gensym ctx) "arg") es
      yts <- newTypeAsterList (gensym ctx) varEnv $ zip ys (map metaOf es)
      _ <- inferArgs ctx IntMap.empty m ets yts (m :< WeakTermTau)
      return (m :< WeakTermSigmaIntro (map fst ets), m :< WeakTermSigma yts)
    m :< WeakTermSigmaElim xts e1 e2 -> do
      (e1', t1') <- infer' ctx varEnv e1
      (xts', (e2', t)) <- inferBinder ctx varEnv xts e2
      insConstraintEnv (m :< WeakTermSigma xts') t1'
      return (m :< WeakTermSigmaElim xts' e1' e2', t)
    m :< WeakTermLet (mx, x, t) e1 e2 -> do
      (e1', t1') <- infer' ctx varEnv e1
      t' <- inferType' ctx varEnv t
      insConstraintEnv t' t1'
      insWeakTypeEnv x t'
      (e2', t2') <- infer' ctx varEnv e2 -- no context extension
      return (m :< WeakTermLet (mx, x, t') e1' e2', t2')
    m :< WeakTermAster x -> do
      holeEnv <- readIORef holeEnvRef
      case IntMap.lookup (HID.reify x) holeEnv of
        Just asterInfo ->
          return asterInfo
        Nothing -> do
          typedAster <- newTypedAster (gensym ctx) varEnv m
          modifyIORef' holeEnvRef $ \env -> IntMap.insert (HID.reify x) typedAster env
          return typedAster
    m :< WeakTermPrim prim
      | Prim.Type _ <- prim ->
        return (term, m :< WeakTermTau)
      | Prim.Op op <- prim -> do
        primOpType <- primOpToType (gensym ctx) m op
        return (term, weaken primOpType)
    m :< WeakTermInt t i -> do
      t' <- inferType' ctx [] t -- varEnv == [] since t' should be i64, i8, etc. (i.e. t must be closed)
      return (m :< WeakTermInt t' i, t')
    m :< WeakTermFloat t f -> do
      t' <- inferType' ctx [] t -- t must be closed
      return (m :< WeakTermFloat t' f, t')
    m :< WeakTermEnum _ ->
      return (term, m :< WeakTermTau)
    m :< WeakTermEnumIntro (EnumLabel k _ _) -> do
      return (term, m :< WeakTermEnum k)
    m :< WeakTermEnumElim (e, _) ces -> do
      (e', t') <- infer' ctx varEnv e
      let (cs, es) = unzip ces
      (cs', tcs) <- unzip <$> mapM (inferEnumCase ctx varEnv) cs
      forM_ (zip tcs (repeat t')) $ uncurry insConstraintEnv
      (es', ts) <- unzip <$> mapM (infer' ctx varEnv) es
      h <- newAster (gensym ctx) varEnv m
      forM_ (zip (repeat h) ts) $ uncurry insConstraintEnv
      return (m :< WeakTermEnumElim (e', t') (zip cs' es'), h)
    m :< WeakTermQuestion e _ -> do
      (e', te) <- infer' ctx varEnv e
      return (m :< WeakTermQuestion e' te, te)
    m :< WeakTermMagic der -> do
      case der of
        MagicCast from to value -> do
          from' <- inferType' ctx varEnv from
          to' <- inferType' ctx varEnv to
          (value', t) <- infer' ctx varEnv value
          insConstraintEnv t from'
          return (m :< WeakTermMagic (MagicCast from' to' value'), to')
        _ -> do
          der' <- mapM (infer' ctx varEnv >=> return . fst) der
          resultType <- newAster (gensym ctx) varEnv m
          return (m :< WeakTermMagic der', resultType)
    m :< WeakTermMatch mSubject (e, _) clauseList -> do
      resultType <- newAster (gensym ctx) varEnv m
      (e', t') <- infer' ctx varEnv e
      mSubject' <- mapM (inferSubject ctx m varEnv) mSubject
      clauseList' <- forM clauseList $ \(pat@(mPat, name, xts), body) -> do
        (xts', (body', tBody)) <- inferBinder ctx varEnv xts body
        insConstraintEnv resultType tBody
        (_, tPat) <- infer' ctx varEnv $ patternToTerm pat
        insConstraintEnv tPat t'
        return ((mPat, name, xts'), body')
      return (m :< WeakTermMatch mSubject' (e', t') clauseList', resultType)
    m :< WeakTermNoema s t -> do
      s' <- inferType' ctx varEnv s
      t' <- inferType' ctx varEnv t
      return (m :< WeakTermNoema s' t', m :< WeakTermTau)
    m :< WeakTermNoemaIntro s e -> do
      (e', t') <- infer' ctx varEnv e
      return (m :< WeakTermNoemaIntro s e', m :< WeakTermNoema (m :< WeakTermVar s) t')
    m :< WeakTermNoemaElim s e -> do
      insWeakTypeEnv s (m :< WeakTermTau)
      (e', t) <- infer' ctx (varEnv ++ [(m, s, m :< WeakTermTau)]) e
      return (m :< WeakTermNoemaElim s e', t)
    m :< WeakTermArray elemType -> do
      elemType' <- inferType' ctx varEnv elemType
      return (m :< WeakTermArray elemType', m :< WeakTermTau)
    m :< WeakTermArrayIntro _ elems -> do
      elemType <- newAster (gensym ctx) varEnv m
      (elems', ts') <- unzip <$> mapM (infer' ctx varEnv) elems
      forM_ ts' $ insConstraintEnv elemType
      return (m :< WeakTermArrayIntro elemType elems', m :< WeakTermArray elemType)
    m :< WeakTermArrayAccess _ _ array index -> do
      subject <- newAster (gensym ctx) varEnv m
      elemType <- newAster (gensym ctx) varEnv m
      (array', tArray) <- infer' ctx varEnv array
      (index', tIndex) <- infer' ctx varEnv index
      insConstraintEnv (i64 m) tIndex
      let noeticArrayType = m :< WeakTermNoema subject (m :< WeakTermArray elemType)
      insConstraintEnv noeticArrayType tArray
      return (m :< WeakTermArrayAccess subject elemType array' index', elemType)
    m :< WeakTermText ->
      return (term, m :< WeakTermTau)
    m :< WeakTermTextIntro _ -> do
      return (term, m :< WeakTermText)
    m :< WeakTermCell contentType -> do
      contentType' <- inferType' ctx varEnv contentType
      return (m :< WeakTermCell contentType', m :< WeakTermTau)
    m :< WeakTermCellIntro _ content -> do
      (content', contentType) <- infer' ctx varEnv content
      return (m :< WeakTermCellIntro contentType content', m :< WeakTermCell contentType)
    m :< WeakTermCellRead cell -> do
      (cell', cellType) <- infer' ctx varEnv cell
      contentType <- newAster (gensym ctx) varEnv m
      subject <- newAster (gensym ctx) varEnv m
      insConstraintEnv (m :< WeakTermNoema subject (m :< WeakTermCell contentType)) cellType
      return (m :< WeakTermCellRead cell', contentType)
    m :< WeakTermCellWrite cell newValue -> do
      (cell', cellType) <- infer' ctx varEnv cell
      (newValue', newValueType) <- infer' ctx varEnv newValue
      subject <- newAster (gensym ctx) varEnv m
      insConstraintEnv (m :< WeakTermNoema subject (m :< WeakTermCell newValueType)) cellType
      return (m :< WeakTermCellWrite cell' newValue', m :< WeakTermEnum constTop)
    m :< WeakTermResourceType {} ->
      return (term, m :< WeakTermTau)

inferSubject :: Context -> Hint -> BoundVarEnv -> WeakTerm -> IO WeakTerm
inferSubject ctx m varEnv subject = do
  (subject', tSub) <- infer' ctx varEnv subject
  insConstraintEnv (m :< WeakTermTau) tSub
  return subject'

inferArgs ::
  Context ->
  SubstWeakTerm ->
  Hint ->
  [(WeakTerm, WeakTerm)] ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  IO WeakTerm
inferArgs ctx sub m args1 args2 cod =
  case (args1, args2) of
    ([], []) ->
      subst (gensym ctx) sub cod
    ((e, t) : ets, (_, x, tx) : xts) -> do
      tx' <- subst (gensym ctx) sub tx
      insConstraintEnv tx' t
      inferArgs ctx (IntMap.insert (Ident.toInt x) e sub) m ets xts cod
    _ ->
      Throw.raiseCritical (throw ctx) m "invalid argument passed to inferArgs"

inferType' :: Context -> BoundVarEnv -> WeakTerm -> IO WeakTerm
inferType' ctx varEnv t = do
  (t', u) <- infer' ctx varEnv t
  insConstraintEnv (metaOf t :< WeakTermTau) u
  return t'

inferPi ::
  Context ->
  BoundVarEnv ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  IO ([BinderF WeakTerm], WeakTerm)
inferPi ctx varEnv binder cod =
  case binder of
    [] -> do
      (cod' :< mlPiCod) <- inferType' ctx varEnv cod
      return ([], cod' :< mlPiCod)
    ((mx, x, t) : xts) -> do
      t' <- inferType' ctx varEnv t
      insWeakTypeEnv x t'
      (xtls', tlCod) <- inferPi ctx (varEnv ++ [(mx, x, t')]) xts cod
      return ((mx, x, t') : xtls', tlCod)

inferBinder ::
  Context ->
  BoundVarEnv ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  IO ([BinderF WeakTerm], (WeakTerm, WeakTerm))
inferBinder ctx varEnv binder e =
  case binder of
    [] -> do
      etl' <- infer' ctx varEnv e
      return ([], etl')
    ((mx, x, t) : xts) -> do
      t' <- inferType' ctx varEnv t
      insWeakTypeEnv x t'
      (xts', etl') <- inferBinder ctx (varEnv ++ [(mx, x, t')]) xts e
      return ((mx, x, t') : xts', etl')

inferPiElim ::
  Context ->
  BoundVarEnv ->
  Hint ->
  (WeakTerm, WeakTerm) ->
  [(WeakTerm, WeakTerm)] ->
  IO (WeakTerm, WeakTerm)
inferPiElim ctx varEnv m (e, t) ets = do
  let es = map fst ets
  case t of
    (_ :< WeakTermPi xts (_ :< cod))
      | length xts == length ets -> do
        cod' <- inferArgs ctx IntMap.empty m ets xts (m :< cod)
        return (m :< WeakTermPiElim e es, cod')
      | otherwise -> do
        raiseArityMismatchError ctx e (length xts) (length ets)
    _ -> do
      ys <- mapM (const $ Gensym.newIdentFromText (gensym ctx) "arg") es
      yts <- newTypeAsterList (gensym ctx) varEnv $ zip ys (map metaOf es)
      cod <- newAster (gensym ctx) (varEnv ++ yts) m
      insConstraintEnv (metaOf e :< WeakTermPi yts cod) t
      cod' <- inferArgs ctx IntMap.empty m ets yts cod
      return (m :< WeakTermPiElim e es, cod')

raiseArityMismatchError :: Context -> WeakTerm -> Int -> Int -> IO a
raiseArityMismatchError ctx function expected actual = do
  impArgEnv <- readIORef impArgEnvRef
  case function of
    m :< WeakTermVarGlobal name
      | Just k <- Map.lookup name impArgEnv ->
        Throw.raiseError (throw ctx) m $
          "the function `"
            <> DD.reify name
            <> "` expects "
            <> T.pack (show (expected - k))
            <> " arguments, but found "
            <> T.pack (show (actual - k))
            <> "."
    m :< _ ->
      Throw.raiseError (throw ctx) m $
        "this function expects "
          <> T.pack (show expected)
          <> " arguments, but found "
          <> T.pack (show actual)
          <> "."

newAster :: Gensym.Context -> BoundVarEnv -> Hint -> IO WeakTerm
newAster ctx varEnv m = do
  let varSeq = map (\(mx, x, _) -> mx :< WeakTermVar x) varEnv
  aster <- Gensym.newAster ctx m
  return (m :< WeakTermPiElim aster varSeq)

newTypedAster :: Gensym.Context -> BoundVarEnv -> Hint -> IO (WeakTerm, WeakTerm)
newTypedAster ctx varEnv m = do
  app <- newAster ctx varEnv m
  higherApp <- newAster ctx varEnv m
  return (app, higherApp)

-- In context varEnv == [x1, ..., xn], `newTypeAsterList varEnv [y1, ..., ym]` generates
-- the following list:
--
--   [(y1,   ?M1   @ (x1, ..., xn)),
--    (y2,   ?M2   @ (x1, ..., xn, y1),
--    ...,
--    (y{m}, ?M{m} @ (x1, ..., xn, y1, ..., y{m-1}))]
--
-- inserting type information `yi : ?Mi @ (x1, ..., xn, y1, ..., y{i-1})
newTypeAsterList :: Gensym.Context -> BoundVarEnv -> [(Ident, Hint)] -> IO [BinderF WeakTerm]
newTypeAsterList ctx varEnv ids =
  case ids of
    [] ->
      return []
    ((x, m) : rest) -> do
      t <- newAster ctx varEnv m
      insWeakTypeEnv x t
      ts <- newTypeAsterList ctx (varEnv ++ [(m, x, t)]) rest
      return $ (m, x, t) : ts

inferEnumCase :: Context -> BoundVarEnv -> EnumCase -> IO (EnumCase, WeakTerm)
inferEnumCase ctx varEnv weakCase =
  case weakCase of
    m :< EnumCaseLabel (EnumLabel k _ _) -> do
      return (weakCase, m :< WeakTermEnum k)
    m :< EnumCaseDefault -> do
      h <- newAster (gensym ctx) varEnv m
      return (m :< EnumCaseDefault, h)
    m :< EnumCaseInt _ ->
      Throw.raiseCritical (throw ctx) m "enum-case-int shouldn't be used in the target language"

insConstraintEnv :: WeakTerm -> WeakTerm -> IO ()
insConstraintEnv t1 t2 =
  modifyIORef' constraintListRef $ (:) (t1, t2)

insWeakTypeEnv :: Ident -> WeakTerm -> IO ()
insWeakTypeEnv (I (_, i)) t =
  modifyIORef' weakTypeEnvRef $ IntMap.insert i t

lookupWeakTypeEnv :: Context -> Hint -> Ident -> IO WeakTerm
lookupWeakTypeEnv ctx m s = do
  mt <- lookupWeakTypeEnvMaybe $ Ident.toInt s
  case mt of
    Just t ->
      return t
    Nothing ->
      Throw.raiseCritical (throw ctx) m $
        Ident.toText' s <> " is not found in the weak type environment."

lookupTermTypeEnv :: Context -> Hint -> DD.DefiniteDescription -> IO WeakTerm
lookupTermTypeEnv ctx m name = do
  termTypeEnv <- readIORef termTypeEnvRef
  case Map.lookup name termTypeEnv of
    Nothing ->
      Throw.raiseCritical (throw ctx) m $ "`" <> DD.reify name <> "` is not found in the term type environment."
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

primOpToType :: Gensym.Context -> Hint -> PrimOp -> IO Term
primOpToType ctx m (PrimOp op domList cod) = do
  let domList' = map (Term.fromPrimNum m) domList
  xs <- mapM (const (Gensym.newIdentFromText ctx "_")) domList'
  let xts = zipWith (\x t -> (m, x, t)) xs domList'
  if S.member op cmpOpSet
    then do
      let cod' = m :< TermEnum constBool
      return $ m :< TermPi xts cod'
    else do
      let cod' = Term.fromPrimNum m cod
      return $ m :< TermPi xts cod'

-- ?M ~> ?M @ varEnv
arrange :: Gensym.Context -> BoundVarEnv -> WeakTerm -> IO WeakTerm
arrange ctx varEnv term =
  case term of
    _ :< WeakTermTau ->
      return term
    _ :< WeakTermVar {} ->
      return term
    _ :< WeakTermVarGlobal {} ->
      return term
    m :< WeakTermPi xts t -> do
      (xts', t') <- arrangeBinder ctx varEnv xts t
      return $ m :< WeakTermPi xts' t'
    m :< WeakTermPiIntro kind xts e -> do
      case kind of
        LamKindFix xt -> do
          (xt' : xts', e') <- arrangeBinder ctx varEnv (xt : xts) e
          return $ m :< WeakTermPiIntro (LamKindFix xt') xts' e'
        LamKindCons dataName consName consNumber dataType -> do
          dataType' <- arrange ctx varEnv dataType
          (xts', e') <- arrangeBinder ctx varEnv xts e
          return $ m :< WeakTermPiIntro (LamKindCons dataName consName consNumber dataType') xts' e'
        _ -> do
          (xts', e') <- arrangeBinder ctx varEnv xts e
          return $ m :< WeakTermPiIntro kind xts' e'
    m :< WeakTermPiElim e es -> do
      es' <- mapM (arrange ctx varEnv) es
      e' <- arrange ctx varEnv e
      return $ m :< WeakTermPiElim e' es'
    m :< WeakTermSigma xts -> do
      (xts', _) <- arrangeBinder ctx varEnv xts (m :< WeakTermTau)
      return $ m :< WeakTermSigma xts'
    m :< WeakTermSigmaIntro es -> do
      es' <- mapM (arrange ctx varEnv) es
      return $ m :< WeakTermSigmaIntro es'
    m :< WeakTermSigmaElim xts e1 e2 -> do
      e1' <- arrange ctx varEnv e1
      (xts', e2') <- arrangeBinder ctx varEnv xts e2
      return $ m :< WeakTermSigmaElim xts' e1' e2'
    m :< WeakTermLet mxt e1 e2 -> do
      e1' <- arrange ctx varEnv e1
      ([mxt'], e2') <- arrangeBinder ctx varEnv [mxt] e2
      return $ m :< WeakTermLet mxt' e1' e2'
    _ :< WeakTermPrim _ ->
      return term
    m :< WeakTermAster _ ->
      newAster ctx varEnv m
    m :< WeakTermInt t x -> do
      t' <- arrange ctx varEnv t
      return $ m :< WeakTermInt t' x
    m :< WeakTermFloat t x -> do
      t' <- arrange ctx varEnv t
      return $ m :< WeakTermFloat t' x
    _ :< WeakTermEnum _ ->
      return term
    _ :< WeakTermEnumIntro {} ->
      return term
    m :< WeakTermEnumElim (e, t) caseList -> do
      e' <- arrange ctx varEnv e
      t' <- arrange ctx varEnv t
      caseList' <-
        forM caseList $ \(enumCase, body) -> do
          body' <- arrange ctx varEnv body
          return (enumCase, body')
      return $ m :< WeakTermEnumElim (e', t') caseList'
    m :< WeakTermQuestion e t -> do
      e' <- arrange ctx varEnv e
      t' <- arrange ctx varEnv t
      return $ m :< WeakTermQuestion e' t'
    m :< WeakTermMagic der -> do
      der' <- traverse (arrange ctx varEnv) der
      return $ m :< WeakTermMagic der'
    m :< WeakTermMatch mSubject (e, t) clauseList -> do
      mSubject' <- mapM (arrange ctx varEnv) mSubject
      e' <- arrange ctx varEnv e
      t' <- arrange ctx varEnv t
      clauseList' <- forM clauseList $ \((mCons, constructorName, xts), body) -> do
        (xts', body') <- arrangeBinder ctx varEnv xts body
        return ((mCons, constructorName, xts'), body')
      return $ m :< WeakTermMatch mSubject' (e', t') clauseList'
    m :< WeakTermNoema s e -> do
      s' <- arrange ctx varEnv s
      e' <- arrange ctx varEnv e
      return $ m :< WeakTermNoema s' e'
    m :< WeakTermNoemaIntro x e -> do
      e' <- arrange ctx varEnv e
      return $ m :< WeakTermNoemaIntro x e'
    m :< WeakTermNoemaElim s e -> do
      e' <- arrange ctx varEnv e
      return $ m :< WeakTermNoemaElim s e'
    m :< WeakTermArray elemType -> do
      elemType' <- arrange ctx varEnv elemType
      return $ m :< WeakTermArray elemType'
    m :< WeakTermArrayIntro elemType elems -> do
      elemType' <- arrange ctx varEnv elemType
      elems' <- mapM (arrange ctx varEnv) elems
      return $ m :< WeakTermArrayIntro elemType' elems'
    m :< WeakTermArrayAccess subject elemType array index -> do
      subject' <- arrange ctx varEnv subject
      elemType' <- arrange ctx varEnv elemType
      array' <- arrange ctx varEnv array
      index' <- arrange ctx varEnv index
      return $ m :< WeakTermArrayAccess subject' elemType' array' index'
    _ :< WeakTermText ->
      return term
    _ :< WeakTermTextIntro _ ->
      return term
    m :< WeakTermCell contentType -> do
      contentType' <- arrange ctx varEnv contentType
      return $ m :< WeakTermCell contentType'
    m :< WeakTermCellIntro contentType content -> do
      contentType' <- arrange ctx varEnv contentType
      content' <- arrange ctx varEnv content
      return $ m :< WeakTermCellIntro contentType' content'
    m :< WeakTermCellRead cell -> do
      cell' <- arrange ctx varEnv cell
      return $ m :< WeakTermCellRead cell'
    m :< WeakTermCellWrite cell newValue -> do
      cell' <- arrange ctx varEnv cell
      newValue' <- arrange ctx varEnv newValue
      return $ m :< WeakTermCellWrite cell' newValue'
    _ :< WeakTermResourceType {} ->
      return term

arrangeBinder ::
  Gensym.Context ->
  BoundVarEnv ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  IO ([BinderF WeakTerm], WeakTerm)
arrangeBinder ctx varEnv binder cod =
  case binder of
    [] -> do
      cod' <- arrange ctx varEnv cod
      return ([], cod')
    ((mx, x, t) : xts) -> do
      t' <- arrange ctx varEnv t
      (xts', cod') <- arrangeBinder ctx (varEnv ++ [(mx, x, t')]) xts cod
      return ((mx, x, t') : xts', cod')

patternToTerm :: PatternF WeakTerm -> WeakTerm
patternToTerm (m, name, args) = do
  let args' = map (\(mx, x, _) -> mx :< WeakTermVar x) args
  m :< WeakTermPiElim (m :< WeakTermVarGlobal name) args'
