module Scene.Elaborate.Infer
  ( Context (..),
    specialize,
    infer,
    inferType,
    insConstraintEnv,
    insWeakTypeEnv,
    inferBinder,
  )
where

import qualified Context.App as App
import qualified Context.Gensym as Gensym
import qualified Context.Implicit as Implicit
import qualified Context.Throw as Throw
import qualified Context.Type as Type
import Control.Comonad.Cofree
import Control.Monad
import Data.IORef
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.Binder
import Entity.Constraint
import qualified Entity.DefiniteDescription as DD
import Entity.EnumCase
import Entity.EnumInfo
import Entity.Hint
import qualified Entity.HoleID as HID
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import qualified Entity.ImpArgNum as I
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

data Context = Context
  { base :: App.Context,
    weakTypeEnvRef :: IORef (IntMap.IntMap WeakTerm),
    holeEnvRef :: IORef (IntMap.IntMap (WeakTerm, WeakTerm)),
    constraintListRef :: IORef [Constraint]
  }

specialize :: App.Context -> IO Context
specialize ctx = do
  _weakTypeEnvRef <- newIORef IntMap.empty
  _holeEnvRef <- newIORef IntMap.empty
  _constraintListRef <- newIORef []
  return $
    Context
      { base = ctx,
        weakTypeEnvRef = _weakTypeEnvRef,
        holeEnvRef = _holeEnvRef,
        constraintListRef = _constraintListRef
      }

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
          insWeakTypeEnv ctx x t'
          (xts', (e', tCod)) <- inferBinder ctx varEnv xts e
          let piType = m :< WeakTermPi xts' tCod
          insConstraintEnv ctx piType t'
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
      mImpArgNum <- Implicit.lookup (App.implicit (base ctx)) name
      case mImpArgNum of
        Nothing -> do
          inferPiElim ctx varEnv m (e, t) etls
        Just i -> do
          holes <- forM [1 .. I.reify i] $ const $ newTypedAster (App.gensym (base ctx)) varEnv m
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
      ys <- mapM (const $ Gensym.newIdentFromText (App.gensym (base ctx)) "arg") es
      yts <- newTypeAsterList ctx varEnv $ zip ys (map metaOf es)
      _ <- inferArgs ctx IntMap.empty m ets yts (m :< WeakTermTau)
      return (m :< WeakTermSigmaIntro (map fst ets), m :< WeakTermSigma yts)
    m :< WeakTermSigmaElim xts e1 e2 -> do
      (e1', t1') <- infer' ctx varEnv e1
      (xts', (e2', t)) <- inferBinder ctx varEnv xts e2
      insConstraintEnv ctx (m :< WeakTermSigma xts') t1'
      return (m :< WeakTermSigmaElim xts' e1' e2', t)
    m :< WeakTermLet (mx, x, t) e1 e2 -> do
      (e1', t1') <- infer' ctx varEnv e1
      t' <- inferType' ctx varEnv t
      insConstraintEnv ctx t' t1'
      insWeakTypeEnv ctx x t'
      (e2', t2') <- infer' ctx varEnv e2 -- no context extension
      return (m :< WeakTermLet (mx, x, t') e1' e2', t2')
    m :< WeakTermAster x es -> do
      holeEnv <- readIORef $ holeEnvRef ctx
      case IntMap.lookup (HID.reify x) holeEnv of
        Just asterInfo ->
          return asterInfo
        Nothing -> do
          holeType <- Gensym.newAster (App.gensym (base ctx)) m es
          modifyIORef' (holeEnvRef ctx) $ \env -> IntMap.insert (HID.reify x) (term, holeType) env
          return (term, holeType)
    m :< WeakTermPrim prim
      | Prim.Type _ <- prim ->
        return (term, m :< WeakTermTau)
      | Prim.Op op <- prim -> do
        primOpType <- primOpToType (App.gensym (base ctx)) m op
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
      forM_ (zip tcs (repeat t')) $ uncurry (insConstraintEnv ctx)
      (es', ts) <- unzip <$> mapM (infer' ctx varEnv) es
      h <- newAster (App.gensym (base ctx)) varEnv m
      forM_ (zip (repeat h) ts) $ uncurry (insConstraintEnv ctx)
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
          insConstraintEnv ctx t from'
          return (m :< WeakTermMagic (MagicCast from' to' value'), to')
        _ -> do
          der' <- mapM (infer' ctx varEnv >=> return . fst) der
          resultType <- newAster (App.gensym (base ctx)) varEnv m
          return (m :< WeakTermMagic der', resultType)
    m :< WeakTermMatch mSubject (e, _) clauseList -> do
      resultType <- newAster (App.gensym (base ctx)) varEnv m
      (e', t') <- infer' ctx varEnv e
      mSubject' <- mapM (inferSubject ctx m varEnv) mSubject
      clauseList' <- forM clauseList $ \(pat@(mPat, name, xts), body) -> do
        (xts', (body', tBody)) <- inferBinder ctx varEnv xts body
        insConstraintEnv ctx resultType tBody
        (_, tPat) <- infer' ctx varEnv $ patternToTerm pat
        insConstraintEnv ctx tPat t'
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
      insWeakTypeEnv ctx s (m :< WeakTermTau)
      (e', t) <- infer' ctx ((m, s, m :< WeakTermTau) : varEnv) e
      return (m :< WeakTermNoemaElim s e', t)
    m :< WeakTermArray elemType -> do
      elemType' <- inferType' ctx varEnv elemType
      return (m :< WeakTermArray elemType', m :< WeakTermTau)
    m :< WeakTermArrayIntro _ elems -> do
      elemType <- newAster (App.gensym (base ctx)) varEnv m
      (elems', ts') <- unzip <$> mapM (infer' ctx varEnv) elems
      forM_ ts' $ insConstraintEnv ctx elemType
      return (m :< WeakTermArrayIntro elemType elems', m :< WeakTermArray elemType)
    m :< WeakTermArrayAccess _ _ array index -> do
      subject <- newAster (App.gensym (base ctx)) varEnv m
      elemType <- newAster (App.gensym (base ctx)) varEnv m
      (array', tArray) <- infer' ctx varEnv array
      (index', tIndex) <- infer' ctx varEnv index
      insConstraintEnv ctx (i64 m) tIndex
      let noeticArrayType = m :< WeakTermNoema subject (m :< WeakTermArray elemType)
      insConstraintEnv ctx noeticArrayType tArray
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
      contentType <- newAster (App.gensym (base ctx)) varEnv m
      subject <- newAster (App.gensym (base ctx)) varEnv m
      insConstraintEnv ctx (m :< WeakTermNoema subject (m :< WeakTermCell contentType)) cellType
      return (m :< WeakTermCellRead cell', contentType)
    m :< WeakTermCellWrite cell newValue -> do
      (cell', cellType) <- infer' ctx varEnv cell
      (newValue', newValueType) <- infer' ctx varEnv newValue
      subject <- newAster (App.gensym (base ctx)) varEnv m
      insConstraintEnv ctx (m :< WeakTermNoema subject (m :< WeakTermCell newValueType)) cellType
      return (m :< WeakTermCellWrite cell' newValue', m :< WeakTermEnum constTop)
    m :< WeakTermResourceType {} ->
      return (term, m :< WeakTermTau)

inferSubject :: Context -> Hint -> BoundVarEnv -> WeakTerm -> IO WeakTerm
inferSubject ctx m varEnv subject = do
  (subject', tSub) <- infer' ctx varEnv subject
  insConstraintEnv ctx (m :< WeakTermTau) tSub
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
      subst (App.gensym (base ctx)) sub cod
    ((e, t) : ets, (_, x, tx) : xts) -> do
      tx' <- subst (App.gensym (base ctx)) sub tx
      insConstraintEnv ctx tx' t
      inferArgs ctx (IntMap.insert (Ident.toInt x) e sub) m ets xts cod
    _ ->
      Throw.raiseCritical (App.throw (base ctx)) m "invalid argument passed to inferArgs"

inferType' :: Context -> BoundVarEnv -> WeakTerm -> IO WeakTerm
inferType' ctx varEnv t = do
  (t', u) <- infer' ctx varEnv t
  insConstraintEnv ctx (metaOf t :< WeakTermTau) u
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
      insWeakTypeEnv ctx x t'
      (xtls', tlCod) <- inferPi ctx ((mx, x, t') : varEnv) xts cod
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
      insWeakTypeEnv ctx x t'
      (xts', etl') <- inferBinder ctx ((mx, x, t') : varEnv) xts e
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
      ys <- mapM (const $ Gensym.newIdentFromText (App.gensym (base ctx)) "arg") es
      yts <- newTypeAsterList ctx varEnv $ zip ys (map metaOf es)
      cod <- newAster (App.gensym (base ctx)) (yts ++ varEnv) m
      insConstraintEnv ctx (metaOf e :< WeakTermPi yts cod) t
      cod' <- inferArgs ctx IntMap.empty m ets yts cod
      return (m :< WeakTermPiElim e es, cod')

raiseArityMismatchError :: Context -> WeakTerm -> Int -> Int -> IO a
raiseArityMismatchError ctx function expected actual = do
  case function of
    m :< WeakTermVarGlobal name -> do
      mImpArgNum <- Implicit.lookup (App.implicit (base ctx)) name
      let k = I.reify $ fromMaybe I.zero mImpArgNum
      Throw.raiseError (App.throw (base ctx)) m $
        "the function `"
          <> DD.reify name
          <> "` expects "
          <> T.pack (show (expected - k))
          <> " arguments, but found "
          <> T.pack (show (actual - k))
          <> "."
    m :< _ ->
      Throw.raiseError (App.throw (base ctx)) m $
        "this function expects "
          <> T.pack (show expected)
          <> " arguments, but found "
          <> T.pack (show actual)
          <> "."

newAster :: Gensym.Context -> BoundVarEnv -> Hint -> IO WeakTerm
newAster ctx varEnv m =
  Gensym.newAster ctx m $ map (\(mx, x, _) -> mx :< WeakTermVar x) varEnv

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
newTypeAsterList :: Context -> BoundVarEnv -> [(Ident, Hint)] -> IO [BinderF WeakTerm]
newTypeAsterList ctx varEnv ids =
  case ids of
    [] ->
      return []
    ((x, m) : rest) -> do
      t <- newAster (App.gensym (base ctx)) varEnv m
      insWeakTypeEnv ctx x t
      ts <- newTypeAsterList ctx ((m, x, t) : varEnv) rest
      return $ (m, x, t) : ts

inferEnumCase :: Context -> BoundVarEnv -> EnumCase -> IO (EnumCase, WeakTerm)
inferEnumCase ctx varEnv weakCase =
  case weakCase of
    m :< EnumCaseLabel (EnumLabel k _ _) -> do
      return (weakCase, m :< WeakTermEnum k)
    m :< EnumCaseDefault -> do
      h <- newAster (App.gensym (base ctx)) varEnv m
      return (m :< EnumCaseDefault, h)
    m :< EnumCaseInt _ ->
      Throw.raiseCritical (App.throw (base ctx)) m "enum-case-int shouldn't be used in the target language"

insConstraintEnv :: Context -> WeakTerm -> WeakTerm -> IO ()
insConstraintEnv ctx t1 t2 =
  modifyIORef' (constraintListRef ctx) $ (:) (t1, t2)

insWeakTypeEnv :: Context -> Ident -> WeakTerm -> IO ()
insWeakTypeEnv ctx (I (_, i)) t =
  modifyIORef' (weakTypeEnvRef ctx) $ IntMap.insert i t

lookupWeakTypeEnv :: Context -> Hint -> Ident -> IO WeakTerm
lookupWeakTypeEnv ctx m s = do
  mt <- lookupWeakTypeEnvMaybe ctx $ Ident.toInt s
  case mt of
    Just t ->
      return t
    Nothing ->
      Throw.raiseCritical (App.throw (base ctx)) m $
        Ident.toText' s <> " is not found in the weak type environment."

lookupTermTypeEnv :: Context -> Hint -> DD.DefiniteDescription -> IO WeakTerm
lookupTermTypeEnv ctx m name = do
  Type.lookup (App.asTypeCtx (base ctx)) m name

lookupWeakTypeEnvMaybe :: Context -> Int -> IO (Maybe WeakTerm)
lookupWeakTypeEnvMaybe ctx s = do
  weakTypeEnv <- readIORef $ weakTypeEnvRef ctx
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

patternToTerm :: PatternF WeakTerm -> WeakTerm
patternToTerm (m, name, args) = do
  let args' = map (\(mx, x, _) -> mx :< WeakTermVar x) args
  m :< WeakTermPiElim (m :< WeakTermVarGlobal name) args'
