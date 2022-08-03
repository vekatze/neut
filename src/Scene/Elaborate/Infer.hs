module Scene.Elaborate.Infer
  ( Context (..),
    infer,
    inferType,
    inferBinder,
    inferDefineResource,
  )
where

import Control.Comonad.Cofree
import Control.Monad
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.Binder
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
import Entity.Stmt
import Entity.Term
import qualified Entity.Term.FromPrimNum as Term
import Entity.Term.Weaken
import Entity.WeakTerm
import qualified Entity.WeakTerm.Subst as Subst

type BoundVarEnv = [BinderF WeakTerm]

data Context = Context
  { substCtx :: Subst.Context,
    insWeakTypeEnv :: Ident -> WeakTerm -> IO (),
    lookupWeakTypeEnvMaybe :: Int -> IO (Maybe WeakTerm),
    lookupTermTypeEnv :: Hint -> DD.DefiniteDescription -> IO WeakTerm,
    newHoleID :: IO HID.HoleID,
    newIdentFromText :: T.Text -> IO Ident,
    lookupHoleEnv :: Int -> IO (Maybe (WeakTerm, WeakTerm)),
    insHoleEnv :: Int -> WeakTerm -> WeakTerm -> IO (),
    lookupImplictArgNum :: DD.DefiniteDescription -> IO (Maybe I.ImpArgNum),
    insConstraintEnv :: WeakTerm -> WeakTerm -> IO (),
    raiseError :: forall a. Hint -> T.Text -> IO a,
    raiseCritical :: forall a. Hint -> T.Text -> IO a
  }

infer :: Context -> WeakTerm -> IO (WeakTerm, WeakTerm)
infer ctx =
  infer' ctx []

inferType :: Context -> WeakTerm -> IO WeakTerm
inferType ctx =
  inferType' ctx []

inferDefineResource :: Context -> Hint -> DD.DefiniteDescription -> WeakTerm -> WeakTerm -> IO WeakStmt
inferDefineResource ctx m name discarder copier = do
  (discarder', td) <- infer ctx discarder
  (copier', tc) <- infer ctx copier
  x <- newIdentFromText ctx "_"
  let botTop = m :< WeakTermPi [(m, x, m :< WeakTermEnum constBottom)] (m :< WeakTermEnum constTop)
  let botBot = m :< WeakTermPi [(m, x, m :< WeakTermEnum constBottom)] (m :< WeakTermEnum constBottom)
  insConstraintEnv ctx botTop td
  insConstraintEnv ctx botBot tc
  return $ WeakStmtDefineResource m name discarder' copier'

infer' :: Context -> BoundVarEnv -> WeakTerm -> IO (WeakTerm, WeakTerm)
infer' ctx varEnv term =
  case term of
    _ :< WeakTermTau ->
      return (term, term)
    m :< WeakTermVar x -> do
      _ :< t <- lookupWeakTypeEnv ctx m x
      return (term, m :< t)
    m :< WeakTermVarGlobal name _ -> do
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
    m :< WeakTermPiElim e@(_ :< WeakTermVarGlobal name _) es -> do
      etls <- mapM (infer' ctx varEnv) es
      t <- lookupTermTypeEnv ctx m name
      mImpArgNum <- lookupImplictArgNum ctx name
      case mImpArgNum of
        Nothing -> do
          inferPiElim ctx varEnv m (e, t) etls
        Just i -> do
          holes <- forM [1 .. I.reify i] $ const $ newTypedAster ctx varEnv m
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
      ys <- mapM (const $ newIdentFromText ctx "arg") es
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
      let rawHoleID = HID.reify x
      mAsterInfo <- lookupHoleEnv ctx rawHoleID
      case mAsterInfo of
        Just asterInfo ->
          return asterInfo
        Nothing -> do
          holeType <- newAster' ctx m es
          insHoleEnv ctx rawHoleID term holeType
          return (term, holeType)
    m :< WeakTermPrim prim
      | Prim.Type _ <- prim ->
        return (term, m :< WeakTermTau)
      | Prim.Op op <- prim -> do
        primOpType <- primOpToType ctx m op
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
      h <- newAster ctx m varEnv
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
          resultType <- newAster ctx m varEnv
          return (m :< WeakTermMagic der', resultType)
    m :< WeakTermMatch mSubject (e, _) clauseList -> do
      resultType <- newAster ctx m varEnv
      (e', t') <- infer' ctx varEnv e
      mSubject' <- mapM (inferSubject ctx m varEnv) mSubject
      clauseList' <- forM clauseList $ \(pat@(mPat, name, arity, xts), body) -> do
        (xts', (body', tBody)) <- inferBinder ctx varEnv xts body
        insConstraintEnv ctx resultType tBody
        (_, tPat) <- infer' ctx varEnv $ patternToTerm pat
        insConstraintEnv ctx tPat t'
        return ((mPat, name, arity, xts'), body')
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
      elemType <- newAster ctx m varEnv
      (elems', ts') <- unzip <$> mapM (infer' ctx varEnv) elems
      forM_ ts' $ insConstraintEnv ctx elemType
      return (m :< WeakTermArrayIntro elemType elems', m :< WeakTermArray elemType)
    m :< WeakTermArrayAccess _ _ array index -> do
      subject <- newAster ctx m varEnv
      elemType <- newAster ctx m varEnv
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
      contentType <- newAster ctx m varEnv
      subject <- newAster ctx m varEnv
      insConstraintEnv ctx (m :< WeakTermNoema subject (m :< WeakTermCell contentType)) cellType
      return (m :< WeakTermCellRead cell', contentType)
    m :< WeakTermCellWrite cell newValue -> do
      (cell', cellType) <- infer' ctx varEnv cell
      (newValue', newValueType) <- infer' ctx varEnv newValue
      subject <- newAster ctx m varEnv
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
      Subst.subst (substCtx ctx) sub cod
    ((e, t) : ets, (_, x, tx) : xts) -> do
      tx' <- Subst.subst (substCtx ctx) sub tx
      insConstraintEnv ctx tx' t
      inferArgs ctx (IntMap.insert (Ident.toInt x) e sub) m ets xts cod
    _ ->
      raiseCritical ctx m "invalid argument passed to inferArgs"

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
      ys <- mapM (const $ newIdentFromText ctx "arg") es
      yts <- newTypeAsterList ctx varEnv $ zip ys (map metaOf es)
      cod <- newAster ctx m (yts ++ varEnv)
      insConstraintEnv ctx (metaOf e :< WeakTermPi yts cod) t
      cod' <- inferArgs ctx IntMap.empty m ets yts cod
      return (m :< WeakTermPiElim e es, cod')

raiseArityMismatchError :: Context -> WeakTerm -> Int -> Int -> IO a
raiseArityMismatchError ctx function expected actual = do
  case function of
    m :< WeakTermVarGlobal name _ -> do
      mImpArgNum <- lookupImplictArgNum ctx name
      let k = I.reify $ fromMaybe I.zero mImpArgNum
      raiseError ctx m $
        "the function `"
          <> DD.reify name
          <> "` expects "
          <> T.pack (show (expected - k))
          <> " arguments, but found "
          <> T.pack (show (actual - k))
          <> "."
    m :< _ ->
      raiseError ctx m $
        "this function expects "
          <> T.pack (show expected)
          <> " arguments, but found "
          <> T.pack (show actual)
          <> "."

-- newAster :: Gensym.Context -> BoundVarEnv -> Hint -> IO WeakTerm
-- newAster ctx varEnv m =
--   Gensym.newAster ctx m $ map (\(mx, x, _) -> mx :< WeakTermVar x) varEnv

newAster :: Context -> Hint -> BoundVarEnv -> IO WeakTerm
newAster ctx m varEnv = do
  newAster' ctx m $ map (\(mx, x, _) -> mx :< WeakTermVar x) varEnv

newAster' :: Context -> Hint -> [WeakTerm] -> IO WeakTerm
newAster' ctx m es = do
  i <- newHoleID ctx
  return $ m :< WeakTermAster i es

newTypedAster :: Context -> BoundVarEnv -> Hint -> IO (WeakTerm, WeakTerm)
newTypedAster ctx varEnv m = do
  app <- newAster ctx m varEnv
  higherApp <- newAster ctx m varEnv
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
      t <- newAster ctx m varEnv
      insWeakTypeEnv ctx x t
      ts <- newTypeAsterList ctx ((m, x, t) : varEnv) rest
      return $ (m, x, t) : ts

inferEnumCase :: Context -> BoundVarEnv -> EnumCase -> IO (EnumCase, WeakTerm)
inferEnumCase ctx varEnv weakCase =
  case weakCase of
    m :< EnumCaseLabel (EnumLabel k _ _) -> do
      return (weakCase, m :< WeakTermEnum k)
    m :< EnumCaseDefault -> do
      h <- newAster ctx m varEnv
      return (m :< EnumCaseDefault, h)
    m :< EnumCaseInt _ ->
      raiseCritical ctx m "enum-case-int shouldn't be used in the target language"

lookupWeakTypeEnv :: Context -> Hint -> Ident -> IO WeakTerm
lookupWeakTypeEnv ctx m s = do
  mt <- lookupWeakTypeEnvMaybe ctx $ Ident.toInt s
  case mt of
    Just t ->
      return t
    Nothing ->
      raiseCritical ctx m $
        Ident.toText' s <> " is not found in the weak type environment."

primOpToType :: Context -> Hint -> PrimOp -> IO Term
primOpToType ctx m (PrimOp op domList cod) = do
  let domList' = map (Term.fromPrimNum m) domList
  xs <- mapM (const (newIdentFromText ctx "_")) domList'
  let xts = zipWith (\x t -> (m, x, t)) xs domList'
  if S.member op cmpOpSet
    then do
      let cod' = m :< TermEnum constBool
      return $ m :< TermPi xts cod'
    else do
      let cod' = Term.fromPrimNum m cod
      return $ m :< TermPi xts cod'

patternToTerm :: PatternF WeakTerm -> WeakTerm
patternToTerm (m, name, arity, args) = do
  let args' = map (\(mx, x, _) -> mx :< WeakTermVar x) args
  m :< WeakTermPiElim (m :< WeakTermVarGlobal name arity) args'
