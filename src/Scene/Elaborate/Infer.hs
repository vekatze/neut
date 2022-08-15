module Scene.Elaborate.Infer
  ( Context (..),
    infer,
    inferType,
    inferBinder,
    inferDefineResource,
  )
where

import qualified Context.Env as Env
import qualified Context.Gensym as Gensym
import qualified Context.Implicit as Implicit
import qualified Context.Throw as Throw
import qualified Context.Type as Type
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

class
  ( Throw.Context m,
    Type.Context m,
    Subst.Context m,
    Gensym.Context m,
    Implicit.Context m,
    Env.Context m
  ) =>
  Context m
  where
  insWeakTypeEnv :: Ident -> WeakTerm -> m ()
  lookupWeakTypeEnv :: Hint -> Ident -> m WeakTerm
  lookupHoleEnv :: Int -> m (Maybe (WeakTerm, WeakTerm))
  insHoleEnv :: Int -> WeakTerm -> WeakTerm -> m ()

infer :: Context m => WeakTerm -> m (WeakTerm, WeakTerm)
infer =
  infer' []

inferType :: Context m => WeakTerm -> m WeakTerm
inferType =
  inferType' []

inferDefineResource :: Context m => Hint -> DD.DefiniteDescription -> WeakTerm -> WeakTerm -> m WeakStmt
inferDefineResource m name discarder copier = do
  (discarder', td) <- infer discarder
  (copier', tc) <- infer copier
  x <- Gensym.newIdentFromText "_"
  let botTop = m :< WeakTermPi [(m, x, m :< WeakTermEnum constBottom)] (m :< WeakTermEnum constTop)
  let botBot = m :< WeakTermPi [(m, x, m :< WeakTermEnum constBottom)] (m :< WeakTermEnum constBottom)
  Env.insConstraintEnv botTop td
  Env.insConstraintEnv botBot tc
  return $ WeakStmtDefineResource m name discarder' copier'

infer' :: Context m => BoundVarEnv -> WeakTerm -> m (WeakTerm, WeakTerm)
infer' varEnv term =
  case term of
    _ :< WeakTermTau ->
      return (term, term)
    m :< WeakTermVar x -> do
      _ :< t <- lookupWeakTypeEnv m x
      return (term, m :< t)
    m :< WeakTermVarGlobal name _ -> do
      _ :< t <- Type.lookup m name
      return (term, m :< t)
    m :< WeakTermPi xts t -> do
      (xts', t') <- inferPi varEnv xts t
      return (m :< WeakTermPi xts' t', m :< WeakTermTau)
    m :< WeakTermPiIntro kind xts e -> do
      case kind of
        LamKindFix (mx, x, t) -> do
          t' <- inferType' varEnv t
          insWeakTypeEnv x t'
          (xts', (e', tCod)) <- inferBinder varEnv xts e
          let piType = m :< WeakTermPi xts' tCod
          Env.insConstraintEnv piType t'
          return (m :< WeakTermPiIntro (LamKindFix (mx, x, t')) xts' e', piType)
        LamKindCons dataName consName discriminant dataType -> do
          dataType' <- inferType' varEnv dataType
          (xts', (e', _)) <- inferBinder varEnv xts e
          return (m :< WeakTermPiIntro (LamKindCons dataName consName discriminant dataType') xts' e', dataType')
        _ -> do
          (xts', (e', t')) <- inferBinder varEnv xts e
          return (m :< WeakTermPiIntro kind xts' e', m :< WeakTermPi xts' t')
    m :< WeakTermPiElim e@(_ :< WeakTermVarGlobal name _) es -> do
      etls <- mapM (infer' varEnv) es
      t <- Type.lookup m name
      -- t <- lookupTermTypeEnv m name
      mImpArgNum <- Implicit.lookup name
      case mImpArgNum of
        Nothing -> do
          inferPiElim varEnv m (e, t) etls
        Just i -> do
          holes <- forM [1 .. I.reify i] $ const $ newTypedAster varEnv m
          inferPiElim varEnv m (e, t) $ holes ++ etls
    m :< WeakTermPiElim e es -> do
      etls <- mapM (infer' varEnv) es
      etl <- infer' varEnv e
      inferPiElim varEnv m etl etls
    m :< WeakTermSigma xts -> do
      (xts', _) <- inferPi varEnv xts (m :< WeakTermTau)
      return (m :< WeakTermSigma xts', m :< WeakTermTau)
    m :< WeakTermSigmaIntro es -> do
      ets <- mapM (infer' varEnv) es
      ys <- mapM (const $ Gensym.newIdentFromText "arg") es
      yts <- newTypeAsterList varEnv $ zip ys (map metaOf es)
      _ <- inferArgs IntMap.empty m ets yts (m :< WeakTermTau)
      return (m :< WeakTermSigmaIntro (map fst ets), m :< WeakTermSigma yts)
    m :< WeakTermSigmaElim xts e1 e2 -> do
      (e1', t1') <- infer' varEnv e1
      (xts', (e2', t)) <- inferBinder varEnv xts e2
      Env.insConstraintEnv (m :< WeakTermSigma xts') t1'
      return (m :< WeakTermSigmaElim xts' e1' e2', t)
    m :< WeakTermLet (mx, x, t) e1 e2 -> do
      (e1', t1') <- infer' varEnv e1
      t' <- inferType' varEnv t
      Env.insConstraintEnv t' t1'
      insWeakTypeEnv x t'
      (e2', t2') <- infer' varEnv e2 -- no context extension
      return (m :< WeakTermLet (mx, x, t') e1' e2', t2')
    m :< WeakTermAster x es -> do
      let rawHoleID = HID.reify x
      mAsterInfo <- lookupHoleEnv rawHoleID
      case mAsterInfo of
        Just asterInfo ->
          return asterInfo
        Nothing -> do
          holeType <- newAster' m es
          insHoleEnv rawHoleID term holeType
          return (term, holeType)
    m :< WeakTermPrim prim
      | Prim.Type _ <- prim ->
        return (term, m :< WeakTermTau)
      | Prim.Op op <- prim -> do
        primOpType <- primOpToType m op
        return (term, weaken primOpType)
    m :< WeakTermInt t i -> do
      t' <- inferType' [] t -- varEnv == [] since t' should be i64, i8, etc. (i.e. t must be closed)
      return (m :< WeakTermInt t' i, t')
    m :< WeakTermFloat t f -> do
      t' <- inferType' [] t -- t must be closed
      return (m :< WeakTermFloat t' f, t')
    m :< WeakTermEnum _ ->
      return (term, m :< WeakTermTau)
    m :< WeakTermEnumIntro (EnumLabel k _ _) -> do
      return (term, m :< WeakTermEnum k)
    m :< WeakTermEnumElim (e, _) ces -> do
      (e', t') <- infer' varEnv e
      let (cs, es) = unzip ces
      (cs', tcs) <- unzip <$> mapM (inferEnumCase varEnv) cs
      forM_ (zip tcs (repeat t')) $ uncurry Env.insConstraintEnv
      (es', ts) <- unzip <$> mapM (infer' varEnv) es
      h <- newAster m varEnv
      forM_ (zip (repeat h) ts) $ uncurry Env.insConstraintEnv
      return (m :< WeakTermEnumElim (e', t') (zip cs' es'), h)
    m :< WeakTermQuestion e _ -> do
      (e', te) <- infer' varEnv e
      return (m :< WeakTermQuestion e' te, te)
    m :< WeakTermMagic der -> do
      case der of
        MagicCast from to value -> do
          from' <- inferType' varEnv from
          to' <- inferType' varEnv to
          (value', t) <- infer' varEnv value
          Env.insConstraintEnv t from'
          return (m :< WeakTermMagic (MagicCast from' to' value'), to')
        _ -> do
          der' <- mapM (infer' varEnv >=> return . fst) der
          resultType <- newAster m varEnv
          return (m :< WeakTermMagic der', resultType)
    m :< WeakTermMatch mSubject (e, _) clauseList -> do
      resultType <- newAster m varEnv
      (e', t') <- infer' varEnv e
      mSubject' <- mapM (inferSubject m varEnv) mSubject
      clauseList' <- forM clauseList $ \(pat@(mPat, name, arity, xts), body) -> do
        (xts', (body', tBody)) <- inferBinder varEnv xts body
        Env.insConstraintEnv resultType tBody
        (_, tPat) <- infer' varEnv $ patternToTerm pat
        Env.insConstraintEnv tPat t'
        return ((mPat, name, arity, xts'), body')
      return (m :< WeakTermMatch mSubject' (e', t') clauseList', resultType)
    m :< WeakTermNoema s t -> do
      s' <- inferType' varEnv s
      t' <- inferType' varEnv t
      return (m :< WeakTermNoema s' t', m :< WeakTermTau)
    m :< WeakTermNoemaIntro s e -> do
      (e', t') <- infer' varEnv e
      return (m :< WeakTermNoemaIntro s e', m :< WeakTermNoema (m :< WeakTermVar s) t')
    m :< WeakTermNoemaElim s e -> do
      insWeakTypeEnv s (m :< WeakTermTau)
      (e', t) <- infer' ((m, s, m :< WeakTermTau) : varEnv) e
      return (m :< WeakTermNoemaElim s e', t)
    m :< WeakTermArray elemType -> do
      elemType' <- inferType' varEnv elemType
      return (m :< WeakTermArray elemType', m :< WeakTermTau)
    m :< WeakTermArrayIntro _ elems -> do
      elemType <- newAster m varEnv
      (elems', ts') <- unzip <$> mapM (infer' varEnv) elems
      forM_ ts' $ Env.insConstraintEnv elemType
      return (m :< WeakTermArrayIntro elemType elems', m :< WeakTermArray elemType)
    m :< WeakTermArrayAccess _ _ array index -> do
      subject <- newAster m varEnv
      elemType <- newAster m varEnv
      (array', tArray) <- infer' varEnv array
      (index', tIndex) <- infer' varEnv index
      Env.insConstraintEnv (i64 m) tIndex
      let noeticArrayType = m :< WeakTermNoema subject (m :< WeakTermArray elemType)
      Env.insConstraintEnv noeticArrayType tArray
      return (m :< WeakTermArrayAccess subject elemType array' index', elemType)
    m :< WeakTermText ->
      return (term, m :< WeakTermTau)
    m :< WeakTermTextIntro _ -> do
      return (term, m :< WeakTermText)
    m :< WeakTermCell contentType -> do
      contentType' <- inferType' varEnv contentType
      return (m :< WeakTermCell contentType', m :< WeakTermTau)
    m :< WeakTermCellIntro _ content -> do
      (content', contentType) <- infer' varEnv content
      return (m :< WeakTermCellIntro contentType content', m :< WeakTermCell contentType)
    m :< WeakTermCellRead cell -> do
      (cell', cellType) <- infer' varEnv cell
      contentType <- newAster m varEnv
      subject <- newAster m varEnv
      Env.insConstraintEnv (m :< WeakTermNoema subject (m :< WeakTermCell contentType)) cellType
      return (m :< WeakTermCellRead cell', contentType)
    m :< WeakTermCellWrite cell newValue -> do
      (cell', cellType) <- infer' varEnv cell
      (newValue', newValueType) <- infer' varEnv newValue
      subject <- newAster m varEnv
      Env.insConstraintEnv (m :< WeakTermNoema subject (m :< WeakTermCell newValueType)) cellType
      return (m :< WeakTermCellWrite cell' newValue', m :< WeakTermEnum constTop)
    m :< WeakTermResourceType {} ->
      return (term, m :< WeakTermTau)

inferSubject :: Context m => Hint -> BoundVarEnv -> WeakTerm -> m WeakTerm
inferSubject m varEnv subject = do
  (subject', tSub) <- infer' varEnv subject
  Env.insConstraintEnv (m :< WeakTermTau) tSub
  return subject'

inferArgs ::
  Context m =>
  SubstWeakTerm ->
  Hint ->
  [(WeakTerm, WeakTerm)] ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  m WeakTerm
inferArgs sub m args1 args2 cod =
  case (args1, args2) of
    ([], []) ->
      Subst.subst sub cod
    ((e, t) : ets, (_, x, tx) : xts) -> do
      tx' <- Subst.subst sub tx
      Env.insConstraintEnv tx' t
      inferArgs (IntMap.insert (Ident.toInt x) e sub) m ets xts cod
    _ ->
      Throw.raiseCritical m "invalid argument passed to inferArgs"

inferType' :: Context m => BoundVarEnv -> WeakTerm -> m WeakTerm
inferType' varEnv t = do
  (t', u) <- infer' varEnv t
  Env.insConstraintEnv (metaOf t :< WeakTermTau) u
  return t'

inferPi ::
  Context m =>
  BoundVarEnv ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  m ([BinderF WeakTerm], WeakTerm)
inferPi varEnv binder cod =
  case binder of
    [] -> do
      (cod' :< mlPiCod) <- inferType' varEnv cod
      return ([], cod' :< mlPiCod)
    ((mx, x, t) : xts) -> do
      t' <- inferType' varEnv t
      insWeakTypeEnv x t'
      (xtls', tlCod) <- inferPi ((mx, x, t') : varEnv) xts cod
      return ((mx, x, t') : xtls', tlCod)

inferBinder ::
  Context m =>
  BoundVarEnv ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  m ([BinderF WeakTerm], (WeakTerm, WeakTerm))
inferBinder varEnv binder e =
  case binder of
    [] -> do
      etl' <- infer' varEnv e
      return ([], etl')
    ((mx, x, t) : xts) -> do
      t' <- inferType' varEnv t
      insWeakTypeEnv x t'
      (xts', etl') <- inferBinder ((mx, x, t') : varEnv) xts e
      return ((mx, x, t') : xts', etl')

inferPiElim ::
  Context m =>
  BoundVarEnv ->
  Hint ->
  (WeakTerm, WeakTerm) ->
  [(WeakTerm, WeakTerm)] ->
  m (WeakTerm, WeakTerm)
inferPiElim varEnv m (e, t) ets = do
  let es = map fst ets
  case t of
    (_ :< WeakTermPi xts (_ :< cod))
      | length xts == length ets -> do
        cod' <- inferArgs IntMap.empty m ets xts (m :< cod)
        return (m :< WeakTermPiElim e es, cod')
      | otherwise -> do
        raiseArityMismatchError e (length xts) (length ets)
    _ -> do
      ys <- mapM (const $ Gensym.newIdentFromText "arg") es
      yts <- newTypeAsterList varEnv $ zip ys (map metaOf es)
      cod <- newAster m (yts ++ varEnv)
      Env.insConstraintEnv (metaOf e :< WeakTermPi yts cod) t
      cod' <- inferArgs IntMap.empty m ets yts cod
      return (m :< WeakTermPiElim e es, cod')

raiseArityMismatchError :: Context m => WeakTerm -> Int -> Int -> m a
raiseArityMismatchError function expected actual = do
  case function of
    m :< WeakTermVarGlobal name _ -> do
      mImpArgNum <- Implicit.lookup name
      let k = I.reify $ fromMaybe I.zero mImpArgNum
      Throw.raiseError m $
        "the function `"
          <> DD.reify name
          <> "` expects "
          <> T.pack (show (expected - k))
          <> " arguments, but found "
          <> T.pack (show (actual - k))
          <> "."
    m :< _ ->
      Throw.raiseError m $
        "this function expects "
          <> T.pack (show expected)
          <> " arguments, but found "
          <> T.pack (show actual)
          <> "."

-- newAster :: Gensym.Context m => BoundVarEnv -> Hint -> m WeakTerm
-- newAster varEnv m =
--   Gensym.newAster m $ map (\(mx, x, _) -> mx :< WeakTermVar x) varEnv

newAster :: Context m => Hint -> BoundVarEnv -> m WeakTerm
newAster m varEnv = do
  newAster' m $ map (\(mx, x, _) -> mx :< WeakTermVar x) varEnv

newAster' :: Context m => Hint -> [WeakTerm] -> m WeakTerm
newAster' m es = do
  Gensym.newAster m es

-- i <- newHoleID
-- return $ m :< WeakTermAster i es

newTypedAster :: Context m => BoundVarEnv -> Hint -> m (WeakTerm, WeakTerm)
newTypedAster varEnv m = do
  app <- newAster m varEnv
  higherApp <- newAster m varEnv
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
newTypeAsterList :: Context m => BoundVarEnv -> [(Ident, Hint)] -> m [BinderF WeakTerm]
newTypeAsterList varEnv ids =
  case ids of
    [] ->
      return []
    ((x, m) : rest) -> do
      t <- newAster m varEnv
      insWeakTypeEnv x t
      ts <- newTypeAsterList ((m, x, t) : varEnv) rest
      return $ (m, x, t) : ts

inferEnumCase :: Context m => BoundVarEnv -> EnumCase -> m (EnumCase, WeakTerm)
inferEnumCase varEnv weakCase =
  case weakCase of
    m :< EnumCaseLabel (EnumLabel k _ _) -> do
      return (weakCase, m :< WeakTermEnum k)
    m :< EnumCaseDefault -> do
      h <- newAster m varEnv
      return (m :< EnumCaseDefault, h)
    m :< EnumCaseInt _ ->
      Throw.raiseCritical m "enum-case-int shouldn't be used in the target language"

-- lookupWeakTypeEnv :: Context m => Hint -> Ident -> m WeakTerm
-- lookupWeakTypeEnv m s = do
--   mt <- lookupWeakTypeEnvMaybe $ Ident.toInt s
--   case mt of
--     Just t ->
--       return t
--     Nothing ->
--       Throw.raiseCritical m $
--         Ident.toText' s <> " is not found in the weak type environment."

primOpToType :: Context m => Hint -> PrimOp -> m Term
primOpToType m (PrimOp op domList cod) = do
  let domList' = map (Term.fromPrimNum m) domList
  xs <- mapM (const (Gensym.newIdentFromText "_")) domList'
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
