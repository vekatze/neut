module Scene.Elaborate.Infer
  ( Context (..),
    infer,
    inferType,
    inferBinder,
  )
where

import qualified Context.Env as Env
import qualified Context.Gensym as Gensym
import qualified Context.Implicit as Implicit
import qualified Context.Log as Log
import qualified Context.Throw as Throw
import qualified Context.Type as Type
import Control.Comonad.Cofree
import Control.Monad
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Entity.Arity as A
import Entity.Binder
import qualified Entity.DecisionTree as DT
import qualified Entity.DefiniteDescription as DD
import qualified Entity.EnumCase as EC
import Entity.EnumInfo
import Entity.Hint
import qualified Entity.HoleID as HID
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import qualified Entity.ImpArgNum as I
import qualified Entity.LamKind as LK
import qualified Entity.Magic as M
import Entity.PrimOp
import Entity.PrimOp.OpSet
import qualified Entity.Term as TM
import qualified Entity.Term.FromPrimNum as Term
import Entity.Term.Weaken
import qualified Entity.WeakPrim as WP
import qualified Entity.WeakPrimValue as WPV
import qualified Entity.WeakTerm as WT
import qualified Entity.WeakTerm.Subst as Subst
import Entity.WeakTerm.ToText

type BoundVarEnv = [BinderF WT.WeakTerm]

class
  ( Throw.Context m,
    Type.Context m,
    Subst.Context m,
    Gensym.Context m,
    Implicit.Context m,
    Env.Context m,
    Log.Context m
  ) =>
  Context m
  where
  insWeakTypeEnv :: Ident -> WT.WeakTerm -> m ()
  lookupWeakTypeEnv :: Hint -> Ident -> m WT.WeakTerm
  lookupHoleEnv :: Int -> m (Maybe (WT.WeakTerm, WT.WeakTerm))
  insHoleEnv :: Int -> WT.WeakTerm -> WT.WeakTerm -> m ()

infer :: Context m => WT.WeakTerm -> m (WT.WeakTerm, WT.WeakTerm)
infer =
  infer' []

inferType :: Context m => WT.WeakTerm -> m WT.WeakTerm
inferType =
  inferType' []

infer' :: Context m => BoundVarEnv -> WT.WeakTerm -> m (WT.WeakTerm, WT.WeakTerm)
infer' varEnv term =
  case term of
    _ :< WT.Tau ->
      return (term, term)
    m :< WT.Var x -> do
      _ :< t <- lookupWeakTypeEnv m x
      return (term, m :< t)
    m :< WT.VarGlobal name _ -> do
      _ :< t <- Type.lookup m name
      return (term, m :< t)
    m :< WT.Pi xts t -> do
      (xts', t') <- inferPi varEnv xts t
      return (m :< WT.Pi xts' t', m :< WT.Tau)
    m :< WT.PiIntro kind xts e -> do
      case kind of
        LK.Fix (mx, x, t) -> do
          t' <- inferType' varEnv t
          insWeakTypeEnv x t'
          (xts', (e', tCod)) <- inferBinder varEnv xts e
          let piType = m :< WT.Pi xts' tCod
          Env.insConstraintEnv piType t'
          return (m :< WT.PiIntro (LK.Fix (mx, x, t')) xts' e', piType)
        -- LK.Cons dataName consName discriminant dataType -> do
        --   dataType' <- inferType' varEnv dataType
        --   (xts', (e', _)) <- inferBinder varEnv xts e
        --   return (m :< WT.PiIntro (LK.Cons dataName consName discriminant dataType') xts' e', dataType')
        _ -> do
          (xts', (e', t')) <- inferBinder varEnv xts e
          return (m :< WT.PiIntro kind xts' e', m :< WT.Pi xts' t')
    m :< WT.PiElim e@(_ :< WT.VarGlobal name _) es -> do
      etls <- mapM (infer' varEnv) es
      t <- Type.lookup m name
      mImpArgNum <- Implicit.lookup name
      case mImpArgNum of
        Nothing -> do
          inferPiElim varEnv m (e, t) etls
        Just i -> do
          holes <- forM [1 .. I.reify i] $ const $ newTypedAster varEnv m
          inferPiElim varEnv m (e, t) $ holes ++ etls
    m :< WT.PiElim e es -> do
      etls <- mapM (infer' varEnv) es
      etl <- infer' varEnv e
      inferPiElim varEnv m etl etls
    m :< WT.Data name es -> do
      (es', _) <- mapAndUnzipM (infer' varEnv) es
      return (m :< WT.Data name es', m :< WT.Tau)
    m :< WT.DataIntro dataName consName disc dataArgs consArgs -> do
      (dataArgs', _) <- mapAndUnzipM (infer' varEnv) dataArgs
      (consArgs', _) <- mapAndUnzipM (infer' varEnv) consArgs
      return (m :< WT.DataIntro dataName consName disc dataArgs' consArgs', m :< WT.Data dataName dataArgs')
    m :< WT.DataElim oets tree -> do
      let (os, es, _) = unzip3 oets
      (es', ts') <- mapAndUnzipM (infer' varEnv) es
      forM_ (zip os ts') $ uncurry insWeakTypeEnv
      (tree', treeType) <- inferDecisionTree m varEnv tree
      return (m :< WT.DataElim (zip3 os es' ts') tree', treeType)
    -- resultType <- newAster m varEnv
    -- (e', t') <- infer' varEnv e
    -- clauseList' <- forM clauseList $ \(pat@(mPat, name, arity, xts), body) -> do
    --   (xts', (body', tBody)) <- inferBinder varEnv xts body
    --   Env.insConstraintEnv resultType tBody
    --   (_, tPat) <- infer' varEnv $ patternToTerm pat
    --   Env.insConstraintEnv tPat t'
    --   return ((mPat, name, arity, xts'), body')
    -- return (m :< WT.DataElim (e', t') clauseList', resultType)
    m :< WT.Sigma xts -> do
      (xts', _) <- inferPi varEnv xts (m :< WT.Tau)
      return (m :< WT.Sigma xts', m :< WT.Tau)
    m :< WT.SigmaIntro es -> do
      ets <- mapM (infer' varEnv) es
      ys <- mapM (const $ Gensym.newIdentFromText "arg") es
      yts <- newTypeAsterList varEnv $ zip ys (map WT.metaOf es)
      _ <- inferArgs IntMap.empty m ets yts (m :< WT.Tau)
      return (m :< WT.SigmaIntro (map fst ets), m :< WT.Sigma yts)
    m :< WT.SigmaElim xts e1 e2 -> do
      (e1', t1') <- infer' varEnv e1
      (xts', (e2', t)) <- inferBinder varEnv xts e2
      Env.insConstraintEnv (m :< WT.Sigma xts') t1'
      return (m :< WT.SigmaElim xts' e1' e2', t)
    m :< WT.Let (mx, x, t) e1 e2 -> do
      (e1', t1') <- infer' varEnv e1
      t' <- inferType' varEnv t
      Env.insConstraintEnv t' t1'
      insWeakTypeEnv x t'
      (e2', t2') <- infer' varEnv e2 -- no context extension
      return (m :< WT.Let (mx, x, t') e1' e2', t2')
    m :< WT.Aster x es -> do
      let rawHoleID = HID.reify x
      mAsterInfo <- lookupHoleEnv rawHoleID
      case mAsterInfo of
        Just asterInfo ->
          return asterInfo
        Nothing -> do
          holeType <- Gensym.newAster m es
          insHoleEnv rawHoleID term holeType
          return (term, holeType)
    m :< WT.Prim prim
      | WP.Type _ <- prim ->
          return (term, m :< WT.Tau)
      | WP.Value primValue <- prim ->
          case primValue of
            WPV.Int t v -> do
              t' <- inferType' [] t
              return (m :< WT.Prim (WP.Value (WPV.Int t' v)), t')
            WPV.Float t v -> do
              t' <- inferType' [] t
              return (m :< WT.Prim (WP.Value (WPV.Float t' v)), t')
            WPV.Op op -> do
              primOpType <- primOpToType m op
              return (term, weaken primOpType)
    m :< WT.Enum _ ->
      return (term, m :< WT.Tau)
    m :< WT.EnumIntro (EC.EnumLabel k _ _) -> do
      return (term, m :< WT.Enum k)
    m :< WT.EnumElim (e, _) ces -> do
      (e', t') <- infer' varEnv e
      let (cs, es) = unzip ces
      (cs', tcs) <- mapAndUnzipM inferEnumCase cs
      forM_ (zip tcs (repeat t')) $ uncurry Env.insConstraintEnv
      (es', ts) <- mapAndUnzipM (infer' varEnv) es
      h <- newAster m varEnv
      forM_ (zip (repeat h) ts) $ uncurry Env.insConstraintEnv
      return (m :< WT.EnumElim (e', t') (zip cs' es'), h)
    m :< WT.Question e _ -> do
      (e', te) <- infer' varEnv e
      return (m :< WT.Question e' te, te)
    m :< WT.Magic der -> do
      case der of
        M.Cast from to value -> do
          from' <- inferType' varEnv from
          to' <- inferType' varEnv to
          (value', t) <- infer' varEnv value
          Env.insConstraintEnv t from'
          return (m :< WT.Magic (M.Cast from' to' value'), to')
        _ -> do
          der' <- mapM (infer' varEnv >=> return . fst) der
          resultType <- newAster m varEnv
          return (m :< WT.Magic der', resultType)

inferArgs ::
  Context m =>
  WT.SubstWeakTerm ->
  Hint ->
  [(WT.WeakTerm, WT.WeakTerm)] ->
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  m WT.WeakTerm
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

inferType' :: Context m => BoundVarEnv -> WT.WeakTerm -> m WT.WeakTerm
inferType' varEnv t = do
  (t', u) <- infer' varEnv t
  Env.insConstraintEnv (WT.metaOf t :< WT.Tau) u
  return t'

inferPi ::
  Context m =>
  BoundVarEnv ->
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  m ([BinderF WT.WeakTerm], WT.WeakTerm)
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
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  m ([BinderF WT.WeakTerm], (WT.WeakTerm, WT.WeakTerm))
inferBinder varEnv binder e =
  inferBinder' varEnv binder (`infer'` e)

inferBinder' ::
  Context m =>
  BoundVarEnv ->
  [BinderF WT.WeakTerm] ->
  ([BinderF WT.WeakTerm] -> m a) ->
  m ([BinderF WT.WeakTerm], a)
inferBinder' varEnv binder comp =
  case binder of
    [] -> do
      result <- comp varEnv
      return ([], result)
    ((mx, x, t) : xts) -> do
      t' <- inferType' varEnv t
      insWeakTypeEnv x t'
      (xts', etl') <- inferBinder' ((mx, x, t') : varEnv) xts comp
      return ((mx, x, t') : xts', etl')

inferPiElim ::
  Context m =>
  BoundVarEnv ->
  Hint ->
  (WT.WeakTerm, WT.WeakTerm) ->
  [(WT.WeakTerm, WT.WeakTerm)] ->
  m (WT.WeakTerm, WT.WeakTerm)
inferPiElim varEnv m (e, t) ets = do
  let es = map fst ets
  case t of
    (_ :< WT.Pi xts (_ :< cod))
      | length xts == length ets -> do
          cod' <- inferArgs IntMap.empty m ets xts (m :< cod)
          return (m :< WT.PiElim e es, cod')
      | otherwise -> do
          raiseArityMismatchError e (length xts) (length ets)
    _ -> do
      ys <- mapM (const $ Gensym.newIdentFromText "arg") es
      yts <- newTypeAsterList varEnv $ zip ys (map WT.metaOf es)
      cod <- newAster m (yts ++ varEnv)
      Env.insConstraintEnv (WT.metaOf e :< WT.Pi yts cod) t
      cod' <- inferArgs IntMap.empty m ets yts cod
      return (m :< WT.PiElim e es, cod')

raiseArityMismatchError :: Context m => WT.WeakTerm -> Int -> Int -> m a
raiseArityMismatchError function expected actual = do
  case function of
    m :< WT.VarGlobal name _ -> do
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

newAster :: Context m => Hint -> BoundVarEnv -> m WT.WeakTerm
newAster m varEnv = do
  Gensym.newAster m $ map (\(mx, x, _) -> mx :< WT.Var x) varEnv

newTypedAster :: Context m => BoundVarEnv -> Hint -> m (WT.WeakTerm, WT.WeakTerm)
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
newTypeAsterList :: Context m => BoundVarEnv -> [(Ident, Hint)] -> m [BinderF WT.WeakTerm]
newTypeAsterList varEnv ids =
  case ids of
    [] ->
      return []
    ((x, m) : rest) -> do
      t <- newAster m varEnv
      insWeakTypeEnv x t
      ts <- newTypeAsterList ((m, x, t) : varEnv) rest
      return $ (m, x, t) : ts

inferEnumCase :: Context m => EC.EnumCase -> m (EC.EnumCase, WT.WeakTerm)
inferEnumCase weakCase =
  case weakCase of
    m :< EC.Label (EC.EnumLabel k _ _) -> do
      return (weakCase, m :< WT.Enum k)
    m :< EC.Int _ ->
      Throw.raiseCritical m "enum-case-int shouldn't be used in the target language"

primOpToType :: Context m => Hint -> PrimOp -> m TM.Term
primOpToType m (PrimOp op domList cod) = do
  let domList' = map (Term.fromPrimNum m) domList
  xs <- mapM (const (Gensym.newIdentFromText "_")) domList'
  let xts = zipWith (\x t -> (m, x, t)) xs domList'
  if S.member op cmpOpSet
    then do
      let cod' = m :< TM.Enum constBool
      return $ m :< TM.Pi xts cod'
    else do
      let cod' = Term.fromPrimNum m cod
      return $ m :< TM.Pi xts cod'

inferDecisionTree ::
  Context m =>
  Hint ->
  BoundVarEnv ->
  DT.DecisionTree WT.WeakTerm ->
  m (DT.DecisionTree WT.WeakTerm, WT.WeakTerm)
inferDecisionTree m varEnv tree =
  case tree of
    DT.Leaf ys body -> do
      (body', answerType) <- infer' varEnv body
      return (DT.Leaf ys body', answerType)
    DT.Unreachable -> do
      h <- newAster m varEnv
      return (DT.Unreachable, h)
    DT.Switch (cursor, _) clauseList -> do
      cursorType <- lookupWeakTypeEnv m cursor
      (clauseList', answerType) <- inferClauseList m varEnv cursorType clauseList
      return (DT.Switch (cursor, cursorType) clauseList', answerType)

inferClauseList ::
  Context m =>
  Hint ->
  BoundVarEnv ->
  WT.WeakTerm ->
  DT.CaseList WT.WeakTerm ->
  m (DT.CaseList WT.WeakTerm, WT.WeakTerm)
inferClauseList m varEnv cursorType (fallbackClause, clauseList) = do
  (fallbackClause', fallbackAnswerType) <- inferDecisionTree m varEnv fallbackClause
  (clauseList', answerTypeList) <- mapAndUnzipM (inferClause m varEnv cursorType) clauseList
  forM_ answerTypeList $ \answerType -> Env.insConstraintEnv answerType fallbackAnswerType
  return ((fallbackClause', clauseList'), fallbackAnswerType)

inferClause ::
  Context m =>
  Hint ->
  BoundVarEnv ->
  WT.WeakTerm ->
  DT.Case WT.WeakTerm ->
  m (DT.Case WT.WeakTerm, WT.WeakTerm)
inferClause m varEnv cursorType (DT.Cons consName disc dataArgs consArgs body) = do
  typedDataArgs' <- mapM (infer' varEnv) dataArgs
  (consArgs', (body', tBody)) <- inferBinder' varEnv consArgs $ \extendedVarEnv ->
    inferDecisionTree m extendedVarEnv body
  et <- infer' varEnv $ m :< WT.VarGlobal consName (A.fromInt $ length dataArgs + length consArgs)
  (_, tPat) <- inferPiElim varEnv m et $ typedDataArgs' ++ map (\(mx, x, t) -> (mx :< WT.Var x, t)) consArgs'
  Env.insConstraintEnv tPat cursorType
  return (DT.Cons consName disc (map fst typedDataArgs') consArgs' body', tBody)
