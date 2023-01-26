module Scene.Elaborate.Infer
  ( Context (..),
    infer,
    inferType,
    inferBinder,
    inferDefineResource,
  )
where

import Context.Env qualified as Env
import Context.Gensym qualified as Gensym
import Context.Implicit qualified as Implicit
import Context.Log qualified as Log
import Context.Throw qualified as Throw
import Context.Type qualified as Type
import Control.Comonad.Cofree
import Control.Monad
import Data.IntMap qualified as IntMap
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.Arity qualified as A
import Entity.Binder
import Entity.DataInfo qualified as DI
import Entity.DecisionTree qualified as DT
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.HoleID qualified as HID
import Entity.Ident
import Entity.Ident.Reify qualified as Ident
import Entity.ImpArgNum qualified as I
import Entity.LamKind qualified as LK
import Entity.Magic qualified as M
import Entity.PrimNumSize qualified as PNS
import Entity.PrimOp
import Entity.PrimOp.OpSet
import Entity.PrimType qualified as PT
import Entity.Stmt
import Entity.Term qualified as TM
import Entity.Term.FromPrimNum qualified as Term
import Entity.Term.Weaken
import Entity.WeakArrayKind qualified as WAK
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Entity.WeakTerm qualified as WT
import Entity.WeakTerm.Subst qualified as Subst

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

inferDefineResource :: Context m => Hint -> DD.DefiniteDescription -> WT.WeakTerm -> WT.WeakTerm -> m WeakStmt
inferDefineResource m name discarder copier = do
  (discarder', td) <- infer discarder
  (copier', tc) <- infer copier
  x <- Gensym.newIdentFromText "_"
  let botTop = m :< WT.Pi [(m, x, m :< WT.Data DI.constBottom [])] (m :< WT.Data DI.constTop [])
  let botBot = m :< WT.Pi [(m, x, m :< WT.Data DI.constBottom [])] (m :< WT.Data DI.constBottom [])
  Env.insConstraintEnv botTop td
  Env.insConstraintEnv botBot tc
  return $ WeakStmtDefineResource m name discarder' copier'

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
        _ -> do
          (xts', (e', t')) <- inferBinder varEnv xts e
          return (m :< WT.PiIntro kind xts' e', m :< WT.Pi xts' t')
    m :< WT.PiElim e@(_ :< WT.VarGlobal name _) es -> do
      ets <- mapM (infer' varEnv) es
      t <- Type.lookup m name
      mImpArgNum <- Implicit.lookup name
      case mImpArgNum of
        Nothing -> do
          inferPiElim varEnv m (e, t) ets
        Just i -> do
          holes <- forM [1 .. I.reify i] $ const $ newTypedHole varEnv m
          inferPiElim varEnv m (e, t) $ holes ++ ets
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
    m :< WT.DataElim isNoetic oets tree -> do
      let (os, es, _) = unzip3 oets
      (es', ts') <- mapAndUnzipM (infer' varEnv) es
      forM_ (zip os ts') $ uncurry insWeakTypeEnv
      (tree', treeType) <- inferDecisionTree m varEnv tree
      return (m :< WT.DataElim isNoetic (zip3 os es' ts') tree', treeType)
    m :< WT.Array ak -> do
      ak' <- inferArrayKind varEnv ak
      return (m :< WT.Array ak', m :< WT.Tau)
    m :< WT.ArrayIntro ak es -> do
      ak' <- inferArrayKind varEnv ak
      let tElem = WAK.extract ak'
      (es', ts) <- mapAndUnzipM (infer' varEnv) es
      forM_ ts $ Env.insConstraintEnv tElem
      return (m :< WT.ArrayIntro ak' es', m :< WT.Array ak')
    m :< WT.ArrayElim ak array index -> do
      ak' <- inferArrayKind varEnv ak
      (array', tArray) <- infer' varEnv array
      (index', tIndex) <- infer' varEnv index
      Env.insConstraintEnv (m :< WT.Prim (WP.Type (PT.Int (PNS.IntSize 64)))) tIndex
      Env.insConstraintEnv (m :< WT.Noema (m :< WT.Array ak')) tArray
      return (m :< WT.ArrayElim ak' array' index', m :< WT.Noema (WAK.extract ak'))
    m :< WT.Noema t -> do
      t' <- inferType' varEnv t
      return (m :< WT.Noema t', m :< WT.Tau)
    m :< WT.Let opacity (mx, x, t) e1 e2 -> do
      (e1', t1') <- infer' varEnv e1
      t' <- inferType' varEnv t
      Env.insConstraintEnv t' t1'
      insWeakTypeEnv x t'
      (e2', t2') <- infer' varEnv e2 -- no context extension
      return (m :< WT.Let opacity (mx, x, t') e1' e2', t2')
    m :< WT.Hole x es -> do
      let rawHoleID = HID.reify x
      mHoleInfo <- lookupHoleEnv rawHoleID
      case mHoleInfo of
        Just holeInfo ->
          return holeInfo
        Nothing -> do
          holeType <- Gensym.newHole m es
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
    m :< WT.ResourceType {} ->
      return (term, m :< WT.Tau)
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
          resultType <- newHole m varEnv
          return (m :< WT.Magic der', resultType)

inferArrayKind ::
  Context m =>
  BoundVarEnv ->
  WAK.WeakArrayKind WT.WeakTerm ->
  m (WAK.WeakArrayKind WT.WeakTerm)
inferArrayKind varEnv = do
  mapM (inferType' varEnv)

-- case ak of
--   WAK.PrimType t -> do
--     t' <- inferType' varEnv t
--     return $ WAK.PrimType t'
--   WAK.General t -> do
--     t' <- inferType' varEnv t
--     return $ WAK.General t'

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
  (xts, cod) <- getPiType varEnv m (e, t) $ length ets
  _ :< cod' <- inferArgs IntMap.empty m ets xts cod
  return (m :< WT.PiElim e es, m :< cod')

getPiType ::
  Context m =>
  BoundVarEnv ->
  Hint ->
  (WT.WeakTerm, WT.WeakTerm) ->
  Int ->
  m ([BinderF WT.WeakTerm], WT.WeakTerm)
getPiType varEnv m (e, t) numOfArgs =
  case t of
    _ :< WT.Pi xts cod
      | length xts == numOfArgs ->
          return (xts, cod)
      | otherwise ->
          raiseArityMismatchError e (length xts) numOfArgs
    _ -> do
      ys <- mapM (const $ Gensym.newIdentFromText "arg") [1 .. numOfArgs]
      yts <- newTypeHoleList varEnv $ zip ys (replicate numOfArgs m)
      cod <- newHole m (yts ++ varEnv)
      Env.insConstraintEnv (WT.metaOf e :< WT.Pi yts cod) t
      return (yts, cod)

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

newHole :: Context m => Hint -> BoundVarEnv -> m WT.WeakTerm
newHole m varEnv = do
  Gensym.newHole m $ map (\(mx, x, _) -> mx :< WT.Var x) varEnv

newTypedHole :: Context m => BoundVarEnv -> Hint -> m (WT.WeakTerm, WT.WeakTerm)
newTypedHole varEnv m = do
  app <- newHole m varEnv
  higherApp <- newHole m varEnv
  return (app, higherApp)

-- In context varEnv == [x1, ..., xn], `newTypeHoleList varEnv [y1, ..., ym]` generates
-- the following list:
--
--   [(y1,   ?M1   @ (x1, ..., xn)),
--    (y2,   ?M2   @ (x1, ..., xn, y1),
--    ...,
--    (y{m}, ?M{m} @ (x1, ..., xn, y1, ..., y{m-1}))]
--
-- inserting type information `yi : ?Mi @ (x1, ..., xn, y1, ..., y{i-1})
newTypeHoleList :: Context m => BoundVarEnv -> [(Ident, Hint)] -> m [BinderF WT.WeakTerm]
newTypeHoleList varEnv ids =
  case ids of
    [] ->
      return []
    ((x, m) : rest) -> do
      t <- newHole m varEnv
      insWeakTypeEnv x t
      ts <- newTypeHoleList ((m, x, t) : varEnv) rest
      return $ (m, x, t) : ts

primOpToType :: Context m => Hint -> PrimOp -> m TM.Term
primOpToType m (PrimOp op domList cod) = do
  let domList' = map (Term.fromPrimNum m) domList
  xs <- mapM (const (Gensym.newIdentFromText "_")) domList'
  let xts = zipWith (\x t -> (m, x, t)) xs domList'
  if S.member op cmpOpSet
    then do
      let cod' = m :< TM.Data DI.constBool []
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
      h <- newHole m varEnv
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
  let (dataTerm, _) = unzip dataArgs
  typedDataArgs' <- mapM (infer' varEnv) dataTerm
  (consArgs', (body', tBody)) <- inferBinder' varEnv consArgs $ \extendedVarEnv ->
    inferDecisionTree m extendedVarEnv body
  et <- infer' varEnv $ m :< WT.VarGlobal consName (A.fromInt $ length dataArgs + length consArgs)
  (_, tPat) <- inferPiElim varEnv m et $ typedDataArgs' ++ map (\(mx, x, t) -> (mx :< WT.Var x, t)) consArgs'
  Env.insConstraintEnv tPat cursorType
  return (DT.Cons consName disc typedDataArgs' consArgs' body', tBody)
