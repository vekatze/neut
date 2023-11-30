module Scene.Elaborate.Infer (inferStmt) where

import Context.App
import Context.Elaborate
import Context.Env qualified as Env
import Context.Gensym qualified as Gensym
import Context.Throw qualified as Throw
import Context.Type qualified as Type
import Context.WeakDefinition qualified as WeakDefinition
import Control.Comonad.Cofree
import Control.Monad
import Data.IntMap qualified as IntMap
import Entity.Annotation qualified as Annotation
import Entity.ArgNum qualified as AN
import Entity.Attr.Data qualified as AttrD
import Entity.Attr.DataIntro qualified as AttrDI
import Entity.Attr.Lam qualified as AttrL
import Entity.Attr.VarGlobal qualified as AttrVG
import Entity.Binder
import Entity.Const
import Entity.DecisionTree qualified as DT
import Entity.Decl qualified as DE
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.HoleID qualified as HID
import Entity.HoleSubst qualified as HS
import Entity.Ident (isHole)
import Entity.Ident.Reify qualified as Ident
import Entity.LamKind qualified as LK
import Entity.Magic qualified as M
import Entity.Name qualified as N
import Entity.PrimOp
import Entity.Stmt
import Entity.StmtKind
import Entity.Term qualified as TM
import Entity.Term.FromPrimNum qualified as Term
import Entity.Term.Weaken
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Entity.WeakTerm qualified as WT
import Scene.Elaborate.Unify (unifyCurrentConstraints)
import Scene.Parse.Discern.Name qualified as N
import Scene.WeakTerm.Reduce qualified as WT
import Scene.WeakTerm.Subst qualified as Subst
import Scene.WeakTerm.Subst qualified as WT

type BoundVarEnv = [BinderF WT.WeakTerm]

inferStmt :: Maybe DD.DefiniteDescription -> WeakStmt -> App WeakStmt
inferStmt mMainDD stmt =
  case stmt of
    WeakStmtDefine isConstLike stmtKind m x impArgs expArgs codType e -> do
      insertType x $ m :< WT.Pi impArgs expArgs codType
      stmtKind' <- inferStmtKind stmtKind
      (impArgs', varEnv) <- inferBinder' [] impArgs
      (expArgs', varEnv') <- inferBinder' varEnv expArgs
      codType' <- inferType varEnv' codType
      (e', te) <- infer' varEnv' e
      insConstraintEnv codType' te
      when (mMainDD == Just x) $ do
        let _m = m {metaShouldSaveLocation = False}
        unitType <- getUnitType _m
        insConstraintEnv (m :< WT.Pi [] [] unitType) (m :< WT.Pi impArgs' expArgs' codType')
      return $ WeakStmtDefine isConstLike stmtKind' m x impArgs' expArgs' codType' e'
    WeakStmtDefineConst m dd t v -> do
      insertType dd $ m :< WT.Pi [] [] t
      t' <- inferType [] t
      (v', tv) <- infer' [] v
      insConstraintEnv t' tv
      return $ WeakStmtDefineConst m dd t' v'
    WeakStmtDeclare m declList -> do
      declList' <- mapM inferDecl declList
      return $ WeakStmtDeclare m declList'

inferDecl :: DE.Decl WT.WeakTerm -> App (DE.Decl WT.WeakTerm)
inferDecl DE.Decl {..} = do
  (impArgs', varEnv) <- inferBinder' [] impArgs
  (expArgs', varEnv') <- inferBinder' varEnv impArgs
  cod' <- inferType varEnv' cod
  insertType name $ loc :< WT.Pi impArgs' expArgs' cod'
  return $ DE.Decl {impArgs = impArgs', expArgs = expArgs', cod = cod', ..}

insertType :: DD.DefiniteDescription -> WT.WeakTerm -> App ()
insertType dd t = do
  typeOrNone <- Type.lookupMaybe dd
  case typeOrNone of
    Nothing ->
      return ()
    Just declaredType ->
      insConstraintEnv declaredType t
  Type.insert dd t

inferStmtKind :: StmtKind WT.WeakTerm -> App (StmtKind WT.WeakTerm)
inferStmtKind stmtKind =
  case stmtKind of
    Normal {} ->
      return stmtKind
    Data dataName dataArgs consInfoList -> do
      (dataArgs', varEnv) <- inferBinder' [] dataArgs
      consInfoList' <- forM consInfoList $ \(m, dd, constLike, consArgs, discriminant) -> do
        (consArgs', _) <- inferBinder' varEnv consArgs
        return (m, dd, constLike, consArgs', discriminant)
      return $ Data dataName dataArgs' consInfoList'
    DataIntro consName dataArgs consArgs discriminant -> do
      (dataArgs', varEnv) <- inferBinder' [] dataArgs
      (consArgs', _) <- inferBinder' varEnv consArgs
      return $ DataIntro consName dataArgs' consArgs' discriminant

getIntType :: Hint -> App WT.WeakTerm
getIntType m = do
  baseSize <- Env.getBaseSize m
  return $ WT.intTypeBySize m baseSize

getUnitType :: Hint -> App WT.WeakTerm
getUnitType m = do
  locator <- Throw.liftEither $ DD.getLocatorPair m coreUnit
  (unitDD, _) <- N.resolveName m (N.Locator locator)
  let attr = AttrVG.Attr {argNum = AN.fromInt 0, isConstLike = True, isExplicit = False}
  return $ m :< WT.PiElim (m :< WT.VarGlobal attr unitDD) []

infer' :: BoundVarEnv -> WT.WeakTerm -> App (WT.WeakTerm, WT.WeakTerm)
infer' varEnv term =
  case term of
    _ :< WT.Tau ->
      return (term, term)
    m :< WT.Var x -> do
      _ :< t <- lookupWeakTypeEnv m x
      return (term, m :< t)
    m :< WT.VarGlobal _ name -> do
      _ :< t <- Type.lookup m name
      return (term, m :< t)
    m :< WT.Pi impArgs expArgs t -> do
      (impArgs', varEnv') <- inferPiBinder varEnv impArgs
      (expArgs', varEnv'') <- inferPiBinder varEnv' expArgs
      t' <- inferType varEnv'' t
      return (m :< WT.Pi impArgs' expArgs' t', m :< WT.Tau)
    m :< WT.PiIntro attr@(AttrL.Attr {lamKind}) xts e -> do
      case lamKind of
        LK.Fix (mx, x, codType) -> do
          (xts', extendedVarEnv) <- inferBinder' varEnv xts
          codType' <- inferType extendedVarEnv codType
          let piType = m :< WT.Pi [] xts' codType'
          insWeakTypeEnv x piType
          (e', tBody) <- infer' extendedVarEnv e
          insConstraintEnv codType' tBody
          let term' = m :< WT.PiIntro (attr {AttrL.lamKind = LK.Fix (mx, x, codType')}) xts' e'
          return (term', piType)
        _ -> do
          (xts', (e', t')) <- inferBinder varEnv xts e
          let term' = m :< WT.PiIntro attr xts' e'
          return (term', m :< WT.Pi [] xts' t')
    m :< WT.PiElim e es -> do
      etl <- infer' varEnv e
      etls <- mapM (infer' varEnv) es
      inferPiElim m etl etls
    _ :< WT.PiElimExact {} ->
      undefined
    m :< WT.Data attr name es -> do
      (es', _) <- mapAndUnzipM (infer' varEnv) es
      return (m :< WT.Data attr name es', m :< WT.Tau)
    m :< WT.DataIntro attr@(AttrDI.Attr {..}) consName dataArgs consArgs -> do
      (dataArgs', _) <- mapAndUnzipM (infer' varEnv) dataArgs
      (consArgs', _) <- mapAndUnzipM (infer' varEnv) consArgs
      let dataType = m :< WT.Data (AttrD.Attr {..}) dataName dataArgs'
      return (m :< WT.DataIntro attr consName dataArgs' consArgs', dataType)
    m :< WT.DataElim isNoetic oets tree -> do
      let (os, es, _) = unzip3 oets
      (es', ts') <- mapAndUnzipM (infer' varEnv) es
      forM_ (zip os ts') $ uncurry insWeakTypeEnv
      (tree', _ :< treeType) <- inferDecisionTree m varEnv tree
      return (m :< WT.DataElim isNoetic (zip3 os es' ts') tree', m :< treeType)
    m :< WT.Noema t -> do
      t' <- inferType varEnv t
      return (m :< WT.Noema t', m :< WT.Tau)
    m :< WT.Embody _ e -> do
      (e', noemaType) <- infer' varEnv e
      resultType <- newHole m varEnv
      insConstraintEnv (m :< WT.Noema resultType) noemaType
      return (m :< WT.Embody resultType e', resultType)
    m :< WT.Let opacity (mx, x, t) e1 e2 -> do
      (e1', t1') <- infer' varEnv e1
      t' <- inferType varEnv t
      insWeakTypeEnv x t'
      case opacity of
        WT.Noetic ->
          insertActualityConstraint t'
        _ ->
          return ()
      insConstraintEnv t' t1' -- run this before `infer' varEnv e2`
      (e2', t2') <- infer' varEnv e2 -- no context extension
      return (m :< WT.Let opacity (mx, x, t') e1' e2', t2')
    m :< WT.Hole holeID es -> do
      let rawHoleID = HID.reify holeID
      mHoleInfo <- lookupHoleEnv rawHoleID
      case mHoleInfo of
        Just (_ :< holeTerm, _ :< holeType) -> do
          return (m :< holeTerm, m :< holeType)
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
              t' <- inferType [] t
              return (m :< WT.Prim (WP.Value (WPV.Int t' v)), t')
            WPV.Float t v -> do
              t' <- inferType [] t
              return (m :< WT.Prim (WP.Value (WPV.Float t' v)), t')
            WPV.Op op -> do
              primOpType <- primOpToType m op
              return (term, weaken primOpType)
            WPV.StaticText t text -> do
              t' <- inferType [] t
              return (m :< WT.Prim (WP.Value (WPV.StaticText t' text)), m :< WT.Noema t')
    m :< WT.Magic der -> do
      case der of
        M.Cast from to value -> do
          from' <- inferType varEnv from
          to'@(_ :< toInner) <- inferType varEnv to
          (value', t) <- infer' varEnv value
          insConstraintEnv from' t
          return (m :< WT.Magic (M.Cast from' to' value'), m :< toInner)
        _ -> do
          der' <- mapM (infer' varEnv >=> return . fst) der
          resultType <- newHole m varEnv
          return (m :< WT.Magic der', resultType)
    m :< WT.Annotation logLevel annot e -> do
      (e', t) <- infer' varEnv e
      case annot of
        Annotation.Type _ -> do
          return (m :< WT.Annotation logLevel (Annotation.Type t) e', t)
    m :< WT.Resource dd resourceID discarder copier -> do
      (discarder', td) <- infer' [] discarder
      (copier', tc) <- infer' [] copier
      x <- Gensym.newIdentFromText "_"
      intType <- getIntType m
      let tDiscard = m :< WT.Pi [] [(m, x, intType)] intType
      let tCopy = m :< WT.Pi [] [(m, x, intType)] intType
      insConstraintEnv tDiscard td
      insConstraintEnv tCopy tc
      return (m :< WT.Resource dd resourceID discarder' copier', m :< WT.Tau)

inferArgs ::
  WT.SubstWeakTerm ->
  Hint ->
  [(WT.WeakTerm, WT.WeakTerm)] ->
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  App WT.WeakTerm
inferArgs sub m args1 args2 cod =
  case (args1, args2) of
    ([], []) ->
      Subst.subst sub cod
    ((e, t) : ets, (_, x, tx) : xts) -> do
      tx' <- Subst.subst sub tx
      insConstraintEnv tx' t
      inferArgs (IntMap.insert (Ident.toInt x) (Right e) sub) m ets xts cod
    _ ->
      Throw.raiseCritical m "invalid argument passed to inferArgs"

inferType :: BoundVarEnv -> WT.WeakTerm -> App WT.WeakTerm
inferType varEnv t = do
  (t', u) <- infer' varEnv t
  insConstraintEnv (WT.metaOf t :< WT.Tau) u
  return t'

inferPiBinder ::
  BoundVarEnv ->
  [BinderF WT.WeakTerm] ->
  App ([BinderF WT.WeakTerm], BoundVarEnv)
inferPiBinder varEnv binder =
  case binder of
    [] -> do
      return ([], varEnv)
    ((mx, x, t) : xts) -> do
      t' <- inferType varEnv t
      insWeakTypeEnv x t'
      let varEnv' = if isHole x then varEnv else (mx, x, t') : varEnv
      (xtls', varEnv'') <- inferPiBinder varEnv' xts
      return ((mx, x, t') : xtls', varEnv'')

inferBinder ::
  BoundVarEnv ->
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  App ([BinderF WT.WeakTerm], (WT.WeakTerm, WT.WeakTerm))
inferBinder varEnv binder e = do
  (binder', extendedVarEnv) <- inferBinder' varEnv binder
  et <- infer' extendedVarEnv e
  return (binder', et)

inferBinder' ::
  BoundVarEnv ->
  [BinderF WT.WeakTerm] ->
  App ([BinderF WT.WeakTerm], BoundVarEnv)
inferBinder' varEnv binder =
  case binder of
    [] -> do
      return ([], varEnv)
    ((mx, x, t) : xts) -> do
      t' <- inferType varEnv t
      insWeakTypeEnv x t'
      (xts', newVarEnv) <- inferBinder' ((mx, x, t') : varEnv) xts
      return ((mx, x, t') : xts', newVarEnv)

inferPiElim ::
  Hint ->
  (WT.WeakTerm, WT.WeakTerm) ->
  [(WT.WeakTerm, WT.WeakTerm)] ->
  App (WT.WeakTerm, WT.WeakTerm)
inferPiElim m (e, t) ets = do
  let es = map fst ets
  t' <- resolveType t
  (xts, cod) <- getPiType m (e, t') $ length ets
  _ :< cod' <- inferArgs IntMap.empty m ets xts cod
  return (m :< WT.PiElim e es, m :< cod')

getPiType ::
  Hint ->
  (WT.WeakTerm, WT.WeakTerm) ->
  Int ->
  App ([BinderF WT.WeakTerm], WT.WeakTerm)
-- getPiType m (e, t) numOfArgs =
getPiType =
  undefined

-- case t of
--   _ :< WT.Pi impArgs expArgs cod
--     | length xts == numOfArgs ->
--         return (xts, cod)
--     | otherwise ->
--         raiseArityMismatchError e (length xts) numOfArgs
--   _ -> do
--     Throw.raiseError m $ "expected a function type, but got: " <> toText t

-- raiseArityMismatchError :: WT.WeakTerm -> Int -> Int -> App a
-- raiseArityMismatchError function expected actual = do
--   case function of
--     m :< WT.VarGlobal _ name -> do
--       Throw.raiseError m $
--         "the function `"
--           <> DD.reify name
--           <> "` expects "
--           <> T.pack (show expected)
--           <> " arguments, but found "
--           <> T.pack (show actual)
--           <> "."
--     m :< _ ->
--       Throw.raiseError m $
--         "this function expects "
--           <> T.pack (show expected)
--           <> " arguments, but found "
--           <> T.pack (show actual)
--           <> "."

primOpToType :: Hint -> PrimOp -> App TM.Term
primOpToType m op = do
  let (domList, cod) = getTypeInfo op
  let domList' = map (Term.fromPrimNum m) domList
  xs <- mapM (const (Gensym.newIdentFromText "_")) domList'
  let xts = zipWith (\x t -> (m, x, t)) xs domList'
  let cod' = Term.fromPrimNum m cod
  return $ m :< TM.Pi [] xts cod'

inferDecisionTree ::
  Hint ->
  BoundVarEnv ->
  DT.DecisionTree WT.WeakTerm ->
  App (DT.DecisionTree WT.WeakTerm, WT.WeakTerm)
inferDecisionTree m varEnv tree =
  case tree of
    DT.Leaf ys body -> do
      (body', answerType) <- infer' varEnv body
      return (DT.Leaf ys body', answerType)
    DT.Unreachable -> do
      h <- newHole m varEnv
      return (DT.Unreachable, h)
    DT.Switch (cursor, mCursor :< _) clauseList -> do
      _ :< cursorType <- lookupWeakTypeEnv m cursor
      let cursorType' = mCursor :< cursorType
      (clauseList', answerType) <- inferClauseList m varEnv cursorType' clauseList
      return (DT.Switch (cursor, cursorType') clauseList', answerType)

inferClauseList ::
  Hint ->
  BoundVarEnv ->
  WT.WeakTerm ->
  DT.CaseList WT.WeakTerm ->
  App (DT.CaseList WT.WeakTerm, WT.WeakTerm)
inferClauseList m varEnv cursorType (fallbackClause, clauseList) = do
  (clauseList', answerTypeList) <- mapAndUnzipM (inferClause varEnv cursorType) clauseList
  (fallbackClause', fallbackAnswerType) <- inferDecisionTree m varEnv fallbackClause
  h <- newHole m varEnv
  forM_ (answerTypeList ++ [fallbackAnswerType]) $ insConstraintEnv h
  return ((fallbackClause', clauseList'), fallbackAnswerType)

inferClause ::
  BoundVarEnv ->
  WT.WeakTerm ->
  DT.Case WT.WeakTerm ->
  App (DT.Case WT.WeakTerm, WT.WeakTerm)
inferClause varEnv cursorType decisionCase@(DT.Case {..}) = do
  let m = DT.mCons decisionCase
  let (dataTermList, _) = unzip dataArgs
  typedDataArgs' <- mapM (infer' varEnv) dataTermList
  (consArgs', extendedVarEnv) <- inferBinder' varEnv consArgs
  let argNum = AN.fromInt $ length dataArgs + length consArgs
  let attr = AttrVG.Attr {isExplicit = False, ..}
  consTerm <- infer' varEnv $ m :< WT.VarGlobal attr consDD
  (_, tPat) <- inferPiElim m consTerm $ typedDataArgs' ++ map (\(mx, x, t) -> (mx :< WT.Var x, t)) consArgs'
  insConstraintEnv cursorType tPat
  (cont', tCont) <- inferDecisionTree m extendedVarEnv cont
  return
    ( decisionCase
        { DT.dataArgs = typedDataArgs',
          DT.consArgs = consArgs',
          DT.cont = cont'
        },
      tCont
    )

resolveType :: WT.WeakTerm -> App WT.WeakTerm
resolveType t = do
  sub <- unifyCurrentConstraints
  reduceWeakType sub t

reduceWeakType :: HS.HoleSubst -> WT.WeakTerm -> App WT.WeakTerm
reduceWeakType sub e = do
  e' <- WT.reduce e
  case e' of
    m :< WT.Hole h es ->
      case HS.lookup h sub of
        Nothing ->
          return e'
        Just (xs, body)
          | length xs == length es -> do
              let s = IntMap.fromList $ zip (map Ident.toInt xs) (map Right es)
              WT.subst s body >>= reduceWeakType sub
          | otherwise ->
              Throw.raiseError m "arity mismatch"
    m :< WT.PiElim (_ :< WT.VarGlobal _ name) args -> do
      mLam <- WeakDefinition.lookup name
      case mLam of
        Just lam ->
          reduceWeakType sub $ m :< WT.PiElim lam args
        Nothing -> do
          return e'
    _ ->
      return e'
