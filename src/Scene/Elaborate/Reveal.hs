module Scene.Elaborate.Reveal
  ( revealStmt,
  )
where

import Context.App
import Context.Elaborate
import Context.Gensym qualified as Gensym
import Context.Implicit qualified as Implicit
import Control.Comonad.Cofree
import Entity.Annotation qualified as AN
import Entity.ArgNum qualified as AN
import Entity.Arity qualified as A
import Entity.Binder
import Entity.DecisionTree qualified as DT
import Entity.LamKind qualified as LK
import Entity.Magic qualified as M
import Entity.Opacity qualified as O
import Entity.Stmt
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Entity.WeakTerm qualified as WT

type BoundVarEnv = [BinderF WT.WeakTerm]

revealStmt :: WeakStmt -> App WeakStmt
revealStmt stmt =
  case stmt of
    WeakStmtDefine isConstLike stmtKind m x impArgNum xts codType e -> do
      (xts', (codType', e')) <- revealBinder' [] xts $ \varEnv -> do
        codType' <- reveal' varEnv codType
        e' <- reveal' varEnv e
        return (codType', e')
      return $ WeakStmtDefine isConstLike stmtKind m x impArgNum xts' codType' e'
    WeakStmtDefineResource m name discarder copier -> do
      discarder' <- reveal' [] discarder
      copier' <- reveal' [] copier
      return $ WeakStmtDefineResource m name discarder' copier'
    WeakStmtExport m alias dd gn ->
      return $ WeakStmtExport m alias dd gn

reveal' :: BoundVarEnv -> WT.WeakTerm -> App WT.WeakTerm
reveal' varEnv term =
  case term of
    _ :< WT.Tau ->
      return term
    _ :< WT.Var {} ->
      return term
    m :< WT.VarGlobal name arity -> do
      mImpArgNum <- Implicit.lookup name
      case mImpArgNum of
        Just impArgNum
          | AN.reify impArgNum > 0 -> do
              suppliedHoles <- mapM (const $ newHole m varEnv) [1 .. AN.reify impArgNum]
              args <- mapM (const $ Gensym.newIdentFromText "arg") [1 .. fromInteger (A.reify arity) - AN.reify impArgNum]
              let enrichedArgs = map (,m) args
              binder <- newTypeHoleList varEnv enrichedArgs
              let app = m :< WT.PiElim term (suppliedHoles ++ map (\(x, mx) -> mx :< WT.Var x) enrichedArgs)
              return $ m :< WT.PiIntro (LK.Normal O.Transparent) binder app
        _ ->
          return term
    m :< WT.Pi xts t -> do
      (xts', t') <- revealPi varEnv xts t
      return $ m :< WT.Pi xts' t'
    m :< WT.PiIntro kind xts e -> do
      case kind of
        LK.Fix (mx, x, t) -> do
          t' <- reveal' varEnv t
          (xts', e') <- revealBinder varEnv xts e
          return $ m :< WT.PiIntro (LK.Fix (mx, x, t')) xts' e'
        _ -> do
          (xts', e') <- revealBinder varEnv xts e
          return $ m :< WT.PiIntro kind xts' e'
    m :< WT.PiElim e es -> do
      es' <- mapM (reveal' varEnv) es
      e' <- reveal' varEnv e
      return $ m :< WT.PiElim e' es'
    m :< WT.Data name es -> do
      es' <- mapM (reveal' varEnv) es
      return $ m :< WT.Data name es'
    m :< WT.DataIntro dataName consName disc dataArgs consArgs -> do
      dataArgs' <- mapM (reveal' varEnv) dataArgs
      consArgs' <- mapM (reveal' varEnv) consArgs
      return $ m :< WT.DataIntro dataName consName disc dataArgs' consArgs'
    m :< WT.DataElim isNoetic oets tree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (reveal' varEnv) es
      ts' <- mapM (reveal' varEnv) ts
      tree' <- revealDecisionTree varEnv tree
      return $ m :< WT.DataElim isNoetic (zip3 os es' ts') tree'
    m :< WT.Noema t -> do
      t' <- reveal' varEnv t
      return $ m :< WT.Noema t'
    m :< WT.Embody t e -> do
      t' <- reveal' varEnv t
      e' <- reveal' varEnv e
      return $ m :< WT.Embody t' e'
    m :< WT.Cell t -> do
      t' <- reveal' varEnv t
      return $ m :< WT.Cell t'
    m :< WT.CellIntro e -> do
      e' <- reveal' varEnv e
      return $ m :< WT.CellIntro e'
    m :< WT.CellElim e -> do
      e' <- reveal' varEnv e
      return $ m :< WT.CellElim e'
    m :< WT.Let opacity (mx, x, t) e1 e2 -> do
      e1' <- reveal' varEnv e1
      t' <- reveal' varEnv t
      e2' <- reveal' varEnv e2 -- no context extension
      return $ m :< WT.Let opacity (mx, x, t') e1' e2'
    m :< WT.Hole x es -> do
      es' <- mapM (reveal' varEnv) es
      return $ m :< WT.Hole x es'
    m :< WT.Prim prim
      | WP.Type _ <- prim ->
          return term
      | WP.Value primValue <- prim ->
          case primValue of
            WPV.Int t v -> do
              t' <- reveal' [] t
              return $ m :< WT.Prim (WP.Value (WPV.Int t' v))
            WPV.Float t v -> do
              t' <- reveal' [] t
              return $ m :< WT.Prim (WP.Value (WPV.Float t' v))
            WPV.Op {} ->
              return term
            WPV.StaticText {} ->
              return term
    _ :< WT.ResourceType {} ->
      return term
    m :< WT.Magic der -> do
      case der of
        M.Cast from to value -> do
          from' <- reveal' varEnv from
          to' <- reveal' varEnv to
          value' <- reveal' varEnv value
          return $ m :< WT.Magic (M.Cast from' to' value')
        _ -> do
          der' <- mapM (reveal' varEnv) der
          return $ m :< WT.Magic der'
    m :< WT.Annotation logLevel annot e -> do
      e' <- reveal' varEnv e
      case annot of
        AN.Type t -> do
          t' <- reveal' varEnv t
          return $ m :< WT.Annotation logLevel (AN.Type t') e'

revealPi ::
  BoundVarEnv ->
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  App ([BinderF WT.WeakTerm], WT.WeakTerm)
revealPi varEnv binder cod =
  case binder of
    [] -> do
      (cod' :< mlPiCod) <- reveal' varEnv cod
      return ([], cod' :< mlPiCod)
    ((mx, x, t) : xts) -> do
      t' <- reveal' varEnv t
      insWeakTypeEnv x t'
      (xtls', tlCod) <- revealPi ((mx, x, t') : varEnv) xts cod
      return ((mx, x, t') : xtls', tlCod)

revealBinder ::
  BoundVarEnv ->
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  App ([BinderF WT.WeakTerm], WT.WeakTerm)
revealBinder varEnv binder e =
  revealBinder' varEnv binder (`reveal'` e)

revealBinder' ::
  BoundVarEnv ->
  [BinderF WT.WeakTerm] ->
  ([BinderF WT.WeakTerm] -> App a) ->
  App ([BinderF WT.WeakTerm], a)
revealBinder' varEnv binder comp =
  case binder of
    [] -> do
      result <- comp varEnv
      return ([], result)
    ((mx, x, t) : xts) -> do
      t' <- reveal' varEnv t
      insWeakTypeEnv x t'
      (xts', etl') <- revealBinder' ((mx, x, t') : varEnv) xts comp
      return ((mx, x, t') : xts', etl')

revealDecisionTree ::
  BoundVarEnv ->
  DT.DecisionTree WT.WeakTerm ->
  App (DT.DecisionTree WT.WeakTerm)
revealDecisionTree varEnv tree =
  case tree of
    DT.Leaf ys body -> do
      body' <- reveal' varEnv body
      return $ DT.Leaf ys body'
    DT.Unreachable -> do
      return DT.Unreachable
    DT.Switch (cursor, cursorType) clauseList -> do
      cursorType' <- reveal' varEnv cursorType
      clauseList' <- revealClauseList varEnv clauseList
      return $ DT.Switch (cursor, cursorType') clauseList'

revealClauseList ::
  BoundVarEnv ->
  DT.CaseList WT.WeakTerm ->
  App (DT.CaseList WT.WeakTerm)
revealClauseList varEnv (fallbackClause, clauseList) = do
  fallbackClause' <- revealDecisionTree varEnv fallbackClause
  clauseList' <- mapM (revealClause varEnv) clauseList
  return (fallbackClause', clauseList')

revealClause ::
  BoundVarEnv ->
  DT.Case WT.WeakTerm ->
  App (DT.Case WT.WeakTerm)
revealClause varEnv (DT.Cons mCons consName disc dataArgs consArgs body) = do
  let (dataTerms, dataTypeTerms) = unzip dataArgs
  dataTerms' <- mapM (reveal' varEnv) dataTerms
  dataTypeTerms' <- mapM (reveal' varEnv) dataTypeTerms
  (consArgs', body') <- revealBinder' varEnv consArgs $ \extendedVarEnv ->
    revealDecisionTree extendedVarEnv body
  return (DT.Cons mCons consName disc (zip dataTerms' dataTypeTerms') consArgs' body')
