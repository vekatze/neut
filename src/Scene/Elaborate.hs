module Scene.Elaborate
  ( elaborate,
    Context (..),
    insertStmt,
  )
where

import Context.Cache qualified as Cache
import Context.DataDefinition qualified as DataDefinition
import Context.Definition qualified as Definition
import Context.Env qualified as Env
import Context.Global qualified as Global
import Context.Implicit qualified as Implicit
import Context.Locator qualified as Locator
import Context.Log qualified as Log
import Context.Throw qualified as Throw
import Context.Type qualified as Type
import Control.Comonad.Cofree
import Control.Monad
import Data.IntMap qualified as IntMap
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.Binder
import Entity.DecisionTree qualified as DT
import Entity.DefiniteDescription qualified as DD
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.HoleSubst qualified as HS
import Entity.Ident.Reify qualified as Ident
import Entity.LamKind qualified as LK
import Entity.Prim qualified as P
import Entity.PrimType qualified as PT
import Entity.PrimValue qualified as PV
import Entity.Stmt
import Entity.Term qualified as TM
import Entity.Term.Reduce qualified as Term
import Entity.Term.Subst qualified as Subst
import Entity.Term.Weaken
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Entity.WeakTerm qualified as WT
import Entity.WeakTerm.ToText
import Scene.Elaborate.Infer qualified as Infer
import Scene.Elaborate.Unify qualified as Unify
import Scene.WeakTerm.Subst qualified as WT

class
  ( Infer.Context m,
    Unify.Context m,
    Subst.Context m,
    Log.Context m,
    Locator.Context m,
    Global.Context m,
    Cache.Context m,
    Definition.Context m,
    DataDefinition.Context m
  ) =>
  Context m
  where
  initialize :: m ()

elaborate :: Context m => Either [Stmt] [WeakStmt] -> m [Stmt]
elaborate cacheOrStmt = do
  initialize
  case cacheOrStmt of
    Left cache -> do
      forM_ cache insertStmt
      return cache
    Right defList -> do
      source <- Env.getCurrentSource
      mMainDefiniteDescription <- Locator.getMainDefiniteDescription source
      -- infer
      forM_ defList insertWeakStmt
      defList' <- mapM (inferStmt mMainDefiniteDescription) defList
      constraintList <- Env.getConstraintEnv
      -- unify
      Unify.unify constraintList >>= Env.setHoleSubst
      -- elaborate
      defList'' <- elaborateStmtList defList'
      forM_ defList'' insertStmt
      -- mapM_ (viewStmt . weakenStmt) defList''
      Cache.saveCache (source, defList'')
      return defList''

-- viewStmt :: Context m => WeakStmt -> m ()
-- viewStmt stmt = do
--   case stmt of
--     WeakStmtDefine _ m x _ xts codType e ->
--       Log.printNote m $ DD.reify x <> "\n" <> toText (m :< WT.Pi xts codType) <> "\n" <> toText (m :< WT.Pi xts e)
--     WeakStmtDefineResource m name discarder copier ->
--       Log.printNote m $ "define-resource" <> DD.reify name <> "\n" <> toText discarder <> toText copier

inferStmt :: Infer.Context m => Maybe DD.DefiniteDescription -> WeakStmt -> m WeakStmt
inferStmt mMainDD stmt = do
  case stmt of
    WeakStmtDefine isReducible m x impArgNum xts codType e -> do
      (xts', e', codType') <- inferStmtDefine xts e codType
      when (Just x == mMainDD) $
        Env.insConstraintEnv (m :< WT.Pi [] (WT.i64 m)) (m :< WT.Pi xts codType)
      return $ WeakStmtDefine isReducible m x impArgNum xts' codType' e'
    WeakStmtDefineResource m name discarder copier ->
      Infer.inferDefineResource m name discarder copier

inferStmtDefine ::
  Infer.Context m =>
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  WT.WeakTerm ->
  m ([BinderF WT.WeakTerm], WT.WeakTerm, WT.WeakTerm)
inferStmtDefine xts e codType = do
  (xts', (e', te)) <- Infer.inferBinder [] xts e
  codType' <- Infer.inferType codType
  Env.insConstraintEnv codType' te
  return (xts', e', codType')

elaborateStmtList :: Context m => [WeakStmt] -> m [Stmt]
elaborateStmtList stmtList = do
  case stmtList of
    [] ->
      return []
    WeakStmtDefine stmtKind m x impArgNum xts codType e : rest -> do
      stmtKind' <- elaborateStmtKind stmtKind
      e' <- elaborate' e >>= Term.reduce
      xts' <- mapM elaborateWeakBinder xts
      codType' <- elaborate' codType >>= Term.reduce
      Type.insert x $ weaken $ m :< TM.Pi xts' codType'
      let stmt = StmtDefine stmtKind' m x impArgNum xts' codType' e'
      rest' <- elaborateStmtList rest
      return $ stmt : rest'
    WeakStmtDefineResource m name discarder copier : rest -> do
      discarder' <- elaborate' discarder
      copier' <- elaborate' copier
      rest' <- elaborateStmtList rest
      return $ StmtDefineResource m name discarder' copier' : rest'

insertWeakStmt :: Context m => WeakStmt -> m ()
insertWeakStmt stmt = do
  case stmt of
    WeakStmtDefine stmtKind m f impArgNum xts codType e -> do
      Type.insert f $ m :< WT.Pi xts codType
      Implicit.insert f impArgNum
      Definition.insert (toOpacity stmtKind) m f xts e
    WeakStmtDefineResource m name _ _ ->
      Type.insert name $ m :< WT.Tau

insertStmt :: Context m => Stmt -> m ()
insertStmt stmt = do
  insertWeakStmt $ weakenStmt stmt
  insertStmtKindInfo stmt

insertStmtKindInfo :: Context m => Stmt -> m ()
insertStmtKindInfo stmt = do
  case stmt of
    StmtDefine stmtKind _ _ _ _ _ _ -> do
      case stmtKind of
        Normal _ ->
          return ()
        Data dataName dataArgs consInfoList -> do
          DataDefinition.insert dataName dataArgs consInfoList
        DataIntro {} ->
          return ()
    StmtDefineResource {} ->
      return ()

elaborateStmtKind :: Context m => StmtKindF WT.WeakTerm -> m (StmtKindF TM.Term)
elaborateStmtKind stmtKind =
  case stmtKind of
    Normal opacity ->
      return $ Normal opacity
    Data dataName dataArgs consInfoList -> do
      dataArgs' <- mapM elaborateWeakBinder dataArgs
      let (consNameList, consArgsList, discriminantList) = unzip3 consInfoList
      consArgsList' <- mapM (mapM elaborateWeakBinder) consArgsList
      return $ Data dataName dataArgs' $ zip3 consNameList consArgsList' discriminantList
    DataIntro dataName dataArgs consArgs discriminant -> do
      dataArgs' <- mapM elaborateWeakBinder dataArgs
      consArgs' <- mapM elaborateWeakBinder consArgs
      return $ DataIntro dataName dataArgs' consArgs' discriminant

elaborate' :: Context m => WT.WeakTerm -> m TM.Term
elaborate' term =
  case term of
    m :< WT.Tau ->
      return $ m :< TM.Tau
    m :< WT.Var x ->
      return $ m :< TM.Var x
    m :< WT.VarGlobal name arity ->
      return $ m :< TM.VarGlobal name arity
    m :< WT.Pi xts t -> do
      xts' <- mapM elaborateWeakBinder xts
      t' <- elaborate' t
      return $ m :< TM.Pi xts' t'
    m :< WT.PiIntro kind xts e -> do
      kind' <- elaborateKind kind
      xts' <- mapM elaborateWeakBinder xts
      e' <- elaborate' e
      return $ m :< TM.PiIntro kind' xts' e'
    m :< WT.PiElim e es -> do
      e' <- elaborate' e
      es' <- mapM elaborate' es
      return $ m :< TM.PiElim e' es'
    m :< WT.Data name es -> do
      es' <- mapM elaborate' es
      return $ m :< TM.Data name es'
    m :< WT.DataIntro dataName consName disc dataArgs consArgs -> do
      dataArgs' <- mapM elaborate' dataArgs
      consArgs' <- mapM elaborate' consArgs
      return $ m :< TM.DataIntro dataName consName disc dataArgs' consArgs'
    m :< WT.DataElim isNoetic oets tree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM elaborate' es
      ts' <- mapM elaborate' ts
      tree' <- elaborateDecisionTree m tree
      return $ m :< TM.DataElim isNoetic (zip3 os es' ts') tree'
    m :< WT.Noema t -> do
      t' <- elaborate' t
      return $ m :< TM.Noema t'
    m :< WT.Let opacity mxt e1 e2 -> do
      e1' <- elaborate' e1
      mxt'@(_, _, t) <- elaborateWeakBinder mxt
      e2' <- elaborate' e2
      case opacity of
        WT.Noetic -> do
          case (TM.containsNoema t, TM.containsPi t) of
            (True, _) ->
              Throw.raiseError m "the answer type of let-on cannot contain noemas"
            (_, True) ->
              Throw.raiseError m "the answer type of let-on cannot contain universal quantifications"
            _ ->
              return ()
        _ ->
          return ()
      let lamKind = LK.Normal (WT.reifyOpacity opacity)
      return $ m :< TM.PiElim (m :< TM.PiIntro lamKind [mxt'] e2') [e1']
    m :< WT.Hole h es -> do
      holeSubst <- Env.getHoleSubst
      case HS.lookup h holeSubst of
        Nothing ->
          Throw.raiseError m "couldn't instantiate the hole here"
        Just (xs, e)
          | length xs == length es -> do
              let s = IntMap.fromList $ zip (map Ident.toInt xs) es
              WT.subst s e >>= elaborate'
          | otherwise ->
              Throw.raiseError m "arity mismatch"
    m :< WT.Prim prim ->
      case prim of
        WP.Type t ->
          return $ m :< TM.Prim (P.Type t)
        WP.Value primValue ->
          case primValue of
            WPV.Int t x -> do
              t' <- elaborate' t >>= Term.reduce
              case t' of
                _ :< TM.Prim (P.Type (PT.Int size)) ->
                  return $ m :< TM.Prim (P.Value (PV.Int size x))
                _ -> do
                  Throw.raiseError m $
                    "the term `"
                      <> T.pack (show x)
                      <> "` is an integer, but its type is: "
                      <> toText (weaken t')
            WPV.Float t x -> do
              t' <- elaborate' t >>= Term.reduce
              case t' of
                _ :< TM.Prim (P.Type (PT.Float size)) ->
                  return $ m :< TM.Prim (P.Value (PV.Float size x))
                _ -> do
                  Throw.raiseError m $
                    "the term `"
                      <> T.pack (show x)
                      <> "` is a float, but its type is: "
                      <> toText (weaken t')
            WPV.Op op ->
              return $ m :< TM.Prim (P.Value (PV.Op op))
    m :< WT.ResourceType name ->
      return $ m :< TM.ResourceType name
    m :< WT.Magic der -> do
      der' <- mapM elaborate' der
      return $ m :< TM.Magic der'

elaborateWeakBinder :: Context m => BinderF WT.WeakTerm -> m (BinderF TM.Term)
elaborateWeakBinder (m, x, t) = do
  t' <- elaborate' t
  return (m, x, t')

elaborateKind :: Context m => LK.LamKindF WT.WeakTerm -> m (LK.LamKindF TM.Term)
elaborateKind kind =
  case kind of
    LK.Normal opacity ->
      return $ LK.Normal opacity
    LK.Fix xt -> do
      xt' <- elaborateWeakBinder xt
      return $ LK.Fix xt'

-- cs <- readIORef constraintEnv
-- p "==========================================================="
-- forM_ cs $ \(e1, e2) -> do
--   p $ T.unpack $ toText e1
--   p $ T.unpack $ toText e2
--   p "---------------------"

elaborateDecisionTree :: Context m => Hint -> DT.DecisionTree WT.WeakTerm -> m (DT.DecisionTree TM.Term)
elaborateDecisionTree m tree =
  case tree of
    DT.Leaf xs body -> do
      body' <- elaborate' body
      return $ DT.Leaf xs body'
    DT.Unreachable ->
      return DT.Unreachable
    DT.Switch (cursor, cursorType) (fallbackClause, clauseList) -> do
      cursorType' <- elaborate' cursorType >>= Term.reduce
      consList <- extractConstructorList m cursorType'
      let activeConsList = DT.getConstructors clauseList
      let diff = S.difference (S.fromList consList) (S.fromList activeConsList)
      if S.size diff == 0
        then do
          clauseList' <- mapM (elaborateClause m) clauseList
          return $ DT.Switch (cursor, cursorType') (DT.Unreachable, clauseList')
        else do
          case fallbackClause of
            DT.Unreachable ->
              Throw.raiseError m "encountered a non-exhaustive pattern matching"
            _ -> do
              fallbackClause' <- elaborateDecisionTree m fallbackClause
              clauseList' <- mapM (elaborateClause m) clauseList
              return $ DT.Switch (cursor, cursorType') (fallbackClause', clauseList')

elaborateClause :: Context m => Hint -> DT.Case WT.WeakTerm -> m (DT.Case TM.Term)
elaborateClause m (DT.Cons consName disc dataArgs consArgs cont) = do
  let (dataTerms, dataTypes) = unzip dataArgs
  dataTerms' <- mapM elaborate' dataTerms
  dataTypes' <- mapM elaborate' dataTypes
  consArgs' <- mapM elaborateWeakBinder consArgs
  cont' <- elaborateDecisionTree m cont
  return $ DT.Cons consName disc (zip dataTerms' dataTypes') consArgs' cont'

extractConstructorList :: Context m => Hint -> TM.Term -> m [DD.DefiniteDescription]
extractConstructorList m cursorType = do
  case cursorType of
    _ :< TM.Data dataName _ -> do
      kind <- Global.lookup dataName
      case kind of
        Just (GN.Data _ consList) ->
          return consList
        _ ->
          Throw.raiseCritical m "extractConstructorList"
    _ :< TM.PiElim (_ :< TM.Data dataName _) _ -> do
      kind <- Global.lookup dataName
      case kind of
        Just (GN.Data _ consList) ->
          return consList
        _ ->
          Throw.raiseCritical m "extractConstructorList"
    _ :< TM.PiElim (_ :< TM.VarGlobal dataName _) _ -> do
      kind <- Global.lookup dataName
      case kind of
        Just (GN.Data _ consList) ->
          return consList
        _ ->
          Throw.raiseCritical m "extractConstructorList"
    _ ->
      Throw.raiseError m $ "the type of this term is expected to be an ADT, but it's not:\n" <> toText (weaken cursorType)
