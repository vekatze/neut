module Scene.Elaborate
  ( elaborate,
    Context (..),
  )
where

import qualified Context.DataDefinition as DataDefinition
import qualified Context.Definition as Definition
import qualified Context.Env as Env
import qualified Context.Global as Global
import qualified Context.Implicit as Implicit
import qualified Context.Locator as Locator
import qualified Context.Log as Log
import qualified Context.Throw as Throw
import qualified Context.Type as Type
import Control.Comonad.Cofree
import Control.Monad
import qualified Data.IntMap as IntMap
import Data.List
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.Binder
import qualified Entity.DecisionTree as DT
import qualified Entity.DefiniteDescription as DD
import qualified Entity.EnumCase as EC
import Entity.EnumInfo
import qualified Entity.EnumTypeName as ET
import qualified Entity.EnumValueName as EV
import qualified Entity.GlobalName as GN
import Entity.Hint
import qualified Entity.HoleSubst as HS
import qualified Entity.Ident.Reify as Ident
import qualified Entity.LamKind as LK
import qualified Entity.Prim as P
import qualified Entity.PrimType as PT
import qualified Entity.PrimValue as PV
import qualified Entity.Source as Source
import Entity.Stmt
import qualified Entity.Term as TM
import qualified Entity.Term.Reduce as Term
import qualified Entity.Term.Subst as Subst
import Entity.Term.Weaken
import qualified Entity.WeakPrim as WP
import qualified Entity.WeakPrimValue as WPV
import qualified Entity.WeakTerm as WT
import qualified Entity.WeakTerm.Subst as WT
import Entity.WeakTerm.ToText
import qualified Scene.Elaborate.Infer as Infer
import qualified Scene.Elaborate.Unify as Unify

class
  ( Infer.Context m,
    Unify.Context m,
    Subst.Context m,
    Log.Context m,
    Locator.Context m,
    Global.Context m,
    Definition.Context m,
    DataDefinition.Context m
  ) =>
  Context m
  where
  initialize :: m ()
  saveCache :: Program -> [EnumInfo] -> m ()

elaborate :: Context m => Source.Source -> Either [Stmt] ([WeakStmt], [EnumInfo]) -> m [Stmt]
elaborate source cacheOrStmt = do
  initialize
  case cacheOrStmt of
    Left cache -> do
      forM_ cache insertStmt
      return cache
    Right (defList, enumInfoList) -> do
      mMainDefiniteDescription <- Locator.getMainDefiniteDescription source
      -- infer
      forM_ defList insertWeakStmt
      -- defList' <- mapM setupDef defList
      defList' <- mapM (inferStmt mMainDefiniteDescription) defList
      constraintList <- Env.getConstraintEnv
      -- unify
      Unify.unify constraintList >>= Env.setHoleSubst
      -- elaborate
      defList'' <- elaborateStmtList defList'
      forM_ defList'' insertStmt
      saveCache (source, defList'') enumInfoList
      return defList''

-- setupDef :: Context m => WeakStmt -> m WeakStmt
-- setupDef def =
--   case def of
--     WeakStmtDefine stmtKind m f impArgNum xts codType e -> do
--       Type.insert f $ m :< WT.Pi xts codType
--       Implicit.insert f impArgNum
--       Definition.insert (toOpacity stmtKind) m f xts e
--       return $ WeakStmtDefine stmtKind m f impArgNum xts codType e

inferStmt :: Infer.Context m => Maybe DD.DefiniteDescription -> WeakStmt -> m WeakStmt
inferStmt mMainDD stmt = do
  case stmt of
    WeakStmtDefine isReducible m x impArgNum xts codType e -> do
      (xts', e', codType') <- inferStmtDefine xts e codType
      when (Just x == mMainDD) $
        Env.insConstraintEnv (m :< WT.Pi [] (WT.i64 m)) (m :< WT.Pi xts codType)
      return $ WeakStmtDefine isReducible m x impArgNum xts' codType' e'

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
      e' <- elaborate' e
      xts' <- mapM elaborateWeakBinder xts
      codType' <- elaborate' codType >>= Term.reduce
      Type.insert x $ weaken $ m :< TM.Pi xts' codType'
      let stmt = StmtDefine stmtKind' m x impArgNum xts' codType' e'
      -- insertStmt stmt
      -- Definition.insert (toOpacity stmtKind) m x (map weakenBinder xts') (weaken e')
      rest' <- elaborateStmtList rest
      return $ stmt : rest'

insertWeakStmt :: Context m => WeakStmt -> m ()
insertWeakStmt (WeakStmtDefine stmtKind m f impArgNum xts codType e) = do
  Type.insert f $ m :< WT.Pi xts codType
  Implicit.insert f impArgNum
  Definition.insert (toOpacity stmtKind) m f xts e

-- fixme: implement via insertWeakStmt & weaken
insertStmt :: Context m => Stmt -> m ()
insertStmt stmt = do
  insertWeakStmt $ weakenStmt stmt
  insertStmtKindInfo stmt

insertStmtKindInfo :: Context m => Stmt -> m ()
insertStmtKindInfo (StmtDefine stmtKind _ _ _ _ _ _) = do
  case stmtKind of
    DataIntro dataName dataArgs consArgs discriminant ->
      DataDefinition.insert dataName discriminant dataArgs consArgs
    _ ->
      return ()

-- insertStmt (StmtDefine stmtKind' m name impArgNum xts codType e) = do
-- Type.insert name $ weaken $ m :< TM.Pi xts codType
-- Implicit.insert name impArgNum
-- Definition.insert (toOpacity stmtKind') m name (map weakenBinder xts) (weaken e)

elaborateStmtKind :: Context m => StmtKindF WT.WeakTerm -> m (StmtKindF TM.Term)
elaborateStmtKind stmtKind =
  case stmtKind of
    Normal opacity ->
      return $ Normal opacity
    Data arity dataName consNameList ->
      return $ Data arity dataName consNameList
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
    m :< WT.DataElim oets tree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM elaborate' es
      ts' <- mapM elaborate' ts
      tree' <- elaborateDecisionTree m tree
      return $ m :< TM.DataElim (zip3 os es' ts') tree'
    m :< WT.Sigma xts -> do
      xts' <- mapM elaborateWeakBinder xts
      return $ m :< TM.Sigma xts'
    m :< WT.SigmaIntro es -> do
      es' <- mapM elaborate' es
      return $ m :< TM.SigmaIntro es'
    m :< WT.SigmaElim xts e1 e2 -> do
      e1' <- elaborate' e1
      xts' <- mapM elaborateWeakBinder xts
      e2' <- elaborate' e2
      return $ m :< TM.SigmaElim xts' e1' e2'
    m :< WT.Let mxt e1 e2 -> do
      e1' <- elaborate' e1
      mxt' <- elaborateWeakBinder mxt
      e2' <- elaborate' e2
      return $ m :< TM.Let mxt' e1' e2'
    m :< WT.Aster h es -> do
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
    m :< WT.Enum k ->
      return $ m :< TM.Enum k
    m :< WT.EnumIntro label ->
      return $ m :< TM.EnumIntro label
    m :< WT.EnumElim (e, t) les -> do
      e' <- elaborate' e
      let (ls, es) = unzip les
      es' <- mapM elaborate' es
      t' <- elaborate' t >>= Term.reduce
      case t' of
        _ :< TM.Enum x -> do
          checkSwitchExaustiveness m x ls
          return $ m :< TM.EnumElim (e', t') (zip ls es')
        _ ->
          Throw.raiseError m $
            "the type of `"
              <> toText (weaken e')
              <> "` must be an enum type, but is:\n"
              <> toText (weaken t')
    m :< WT.Question e t -> do
      e' <- elaborate' e
      t' <- elaborate' t
      Log.printNote m $ toText (weaken t')
      return e'
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
    LK.Normal ->
      return LK.Normal
    LK.Fix xt -> do
      xt' <- elaborateWeakBinder xt
      return $ LK.Fix xt'

checkSwitchExaustiveness :: Context m => Hint -> ET.EnumTypeName -> [EC.EnumCase] -> m ()
checkSwitchExaustiveness m enumTypeName caseList = do
  let containsDefaultCase = doesContainDefaultCase caseList
  enumSet <- lookupEnumSet m enumTypeName
  let len = toInteger $ length (nub caseList)
  unless (toInteger (length enumSet) <= len || containsDefaultCase) $
    Throw.raiseError m "this switch is ill-constructed in that it is not exhaustive"

lookupEnumSet :: Context m => Hint -> ET.EnumTypeName -> m [EV.EnumValueName]
lookupEnumSet m enumTypeName = do
  let name = ET.reify enumTypeName
  mEnumItems <- Global.lookup name
  case mEnumItems of
    Just (GN.EnumType enumItems) ->
      return $ map fst enumItems
    _ ->
      Throw.raiseError m $ "no such enum defined: " <> DD.reify name

doesContainDefaultCase :: [EC.EnumCase] -> Bool
doesContainDefaultCase enumCaseList =
  case enumCaseList of
    [] ->
      False
    _ : rest ->
      doesContainDefaultCase rest

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
    DT.Switch (cursor, cursorType) clauseList -> do
      cursorType' <- elaborate' cursorType >>= Term.reduce
      clauseList' <- elaborateClauseList m clauseList
      ensureExhaustiveness m cursorType' clauseList'
      return $ DT.Switch (cursor, cursorType') clauseList'

elaborateClauseList :: Context m => Hint -> DT.CaseList WT.WeakTerm -> m (DT.CaseList TM.Term)
elaborateClauseList m (fallbackClause, clauseList) = do
  fallbackClause' <- elaborateDecisionTree m fallbackClause
  clauseList' <- mapM (elaborateClause m) clauseList
  return (fallbackClause', clauseList')

elaborateClause :: Context m => Hint -> DT.Case WT.WeakTerm -> m (DT.Case TM.Term)
elaborateClause m (DT.Cons consName disc dataArgs consArgs cont) = do
  dataArgs' <- mapM elaborateWeakBinder dataArgs
  consArgs' <- mapM elaborateWeakBinder consArgs
  cont' <- elaborateDecisionTree m cont
  return $ DT.Cons consName disc dataArgs' consArgs' cont'

ensureExhaustiveness :: Context m => Hint -> TM.Term -> DT.CaseList TM.Term -> m ()
ensureExhaustiveness m cursorType (fallbackClause, clauseList) = do
  consList <- extractConstructorList m cursorType
  let activeConsList = DT.getConstructors clauseList
  let diff = S.difference (S.fromList consList) (S.fromList activeConsList)
  case (S.size diff == 0, fallbackClause) of
    (True, _) ->
      return ()
    (_, DT.Unreachable) ->
      Throw.raiseError m "encountered a non-exhaustive pattern matching"
    _ ->
      return ()

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
    _ ->
      Throw.raiseError m "the type of this term is expected to be an ADT, but it's not."
