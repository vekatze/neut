module Scene.Elaborate (elaborate) where

import Context.App
import Context.Cache qualified as Cache
import Context.DataDefinition qualified as DataDefinition
import Context.Definition qualified as Definition
import Context.Elaborate
import Context.Env qualified as Env
import Context.Locator qualified as Locator
import Context.Remark qualified as Remark
import Context.Throw qualified as Throw
import Context.Type qualified as Type
import Context.WeakDefinition qualified as WeakDefinition
import Control.Comonad.Cofree
import Control.Monad
import Data.IntMap qualified as IntMap
import Data.List
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.Annotation qualified as AN
import Entity.Attr.Data qualified as AttrD
import Entity.Binder
import Entity.Cache qualified as Cache
import Entity.DecisionTree qualified as DT
import Entity.Decl qualified as DE
import Entity.DefiniteDescription qualified as DD
import Entity.ExternalName qualified as EN
import Entity.Hint
import Entity.HoleID qualified as HID
import Entity.HoleSubst qualified as HS
import Entity.Ident.Reify qualified as Ident
import Entity.LamKind qualified as LK
import Entity.Magic qualified as M
import Entity.Opacity qualified as O
import Entity.Prim qualified as P
import Entity.PrimType qualified as PT
import Entity.PrimValue qualified as PV
import Entity.Remark qualified as Remark
import Entity.Stmt
import Entity.StmtKind
import Entity.Term qualified as TM
import Entity.Term.Weaken
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Entity.WeakTerm qualified as WT
import Entity.WeakTerm.ToText
import Scene.Elaborate.Infer qualified as Infer
import Scene.Elaborate.Reveal qualified as Reveal
import Scene.Elaborate.Unify qualified as Unify
import Scene.Term.Inline qualified as TM
import Scene.Term.Reduce qualified as Term
import Scene.WeakTerm.Reduce qualified as WT
import Scene.WeakTerm.Subst qualified as WT

elaborate :: Either Cache.Cache ([WeakStmt], [DE.Decl]) -> App ([Stmt], [DE.Decl])
elaborate cacheOrStmt = do
  initialize
  case cacheOrStmt of
    Left cache -> do
      let stmtList = Cache.stmtList cache
      forM_ stmtList insertStmt
      let remarkList = Cache.remarkList cache
      Remark.insertToGlobalRemarkList remarkList
      Remark.printRemarkList remarkList
      let declList = Cache.declList cache
      return (stmtList, declList)
    Right (defList, declList) -> do
      defList' <- (analyzeDefList >=> synthesizeDefList declList) defList
      return (defList', declList)

analyzeDefList :: [WeakStmt] -> App [WeakStmt]
analyzeDefList defList = do
  defList' <- forM defList $ \stmt -> do
    stmt' <- Reveal.revealStmt stmt
    insertWeakStmt stmt'
    return stmt'
  source <- Env.getCurrentSource
  mMainDD <- Locator.getMainDefiniteDescription source
  mapM (Infer.inferStmt mMainDD) defList'

-- viewStmt :: WeakStmt -> App ()
-- viewStmt stmt = do
--   case stmt of
--     WeakStmtDefine _ _ m x _ xts codType e ->
--       Remark.printNote m $ DD.reify x <> "\n" <> toText (m :< WT.Pi xts codType) <> "\n" <> toText (m :< WT.PiIntro LK.Normal xts e)
--     WeakStmtDefineResource m name discarder copier ->
--       Remark.printNote m $ "define-resource" <> DD.reify name <> "\n" <> toText discarder <> toText copier

synthesizeDefList :: [DE.Decl] -> [WeakStmt] -> App [Stmt]
synthesizeDefList declList defList = do
  -- mapM_ viewStmt defList
  getConstraintEnv >>= Unify.unify >>= setHoleSubst
  defList' <- mapM elaborateStmt defList
  defList'' <- mapM inlineStmt defList'
  -- mapM_ (viewStmt . weakenStmt) defList'
  source <- Env.getCurrentSource
  remarkList <- Remark.getRemarkList
  tmap <- Env.getTagMap
  Cache.saveCache source $
    Cache.Cache
      { Cache.stmtList = defList'',
        Cache.remarkList = remarkList,
        Cache.locationTree = tmap,
        Cache.declList = declList
      }
  Remark.printRemarkList remarkList
  Remark.insertToGlobalRemarkList remarkList
  return defList''

elaborateStmt :: WeakStmt -> App Stmt
elaborateStmt stmt = do
  case stmt of
    WeakStmtDefine isConstLike stmtKind m x impArgNum xts codType e -> do
      stmtKind' <- elaborateStmtKind stmtKind
      e' <- elaborate' e >>= Term.reduce
      xts' <- mapM elaborateWeakBinder xts
      codType' <- elaborate' codType >>= Term.reduce
      Type.insert x $ weaken $ m :< TM.Pi xts' codType'
      let result = StmtDefine isConstLike stmtKind' (SavedHint m) x impArgNum xts' codType' e'
      insertStmt result
      return result
    WeakStmtDefineConst m dd t v -> do
      t' <- elaborate' t
      v' <- elaborate' v
      let result = StmtDefineConst (SavedHint m) dd t' v'
      insertStmt result
      return result
    WeakStmtDefineResource m name discarder copier -> do
      discarder' <- elaborate' discarder
      copier' <- elaborate' copier
      let result = StmtDefineResource (SavedHint m) name discarder' copier'
      insertStmt result
      return result

inlineStmt :: Stmt -> App Stmt
inlineStmt stmt = do
  case stmt of
    StmtDefine isConstLike stmtKind m@(SavedHint m') x impArgNum xts codType e -> do
      e' <- TM.inline m' e
      return $ StmtDefine isConstLike stmtKind m x impArgNum xts codType e'
    StmtDefineConst m@(SavedHint m') dd t v -> do
      t' <- TM.inline m' t
      v' <- TM.inline m' v
      unless (TM.isValue v') $ do
        Throw.raiseError m' $
          "couldn't reduce this term into a constant, but got:\n" <> toText (weaken v')
      return $ StmtDefineConst m dd t' v'
    StmtDefineResource m@(SavedHint m') name discarder copier -> do
      discarder' <- TM.inline m' discarder
      copier' <- TM.inline m' copier
      return $ StmtDefineResource m name discarder' copier'

insertStmt :: Stmt -> App ()
insertStmt stmt = do
  case stmt of
    StmtDefine _ stmtKind _ f _ xts _ e -> do
      Definition.insert (toOpacity stmtKind) f xts e
    StmtDefineConst _ dd _ v ->
      Definition.insert O.Clear dd [] v
    StmtDefineResource {} ->
      return ()
  insertWeakStmt $ weakenStmt stmt
  insertStmtKindInfo stmt

insertWeakStmt :: WeakStmt -> App ()
insertWeakStmt stmt = do
  case stmt of
    WeakStmtDefine _ stmtKind m f _ xts codType e -> do
      Type.insert f $ m :< WT.Pi xts codType
      WeakDefinition.insert (toOpacity stmtKind) m f xts e
    WeakStmtDefineConst m dd t v -> do
      Type.insert dd $ m :< WT.Pi [] t
      WeakDefinition.insert O.Clear m dd [] v
    WeakStmtDefineResource m name _ _ ->
      Type.insert name $ m :< WT.Tau

insertStmtKindInfo :: Stmt -> App ()
insertStmtKindInfo stmt = do
  case stmt of
    StmtDefine _ stmtKind _ _ _ _ _ _ -> do
      case stmtKind of
        Normal _ ->
          return ()
        Data dataName dataArgs consInfoList -> do
          DataDefinition.insert dataName dataArgs consInfoList
        DataIntro {} ->
          return ()
    StmtDefineConst {} ->
      return ()
    StmtDefineResource {} ->
      return ()

elaborateStmtKind :: StmtKind WT.WeakTerm -> App (StmtKind TM.Term)
elaborateStmtKind stmtKind =
  case stmtKind of
    Normal opacity ->
      return $ Normal opacity
    Data dataName dataArgs consInfoList -> do
      dataArgs' <- mapM elaborateWeakBinder dataArgs
      let (ms, consNameList, constLikeList, consArgsList, discriminantList) = unzip5 consInfoList
      consArgsList' <- mapM (mapM elaborateWeakBinder) consArgsList
      let consInfoList' = zip5 ms consNameList constLikeList consArgsList' discriminantList
      return $ Data dataName dataArgs' consInfoList'
    DataIntro dataName dataArgs consArgs discriminant -> do
      dataArgs' <- mapM elaborateWeakBinder dataArgs
      consArgs' <- mapM elaborateWeakBinder consArgs
      return $ DataIntro dataName dataArgs' consArgs' discriminant

elaborate' :: WT.WeakTerm -> App TM.Term
elaborate' term =
  case term of
    m :< WT.Tau ->
      return $ m :< TM.Tau
    m :< WT.Var x ->
      return $ m :< TM.Var x
    m :< WT.VarGlobal name argNum ->
      return $ m :< TM.VarGlobal name argNum
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
    m :< WT.Data attr name es -> do
      es' <- mapM elaborate' es
      return $ m :< TM.Data attr name es'
    m :< WT.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM elaborate' dataArgs
      consArgs' <- mapM elaborate' consArgs
      return $ m :< TM.DataIntro attr consName dataArgs' consArgs'
    m :< WT.DataElim isNoetic oets tree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM elaborate' es
      ts' <- mapM elaborate' ts
      tree' <- elaborateDecisionTree m tree
      when (DT.isUnreachable tree') $ do
        forM_ ts' $ \t -> do
          t' <- reduceType (weaken t)
          consList <- extractConstructorList m t'
          unless (null consList) $
            raiseNonExhaustivePatternMatching m
      return $ m :< TM.DataElim isNoetic (zip3 os es' ts') tree'
    m :< WT.Noema t -> do
      t' <- elaborate' t
      return $ m :< TM.Noema t'
    m :< WT.Embody t e -> do
      t' <- elaborate' t
      e' <- elaborate' e
      return $ m :< TM.Embody t' e'
    m :< WT.Let opacity (mx, x, t) e1 e2 -> do
      e1' <- elaborate' e1
      t' <- reduceType t
      e2' <- elaborate' e2
      return $ m :< TM.Let (WT.reifyOpacity opacity) (mx, x, t') e1' e2'
    m :< WT.Hole h es -> do
      fillHole m h es >>= elaborate'
    m :< WT.Prim prim ->
      case prim of
        WP.Type t ->
          return $ m :< TM.Prim (P.Type t)
        WP.Value primValue ->
          case primValue of
            WPV.Int t x -> do
              t' <- reduceWeakType t >>= elaborate'
              case t' of
                _ :< TM.Prim (P.Type (PT.Int size)) ->
                  return $ m :< TM.Prim (P.Value (PV.Int size x))
                _ :< TM.Prim (P.Type (PT.Float size)) ->
                  return $ m :< TM.Prim (P.Value (PV.Float size (fromInteger x)))
                _ -> do
                  Throw.raiseError m $
                    "the term `"
                      <> T.pack (show x)
                      <> "` is an integer, but its type is: "
                      <> toText (weaken t')
            WPV.Float t x -> do
              t' <- reduceWeakType t >>= elaborate'
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
            WPV.StaticText t text -> do
              t' <- elaborate' t
              return $ m :< TM.Prim (P.Value (PV.StaticText t' text))
    m :< WT.ResourceType name ->
      return $ m :< TM.ResourceType name
    m :< WT.Magic magic -> do
      case magic of
        M.External domList cod name args varArgs -> do
          let expected = length domList
          let actual = length args
          when (actual /= length domList) $ do
            Throw.raiseError m $
              "the external function `"
                <> EN.reify name
                <> "` expects "
                <> T.pack (show expected)
                <> " arguments, but found "
                <> T.pack (show actual)
                <> "."
          args' <- mapM elaborate' args
          varArgs' <- mapM (mapM elaborate') varArgs
          return $ m :< TM.Magic (M.External domList cod name args' varArgs')
        _ -> do
          magic' <- mapM elaborate' magic
          return $ m :< TM.Magic magic'
    m :< WT.Annotation remarkLevel annot e -> do
      e' <- elaborate' e
      case annot of
        AN.Type t -> do
          t' <- elaborate' t
          let message = "admitting `" <> toText (weaken t') <> "`"
          let typeRemark = Remark.newRemark m remarkLevel message
          Remark.insertRemark typeRemark
          return e'

elaborateWeakBinder :: BinderF WT.WeakTerm -> App (BinderF TM.Term)
elaborateWeakBinder (m, x, t) = do
  t' <- elaborate' t
  return (m, x, t')

elaborateKind :: LK.LamKindF WT.WeakTerm -> App (LK.LamKindF TM.Term)
elaborateKind kind =
  case kind of
    LK.Normal ->
      return LK.Normal
    LK.Fix xt -> do
      xt' <- elaborateWeakBinder xt
      return $ LK.Fix xt'

fillHole ::
  Hint ->
  HID.HoleID ->
  [WT.WeakTerm] ->
  App WT.WeakTerm
fillHole m h es = do
  holeSubst <- getHoleSubst
  case HS.lookup h holeSubst of
    Nothing ->
      Throw.raiseError m $ "couldn't instantiate the hole here: " <> T.pack (show h)
    Just (xs, e)
      | length xs == length es -> do
          let s = IntMap.fromList $ zip (map Ident.toInt xs) (map Right es)
          WT.subst s e
      | otherwise ->
          Throw.raiseError m "arity mismatch"

elaborateDecisionTree :: Hint -> DT.DecisionTree WT.WeakTerm -> App (DT.DecisionTree TM.Term)
elaborateDecisionTree m tree =
  case tree of
    DT.Leaf xs body -> do
      body' <- elaborate' body
      return $ DT.Leaf xs body'
    DT.Unreachable ->
      return DT.Unreachable
    DT.Switch (cursor, cursorType) (fallbackClause, clauseList) -> do
      cursorType' <- reduceWeakType cursorType >>= elaborate'
      consList <- extractConstructorList m cursorType'
      let activeConsList = DT.getConstructors clauseList
      let diff = S.difference (S.fromList consList) (S.fromList activeConsList)
      if S.size diff == 0
        then do
          clauseList' <- mapM elaborateClause clauseList
          return $ DT.Switch (cursor, cursorType') (DT.Unreachable, clauseList')
        else do
          case fallbackClause of
            DT.Unreachable ->
              raiseNonExhaustivePatternMatching m
            _ -> do
              fallbackClause' <- elaborateDecisionTree m fallbackClause
              clauseList' <- mapM elaborateClause clauseList
              return $ DT.Switch (cursor, cursorType') (fallbackClause', clauseList')

elaborateClause :: DT.Case WT.WeakTerm -> App (DT.Case TM.Term)
elaborateClause decisionCase = do
  let (dataTerms, dataTypes) = unzip $ DT.dataArgs decisionCase
  dataTerms' <- mapM elaborate' dataTerms
  dataTypes' <- mapM elaborate' dataTypes
  consArgs' <- mapM elaborateWeakBinder $ DT.consArgs decisionCase
  cont' <- elaborateDecisionTree (DT.mCons decisionCase) (DT.cont decisionCase)
  return $
    decisionCase
      { DT.dataArgs = zip dataTerms' dataTypes',
        DT.consArgs = consArgs',
        DT.cont = cont'
      }

raiseNonExhaustivePatternMatching :: Hint -> App a
raiseNonExhaustivePatternMatching m =
  Throw.raiseError m "encountered a non-exhaustive pattern matching"

reduceType :: WT.WeakTerm -> App TM.Term
reduceType e = do
  reduceWeakType e >>= elaborate'

reduceWeakType :: WT.WeakTerm -> App WT.WeakTerm
reduceWeakType e = do
  e' <- WT.reduce e
  case e' of
    m :< WT.Hole h es ->
      fillHole m h es >>= reduceWeakType
    m :< WT.PiElim (_ :< WT.VarGlobal _ name) args -> do
      mLam <- WeakDefinition.lookup name
      case mLam of
        Just lam ->
          reduceWeakType $ m :< WT.PiElim lam args
        Nothing -> do
          return e'
    _ ->
      return e'

extractConstructorList :: Hint -> TM.Term -> App [DD.DefiniteDescription]
extractConstructorList m cursorType = do
  case cursorType of
    _ :< TM.Data (AttrD.Attr {..}) _ _ -> do
      return consNameList
    _ ->
      Throw.raiseError m $ "the type of this term is expected to be an ADT, but it's not:\n" <> toText (weaken cursorType)
