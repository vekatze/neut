module Scene.Elaborate (elaborate) where

import Context.App
import Context.Cache qualified as Cache
import Context.DataDefinition qualified as DataDefinition
import Context.Definition qualified as Definition
import Context.Elaborate
import Context.Env qualified as Env
import Context.Global qualified as Global
import Context.Locator qualified as Locator
import Context.Throw qualified as Throw
import Context.Type qualified as Type
import Context.WeakDefinition qualified as WeakDefinition
import Control.Comonad.Cofree
import Control.Monad
import Data.IntMap qualified as IntMap
import Data.List
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.Binder
import Entity.DecisionTree qualified as DT
import Entity.DefiniteDescription qualified as DD
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.HoleID qualified as HID
import Entity.HoleSubst qualified as HS
import Entity.Ident.Reify qualified as Ident
import Entity.LamKind qualified as LK
import Entity.Prim qualified as P
import Entity.PrimType qualified as PT
import Entity.PrimValue qualified as PV
import Entity.Stmt
import Entity.Term qualified as TM
import Entity.Term.Weaken
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Entity.WeakTerm qualified as WT
import Entity.WeakTerm.ToText
import Scene.Elaborate.Infer qualified as Infer
import Scene.Elaborate.Reveal qualified as Reveal
import Scene.Elaborate.Unify qualified as Unify
import Scene.Term.Reduce qualified as Term
import Scene.WeakTerm.Reduce qualified as WT
import Scene.WeakTerm.Subst qualified as WT

elaborate :: Either [Stmt] [WeakStmt] -> App [Stmt]
elaborate cacheOrStmt = do
  initialize
  case cacheOrStmt of
    Left cache -> do
      forM_ cache insertStmt
      return cache
    Right defList -> do
      (analyzeDefList >=> synthesizeDefList) defList

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
--     WeakStmtDefine _ m x _ xts codType e ->
--       Log.printNote m $ DD.reify x <> "\n" <> toText (m :< WT.Pi xts codType) <> "\n" <> toText (m :< WT.Pi xts e)
--     WeakStmtDefineResource m name discarder copier ->
--       Log.printNote m $ "define-resource" <> DD.reify name <> "\n" <> toText discarder <> toText copier

synthesizeDefList :: [WeakStmt] -> App [Stmt]
synthesizeDefList defList = do
  -- mapM_ viewStmt defList
  getConstraintEnv >>= Unify.unify >>= setHoleSubst
  defList' <- mapM elaborateStmt defList
  -- mapM_ (viewStmt . weakenStmt) defList'
  source <- Env.getCurrentSource
  Cache.saveCache (source, defList')
  return defList'

elaborateStmt :: WeakStmt -> App Stmt
elaborateStmt stmt = do
  case stmt of
    WeakStmtDefine isConstLike stmtKind m x impArgNum xts codType e -> do
      stmtKind' <- elaborateStmtKind stmtKind
      e' <- elaborate' e >>= Term.reduce
      xts' <- mapM elaborateWeakBinder xts
      codType' <- elaborate' codType >>= Term.reduce
      Type.insert x $ weaken $ m :< TM.Pi xts' codType'
      let result = StmtDefine isConstLike stmtKind' m x impArgNum xts' codType' e'
      insertStmt result
      return result
    WeakStmtDefineResource m name discarder copier -> do
      discarder' <- elaborate' discarder
      copier' <- elaborate' copier
      let result = StmtDefineResource m name discarder' copier'
      insertStmt result
      return result

insertStmt :: Stmt -> App ()
insertStmt stmt = do
  case stmt of
    StmtDefine _ stmtKind m f _ xts _ e -> do
      let lamKind = LK.Normal $ toOpacity stmtKind
      Definition.insert (toOpacity stmtKind) f (m :< TM.PiIntro lamKind xts e)
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
    StmtDefineResource {} ->
      return ()

elaborateStmtKind :: StmtKindF WT.WeakTerm -> App (StmtKindF TM.Term)
elaborateStmtKind stmtKind =
  case stmtKind of
    Normal opacity ->
      return $ Normal opacity
    Data dataName dataArgs consInfoList -> do
      dataArgs' <- mapM elaborateWeakBinder dataArgs
      let (consNameList, constLikeList, consArgsList, discriminantList) = unzip4 consInfoList
      consArgsList' <- mapM (mapM elaborateWeakBinder) consArgsList
      return $ Data dataName dataArgs' $ zip4 consNameList constLikeList consArgsList' discriminantList
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
    m :< WT.Embody t e -> do
      t' <- elaborate' t
      e' <- elaborate' e
      return $ m :< TM.Embody t' e'
    m :< WT.Cell t -> do
      t' <- elaborate' t
      return $ m :< TM.Cell t'
    m :< WT.CellIntro e -> do
      e' <- elaborate' e
      return $ m :< TM.CellIntro e'
    m :< WT.CellElim e -> do
      e' <- elaborate' e
      return $ m :< TM.CellElim e'
    m :< WT.Let opacity (mx, x, t) e1 e2 -> do
      e1' <- elaborate' e1
      t' <- reduceType t
      e2' <- elaborate' e2
      case opacity of
        WT.Noetic ->
          ensureTypeLucidity m t' t'
        _ ->
          return ()
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
              t' <- elaborate' t >>= Term.reduce
              case t' of
                _ :< TM.Prim (P.Type (PT.Int size)) ->
                  return $ m :< TM.Prim (P.Value (PV.Int size x))
                _ :< TM.Prim (P.Type (PT.UInt size)) ->
                  return $ m :< TM.Prim (P.Value (PV.UInt size x))
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

elaborateWeakBinder :: BinderF WT.WeakTerm -> App (BinderF TM.Term)
elaborateWeakBinder (m, x, t) = do
  t' <- elaborate' t
  return (m, x, t')

elaborateKind :: LK.LamKindF WT.WeakTerm -> App (LK.LamKindF TM.Term)
elaborateKind kind =
  case kind of
    LK.Normal opacity ->
      return $ LK.Normal opacity
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
      Throw.raiseError m "couldn't instantiate the hole here"
    Just (xs, e)
      | length xs == length es -> do
          let s = IntMap.fromList $ zip (map Ident.toInt xs) (map Right es)
          WT.subst s e
      | otherwise ->
          Throw.raiseError m "arity mismatch"

-- cs <- readIORef constraintEnv
-- p "==========================================================="
-- forM_ cs $ \(e1, e2) -> do
--   p $ T.unpack $ toText e1
--   p $ T.unpack $ toText e2
--   p "---------------------"

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
              Throw.raiseError m "encountered a non-exhaustive pattern matching"
            _ -> do
              fallbackClause' <- elaborateDecisionTree m fallbackClause
              clauseList' <- mapM elaborateClause clauseList
              return $ DT.Switch (cursor, cursorType') (fallbackClause', clauseList')

elaborateClause :: DT.Case WT.WeakTerm -> App (DT.Case TM.Term)
elaborateClause (DT.Cons mCons consName disc dataArgs consArgs cont) = do
  let (dataTerms, dataTypes) = unzip dataArgs
  dataTerms' <- mapM elaborate' dataTerms
  dataTypes' <- mapM elaborate' dataTypes
  consArgs' <- mapM elaborateWeakBinder consArgs
  cont' <- elaborateDecisionTree mCons cont
  return $ DT.Cons mCons consName disc (zip dataTerms' dataTypes') consArgs' cont'

reduceType :: WT.WeakTerm -> App TM.Term
reduceType e = do
  reduceWeakType e >>= elaborate'

reduceWeakType :: WT.WeakTerm -> App WT.WeakTerm
reduceWeakType e = do
  e' <- WT.reduce e
  case e' of
    m :< WT.Hole h es ->
      fillHole m h es >>= reduceWeakType
    m :< WT.PiElim (_ :< WT.VarGlobal name _) args -> do
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
    _ :< TM.Data dataName _ -> do
      getConstructorList m dataName
    _ ->
      Throw.raiseError m $ "the type of this term is expected to be an ADT, but it's not:\n" <> toText (weaken cursorType)

getConstructorList :: Hint -> DD.DefiniteDescription -> App [DD.DefiniteDescription]
getConstructorList m dataName = do
  kind <- Global.lookup dataName
  case kind of
    Just (GN.Data _ consList _) ->
      return consList
    _ -> do
      Throw.raiseCritical m $ "the datatype `" <> DD.reify dataName <> "` isn't defined"

ensureTypeLucidity :: Hint -> TM.Term -> TM.Term -> App ()
ensureTypeLucidity m orig t = do
  case t of
    _ :< TM.Tau ->
      return ()
    _ :< TM.Var _ ->
      return () -- opaque type variable
    _ :< TM.Data dataName dataArgs -> do
      ensureDataLucidity m orig dataName
      dataArgs' <- mapM (reduceType . weaken) dataArgs
      forM_ dataArgs' $ ensureTypeLucidity m orig
    _ :< TM.Prim (P.Type {}) ->
      return ()
    _ :< TM.ResourceType _ ->
      return ()
    _ ->
      Throw.raiseError m $ "the answer type of let-on must be lucid, but it's not:\n" <> toText (weaken orig)

ensureDataLucidity :: Hint -> TM.Term -> DD.DefiniteDescription -> App ()
ensureDataLucidity m orig dataName = do
  consList <- getConstructorList m dataName
  mLucidConsList <- mapM (getNonLucidConsName m) consList
  case catMaybes mLucidConsList of
    [] ->
      return ()
    lucidConsNames ->
      Throw.raiseError m $
        "the `"
          <> showGlobalVariable dataName
          <> "` in `"
          <> toText (weaken orig)
          <> "` contains the following non-lucid constructors:\n"
          <> showNonLucidConstructorList lucidConsNames

getNonLucidConsName ::
  Hint ->
  DD.DefiniteDescription ->
  App (Maybe DD.DefiniteDescription)
getNonLucidConsName m consName = do
  t <- Type.lookup m consName >>= reduceType
  case t of
    _ :< TM.Pi xts _ -> do
      isLucidArgFlagList <- forM xts $ \(_, _, dom) -> do
        reduceType (weaken dom) >>= isLucidArgument
      if and isLucidArgFlagList
        then return Nothing
        else return $ Just consName
    _ ->
      Throw.raiseCritical m $ "the type of a constructor must be a Π-type, but it's not:\n" <> toText (weaken t)

isLucidArgument :: TM.Term -> App Bool
isLucidArgument t = do
  case t of
    _ :< TM.Pi {} -> do
      return False
    _ ->
      return True

showNonLucidConstructorList :: [DD.DefiniteDescription] -> T.Text
showNonLucidConstructorList ddList =
  case ddList of
    [] ->
      ""
    [d] ->
      "- " <> showGlobalVariable d
    d : rest ->
      "- " <> showGlobalVariable d <> "\n" <> showNonLucidConstructorList rest
