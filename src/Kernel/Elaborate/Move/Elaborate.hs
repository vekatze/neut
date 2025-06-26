module Kernel.Elaborate.Move.Elaborate
  ( getWeakTypeEnv,
    elaborate,
    elaborate',
  )
where

import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Bitraversable (bimapM)
import Data.IntMap qualified as IntMap
import Data.List (unzip5, zip5)
import Data.Set qualified as S
import Data.Text qualified as T
import Error.Move.Run (raiseCritical, raiseError)
import Error.Rule.EIO (EIO)
import Error.Rule.Error qualified as E
import Gensym.Move.Trick qualified as Gensym
import Kernel.Common.Move.Handle.Global.GlobalRemark qualified as GlobalRemark
import Kernel.Common.Move.Handle.Global.KeyArg qualified as KeyArg
import Kernel.Common.Move.Handle.Global.Type qualified as Type
import Kernel.Common.Move.ManageCache qualified as Cache
import Kernel.Common.Rule.Cache qualified as Cache
import Kernel.Common.Rule.Const (holeLiteral)
import Kernel.Common.Rule.Target hiding (Main)
import Kernel.Elaborate.Move.Internal.EnsureAffinity qualified as EnsureAffinity
import Kernel.Elaborate.Move.Internal.Handle.Constraint qualified as Constraint
import Kernel.Elaborate.Move.Internal.Handle.Def qualified as Definition
import Kernel.Elaborate.Move.Internal.Handle.Elaborate
import Kernel.Elaborate.Move.Internal.Handle.Hole qualified as Hole
import Kernel.Elaborate.Move.Internal.Handle.LocalLogs qualified as LocalLogs
import Kernel.Elaborate.Move.Internal.Handle.WeakDecl qualified as WeakDecl
import Kernel.Elaborate.Move.Internal.Handle.WeakDef qualified as WeakDef
import Kernel.Elaborate.Move.Internal.Handle.WeakType qualified as WeakType
import Kernel.Elaborate.Move.Internal.Infer qualified as Infer
import Kernel.Elaborate.Move.Internal.Unify qualified as Unify
import Kernel.Elaborate.Rule.HoleSubst qualified as HS
import Language.Common.Move.CreateSymbol qualified as Gensym
import Language.Common.Rule.Annotation qualified as AN
import Language.Common.Rule.Attr.Data qualified as AttrD
import Language.Common.Rule.Attr.Lam qualified as AttrL
import Language.Common.Rule.BaseLowType qualified as BLT
import Language.Common.Rule.BasePrimType qualified as BPT
import Language.Common.Rule.Binder
import Language.Common.Rule.DecisionTree qualified as DT
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.Foreign qualified as F
import Language.Common.Rule.Geist qualified as G
import Language.Common.Rule.HoleID qualified as HID
import Language.Common.Rule.Ident
import Language.Common.Rule.Ident.Reify qualified as Ident
import Language.Common.Rule.ImpArgs qualified as ImpArgs
import Language.Common.Rule.IsConstLike (IsConstLike)
import Language.Common.Rule.LamKind qualified as LK
import Language.Common.Rule.Magic qualified as M
import Language.Common.Rule.PiKind qualified as PK
import Language.Common.Rule.PrimNumSize
import Language.Common.Rule.PrimType qualified as PT
import Language.Common.Rule.StmtKind
import Language.Common.Rule.StmtKind qualified as SK
import Language.LowComp.Rule.DeclarationName qualified as DN
import Language.Term.Rule.Prim qualified as P
import Language.Term.Rule.PrimValue qualified as PV
import Language.Term.Rule.Stmt
import Language.Term.Rule.Term qualified as TM
import Language.Term.Rule.Term.Weaken
import Language.WeakTerm.Move.Subst qualified as Subst
import Language.WeakTerm.Rule.WeakPrim qualified as WP
import Language.WeakTerm.Rule.WeakPrimValue qualified as WPV
import Language.WeakTerm.Rule.WeakStmt
import Language.WeakTerm.Rule.WeakTerm qualified as WT
import Language.WeakTerm.Rule.WeakTerm.ToText
import Logger.Rule.Hint
import Logger.Rule.Log qualified as L

getWeakTypeEnv :: Handle -> IO WeakType.WeakTypeEnv
getWeakTypeEnv h =
  WeakType.get $ weakTypeHandle h

elaborate :: Handle -> Target -> [L.Log] -> Either Cache.Cache [WeakStmt] -> EIO [Stmt]
elaborate h t logs cacheOrStmt = do
  case cacheOrStmt of
    Left cache -> do
      let stmtList = Cache.stmtList cache
      forM_ stmtList $ insertStmt h
      liftIO $ GlobalRemark.insert (globalRemarkHandle h) logs
      liftIO $ Gensym.setCount (gensymHandle h) $ Cache.countSnapshot cache
      return stmtList
    Right stmtList -> do
      analyzeStmtList h stmtList >>= synthesizeStmtList h t logs

analyzeStmtList :: Handle -> [WeakStmt] -> EIO [WeakStmt]
analyzeStmtList h stmtList = do
  forM stmtList $ \stmt -> do
    stmt' <- Infer.inferStmt h stmt
    insertWeakStmt h stmt'
    return stmt'

synthesizeStmtList :: Handle -> Target -> [L.Log] -> [WeakStmt] -> EIO [Stmt]
synthesizeStmtList h t logs stmtList = do
  -- mapM_ viewStmt stmtList
  liftIO (Constraint.get (constraintHandle h)) >>= Unify.unify h >>= liftIO . Hole.setSubst (holeHandle h)
  (stmtList', affineErrorList) <- bimap concat concat . unzip <$> mapM (elaborateStmt h) stmtList
  unless (null affineErrorList) $ do
    throwError $ E.MakeError affineErrorList
  -- mapM_ (liftIO . viewStmt . weakenStmt) stmtList'
  countSnapshot <- liftIO $ Gensym.getCount (gensymHandle h)
  localLogs <- liftIO $ LocalLogs.get (localLogsHandle h)
  let logs' = logs ++ localLogs
  Cache.saveCache (pathHandle h) t (currentSource h) $
    Cache.Cache
      { Cache.stmtList = stmtList',
        Cache.remarkList = logs',
        Cache.countSnapshot = countSnapshot
      }
  liftIO $ GlobalRemark.insert (globalRemarkHandle h) logs'
  return stmtList'

elaborateStmt :: Handle -> WeakStmt -> EIO ([Stmt], [L.Log])
elaborateStmt h stmt = do
  case stmt of
    WeakStmtDefine isConstLike stmtKind m x impArgs expArgs codType e -> do
      stmtKind' <- elaborateStmtKind h stmtKind
      e' <- elaborate' h e
      impArgs' <- mapM (elaborateWeakBinderWithMaybeType h) impArgs
      expArgs' <- mapM (elaborateWeakBinder h) expArgs
      codType' <- elaborate' h codType
      let dummyAttr = AttrL.Attr {lamKind = LK.Normal Nothing codType', identity = 0}
      remarks <- do
        affHandle <- liftIO $ EnsureAffinity.new h
        EnsureAffinity.ensureAffinity affHandle $ m :< TM.PiIntro dummyAttr impArgs' expArgs' e'
      e'' <- inline h m e'
      impArgs'' <- mapM (inlineBinderWithMaybeType h) impArgs'
      expArgs'' <- mapM (inlineBinder h) expArgs'
      codType'' <- inline h m codType'
      when isConstLike $ do
        unless (TM.isValue e'') $ do
          raiseError m "Could not reduce the body of this definition into a constant"
      let result = StmtDefine isConstLike stmtKind' (SavedHint m) x impArgs'' expArgs'' codType'' e''
      insertStmt h result
      return ([result], remarks)
    WeakStmtNominal _ geistList -> do
      mapM_ (elaborateGeist h) geistList
      return ([], [])
    WeakStmtForeign foreignList -> do
      foreignList' <- forM foreignList $ \(F.Foreign m externalName domList cod) -> do
        domList' <- mapM (strictify h) domList
        cod' <- mapM (strictify h) cod
        return $ F.Foreign m externalName domList' cod'
      return ([StmtForeign foreignList'], [])

elaborateGeist :: Handle -> G.Geist WT.WeakTerm -> EIO (G.Geist TM.Term)
elaborateGeist h (G.Geist {..}) = do
  impArgs' <-
    mapM
      ( \(binder, maybeType) -> do
          (binder', maybeType') <- elaborateWeakBinderWithMaybeType h (binder, maybeType)
          return (binder', maybeType')
      )
      impArgs
  expArgs' <- mapM (elaborateWeakBinder h) expArgs
  cod' <- elaborate' h cod
  return $ G.Geist {impArgs = impArgs', expArgs = expArgs', cod = cod', ..}

insertStmt :: Handle -> Stmt -> EIO ()
insertStmt h stmt = do
  case stmt of
    StmtDefine isConstLike stmtKind (SavedHint m) f impArgs expArgs t e -> do
      let impArgsWithDefaults = impArgs
      case stmtKind of
        SK.DataIntro {} ->
          liftIO $ Type.insert' (typeHandle h) f $ weaken $ m :< TM.Pi (PK.DataIntro isConstLike) impArgsWithDefaults expArgs t
        _ ->
          liftIO $ Type.insert' (typeHandle h) f $ weaken $ m :< TM.Pi (PK.Normal isConstLike) impArgsWithDefaults expArgs t
      liftIO $ Definition.insert' (defHandle h) (toOpacity stmtKind) f (map fst impArgs ++ expArgs) e
    StmtForeign _ -> do
      return ()
  insertWeakStmt h $ weakenStmt stmt

insertWeakStmt :: Handle -> WeakStmt -> EIO ()
insertWeakStmt h stmt = do
  case stmt of
    WeakStmtDefine _ stmtKind m f impArgs expArgs codType e -> do
      liftIO $ WeakDef.insert' (weakDefHandle h) (toOpacity stmtKind) m f (map fst impArgs) expArgs codType e
    WeakStmtNominal {} -> do
      return ()
    WeakStmtForeign foreignList ->
      forM_ foreignList $ \(F.Foreign _ externalName domList cod) -> do
        domList' <- mapM (elaborate' h >=> return . weaken) domList
        cod' <- mapM (elaborate' h >=> return . weaken) cod
        liftIO $ WeakDecl.insert (weakDeclHandle h) (DN.Ext externalName) domList' cod'

elaborateStmtKind :: Handle -> StmtKind WT.WeakTerm -> EIO (StmtKind TM.Term)
elaborateStmtKind h stmtKind =
  case stmtKind of
    Normal opacity ->
      return $ Normal opacity
    Main opacity t -> do
      t' <- elaborate' h t
      return $ Main opacity t'
    Data dataName dataArgs consInfoList -> do
      dataArgs' <- mapM (elaborateWeakBinder h) dataArgs
      let (ms, consNameList, constLikeList, consArgsList, discriminantList) = unzip5 consInfoList
      consArgsList' <- mapM (mapM $ elaborateWeakBinder h) consArgsList
      let consInfoList' = zip5 ms consNameList constLikeList consArgsList' discriminantList
      return $ Data dataName dataArgs' consInfoList'
    DataIntro dataName dataArgs expConsArgs discriminant -> do
      dataArgs' <- mapM (elaborateWeakBinder h) dataArgs
      expConsArgs' <- mapM (elaborateWeakBinder h) expConsArgs
      return $ DataIntro dataName dataArgs' expConsArgs' discriminant

elaborate' :: Handle -> WT.WeakTerm -> EIO TM.Term
elaborate' h term =
  case term of
    m :< WT.Tau ->
      return $ m :< TM.Tau
    m :< WT.Var x ->
      return $ m :< TM.Var x
    m :< WT.VarGlobal name argNum ->
      return $ m :< TM.VarGlobal name argNum
    m :< WT.Pi piKind impArgs expArgs t -> do
      impArgs' <- forM impArgs $ \(binder, maybeType) -> do
        binder' <- elaborateWeakBinder h binder
        maybeType' <- traverse (elaborate' h) maybeType
        return (binder', maybeType')
      expArgs' <- mapM (elaborateWeakBinder h) expArgs
      t' <- elaborate' h t
      return $ m :< TM.Pi piKind impArgs' expArgs' t'
    m :< WT.PiIntro kind impArgs expArgs e -> do
      kind' <- elaborateLamAttr h kind
      impArgs' <- mapM (elaborateWeakBinderWithMaybeType h) impArgs
      expArgs' <- mapM (elaborateWeakBinder h) expArgs
      e' <- elaborate' h e
      return $ m :< TM.PiIntro kind' impArgs' expArgs' e'
    m :< WT.PiElim b e impArgs expArgs -> do
      e' <- elaborate' h e
      let impArgs' = ImpArgs.extract impArgs
      impArgs'' <- mapM (elaborate' h) impArgs'
      expArgs' <- mapM (elaborate' h) expArgs
      return $ m :< TM.PiElim b e' impArgs'' expArgs'
    m :< WT.PiElimExact {} -> do
      raiseCritical m "Scene.Elaborate.elaborate': found a remaining `exact`"
    m :< WT.Data attr name es -> do
      es' <- mapM (elaborate' h) es
      return $ m :< TM.Data attr name es'
    m :< WT.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (elaborate' h) dataArgs
      consArgs' <- mapM (elaborate' h) consArgs
      return $ m :< TM.DataIntro attr consName dataArgs' consArgs'
    m :< WT.DataElim isNoetic oets tree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (elaborate' h) es
      ts' <- mapM (elaborate' h) ts
      rootCursor <- liftIO $ Gensym.newIdentForHole (gensymHandle h)
      let rootElem = (rootCursor, (Nothing, True, os))
      tree' <- elaborateDecisionTree h [rootElem] m m tree
      when (DT.isUnreachable tree') $ do
        forM_ ts' $ \t -> do
          t' <- reduceType h (weaken t)
          switchSpec <- getSwitchSpec m t'
          case switchSpec of
            LiteralSwitch -> do
              raiseEmptyNonExhaustivePatternMatching m
            ConsSwitch consList -> do
              unless (null consList) $
                raiseEmptyNonExhaustivePatternMatching m
      return $ m :< TM.DataElim isNoetic (zip3 os es' ts') tree'
    m :< WT.Box t -> do
      t' <- elaborate' h t
      return $ m :< TM.Box t'
    m :< WT.BoxNoema t -> do
      t' <- elaborate' h t
      return $ m :< TM.BoxNoema t'
    m :< WT.BoxIntro letSeq e -> do
      letSeq' <- mapM (elaborateLet h) letSeq
      e' <- elaborate' h e
      return $ m :< TM.BoxIntro letSeq' e'
    _ :< WT.BoxIntroQuote e -> do
      elaborate' h e
    m :< WT.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      castSeq' <- mapM (elaborateLet h) castSeq
      mxt' <- elaborateWeakBinder h mxt
      e1' <- elaborate' h e1
      uncastSeq' <- mapM (elaborateLet h) uncastSeq
      e2' <- elaborate' h e2
      return $ m :< TM.BoxElim castSeq' mxt' e1' uncastSeq' e2'
    _ :< WT.Actual e -> do
      elaborate' h e
    m :< WT.Let opacity (mx, x, t) e1 e2 -> do
      e1' <- elaborate' h e1
      t' <- reduceType h t
      e2' <- elaborate' h e2
      return $ m :< TM.Let (WT.reifyOpacity opacity) (mx, x, t') e1' e2'
    m :< WT.Hole hole es -> do
      fillHole h m hole es >>= elaborate' h
    m :< WT.Prim prim ->
      case prim of
        WP.Type t ->
          return $ m :< TM.Prim (P.Type t)
        WP.Value primValue ->
          case primValue of
            WPV.Int t x -> do
              (size, t') <- strictifyDecimalType h m x t
              case size of
                Right intSize ->
                  return $ m :< TM.Prim (P.Value (PV.Int t' intSize x))
                Left floatSize ->
                  return $ m :< TM.Prim (P.Value (PV.Float t' floatSize (fromInteger x)))
            WPV.Float t x -> do
              (size, t') <- strictifyFloatType h m x t
              return $ m :< TM.Prim (P.Value (PV.Float t' size x))
            WPV.Op op ->
              return $ m :< TM.Prim (P.Value (PV.Op op))
            WPV.StaticText t text -> do
              t' <- elaborate' h t
              return $ m :< TM.Prim (P.Value (PV.StaticText t' text))
            WPV.Rune r ->
              return $ m :< TM.Prim (P.Value (PV.Rune r))
    m :< WT.Magic (M.WeakMagic magic) -> do
      case magic of
        M.External domList cod name args varArgs -> do
          domList' <- mapM (strictify h) domList
          cod' <- mapM (strictify h) cod
          args' <- mapM (elaborate' h) args
          let (vArgs, vTypes) = unzip varArgs
          vArgs' <- mapM (elaborate' h) vArgs
          vTypes' <- mapM (strictify h) vTypes
          return $ m :< TM.Magic (M.External domList' cod' name args' (zip vArgs' vTypes'))
        M.Cast from to value -> do
          from' <- elaborate' h from
          to' <- elaborate' h to
          value' <- elaborate' h value
          return $ m :< TM.Magic (M.Cast from' to' value')
        M.Store t unit value pointer -> do
          t' <- strictify h t
          unit' <- elaborate' h unit
          value' <- elaborate' h value
          pointer' <- elaborate' h pointer
          return $ m :< TM.Magic (M.Store t' unit' value' pointer')
        M.Load t pointer -> do
          t' <- strictify h t
          pointer' <- elaborate' h pointer
          return $ m :< TM.Magic (M.Load t' pointer')
        M.Alloca t size -> do
          t' <- strictify h t
          size' <- elaborate' h size
          return $ m :< TM.Magic (M.Alloca t' size')
        M.Global name t -> do
          t' <- strictify h t
          return $ m :< TM.Magic (M.Global name t')
        M.OpaqueValue e -> do
          e' <- elaborate' h e
          return $ m :< TM.Magic (M.OpaqueValue e')
        M.CallType func arg1 arg2 -> do
          func' <- elaborate' h func
          arg1' <- elaborate' h arg1
          arg2' <- elaborate' h arg2
          return $ m :< TM.Magic (M.CallType func' arg1' arg2')
    m :< WT.Annotation remarkLevel annot e -> do
      e' <- elaborate' h e
      case annot of
        AN.Type t -> do
          t' <- elaborate' h t
          let message = "Admitted: `" <> toText (weaken t') <> "`"
          let typeRemark = L.newLog m remarkLevel message
          liftIO $ LocalLogs.insert (localLogsHandle h) typeRemark
          return e'
    m :< WT.Resource dd resourceID unitType discarder copier typeTag -> do
      unitType' <- elaborate' h unitType
      discarder' <- elaborate' h discarder
      copier' <- elaborate' h copier
      typeTag' <- elaborate' h typeTag
      return $ m :< TM.Resource dd resourceID unitType' discarder' copier' typeTag'
    m :< WT.Void ->
      return $ m :< TM.Void

strictify :: Handle -> WT.WeakTerm -> EIO BLT.BaseLowType
strictify h t@(mt :< _) =
  strictify' h mt t

strictify' :: Handle -> Hint -> WT.WeakTerm -> EIO BLT.BaseLowType
strictify' h m t = do
  t' <- reduceWeakType h t >>= elaborate' h
  case t' of
    _ :< TM.Prim (P.Type (PT.Int size)) ->
      return $ BLT.PrimNum $ BPT.Int $ BPT.Explicit size
    _ :< TM.Prim (P.Type (PT.Float size)) ->
      return $ BLT.PrimNum $ BPT.Float $ BPT.Explicit size
    _ :< TM.Prim (P.Type PT.Pointer) ->
      return BLT.Pointer
    _ :< TM.Data (AttrD.Attr {consNameList = [(consName, _)]}) _ [] -> do
      consType <- Type.lookup' (typeHandle h) m consName
      case consType of
        _ :< WT.Pi (PK.DataIntro False) _ [] (_ :< WT.Pi _ impArgs expArgs _)
          | [(_, _, arg)] <- map fst impArgs ++ expArgs -> do
              strictify' h m arg
        _ ->
          raiseNonStrictType m consType
    _ :< _ ->
      raiseNonStrictType m (weaken t')

strictifyDecimalType :: Handle -> Hint -> Integer -> WT.WeakTerm -> EIO (Either FloatSize IntSize, TM.Term)
strictifyDecimalType h m x t = do
  t' <- reduceWeakType h t >>= elaborate' h
  case t' of
    _ :< TM.Prim (P.Type (PT.Int size)) ->
      return (Right size, t')
    _ :< TM.Prim (P.Type (PT.Float size)) ->
      return (Left size, t')
    _ :< TM.Data (AttrD.Attr {consNameList = [(consName, _)]}) _ [] -> do
      consType <- Type.lookup' (typeHandle h) m consName
      case consType of
        _ :< WT.Pi (PK.DataIntro False) _ [] (_ :< WT.Pi _ impArgs expArgs _)
          | [(_, _, arg)] <- map fst impArgs ++ expArgs -> do
              strictifyDecimalType h m x arg
        _ ->
          raiseNonDecimalType m x (weaken t')
    _ :< _ ->
      raiseNonDecimalType m x (weaken t')

strictifyFloatType :: Handle -> Hint -> Double -> WT.WeakTerm -> EIO (FloatSize, TM.Term)
strictifyFloatType h m x t = do
  t' <- reduceWeakType h t >>= elaborate' h
  case t' of
    _ :< TM.Prim (P.Type (PT.Float size)) ->
      return (size, t')
    _ :< TM.Data (AttrD.Attr {consNameList = [(consName, _)]}) _ [] -> do
      consType <- Type.lookup' (typeHandle h) m consName
      case consType of
        _ :< WT.Pi (PK.DataIntro False) _ [] (_ :< WT.Pi _ impArgs expArgs _)
          | [(_, _, arg)] <- map fst impArgs ++ expArgs -> do
              strictifyFloatType h m x arg
        _ ->
          raiseNonFloatType m x (weaken t')
    _ :< _ ->
      raiseNonFloatType m x (weaken t')

elaborateWeakBinder :: Handle -> BinderF WT.WeakTerm -> EIO (BinderF TM.Term)
elaborateWeakBinder h (m, x, t) = do
  t' <- elaborate' h t
  return (m, x, t')

elaborateWeakBinderWithMaybeType :: Handle -> (BinderF WT.WeakTerm, Maybe WT.WeakTerm) -> EIO (BinderF TM.Term, Maybe TM.Term)
elaborateWeakBinderWithMaybeType h ((m, x, t), maybeType) = do
  t' <- elaborate' h t
  maybeType' <- traverse (elaborate' h) maybeType
  return ((m, x, t'), maybeType')

inlineBinderWithMaybeType :: Handle -> (BinderF TM.Term, Maybe TM.Term) -> EIO (BinderF TM.Term, Maybe TM.Term)
inlineBinderWithMaybeType h ((m, x, t), maybeType) = do
  t' <- inline h m t
  maybeType' <- traverse (inline h m) maybeType
  return ((m, x, t'), maybeType')

elaborateLet :: Handle -> (BinderF WT.WeakTerm, WT.WeakTerm) -> EIO (BinderF TM.Term, TM.Term)
elaborateLet h (xt, e) = do
  xt' <- elaborateWeakBinder h xt
  e' <- elaborate' h e
  return (xt', e')

elaborateLamAttr :: Handle -> AttrL.Attr WT.WeakTerm -> EIO (AttrL.Attr TM.Term)
elaborateLamAttr h (AttrL.Attr {lamKind, identity}) =
  case lamKind of
    LK.Normal name codType -> do
      codType' <- elaborate' h codType
      return $ AttrL.Attr {lamKind = LK.Normal name codType', identity}
    LK.Fix xt -> do
      xt' <- elaborateWeakBinder h xt
      return $ AttrL.Attr {lamKind = LK.Fix xt', identity}

type ClauseContext =
  [(Ident, (Maybe DD.DefiniteDescription, IsConstLike, [Ident]))]

data PatternTree
  = Leaf
  | Node (Maybe T.Text) IsConstLike [(Ident, PatternTree)]
  deriving (Show)

patternTreeToText :: (Ident, PatternTree) -> T.Text
patternTreeToText (x, t) =
  case t of
    Leaf ->
      Ident.toText x
    Node mFunc isConstLike args -> do
      case mFunc of
        Nothing -> do
          T.intercalate ", " (map patternTreeToText args)
        Just func ->
          if isConstLike
            then func
            else func <> "(" <> T.intercalate ", " (map patternTreeToText args) <> ")"

suppress :: (Ident, PatternTree) -> (Ident, PatternTree)
suppress (x, tree) = do
  case tree of
    Leaf ->
      (suppress' x, Leaf)
    Node dd isConstLike args -> do
      (suppress' x, Node dd isConstLike (map suppress args))

suppress' :: Ident -> Ident
suppress' (I (_, i)) =
  I (holeLiteral, i)

makeTree :: Hint -> ClauseContext -> EIO (Ident, PatternTree)
makeTree m ctx =
  case ctx of
    [] ->
      raiseCritical m "Scene.Elaborate.makeTree: invalid argument (empty context)"
    [(v, (mDD, isConstLike, args))] ->
      return (v, Node (DD.localLocator <$> mDD) isConstLike (map (,Leaf) args))
    (v, (dd, isConstLike, args)) : rest -> do
      (v', t) <- makeTree m rest
      return (v', graft v (Node (DD.localLocator <$> dd) isConstLike (map (,Leaf) args)) t)

graft :: Ident -> PatternTree -> PatternTree -> PatternTree
graft from to tree =
  case tree of
    Leaf ->
      Leaf
    Node dd isConstLike children -> do
      Node dd isConstLike $ flip map children $ \(x, t) -> do
        if x == from
          then (x, to)
          else (x, graft from to t)

holeIdent :: Ident
holeIdent =
  I (holeLiteral, 0)

elaborateDecisionTree ::
  Handle ->
  ClauseContext ->
  Hint ->
  Hint ->
  DT.DecisionTree WT.WeakTerm ->
  EIO (DT.DecisionTree TM.Term)
elaborateDecisionTree h ctx mOrig m tree =
  case tree of
    DT.Leaf xs letSeq body -> do
      letSeq' <- mapM (bimapM (elaborateWeakBinder h) (elaborate' h)) letSeq
      body' <- elaborate' h body
      return $ DT.Leaf xs letSeq' body'
    DT.Unreachable ->
      return DT.Unreachable
    DT.Switch (cursor, cursorType) (fallbackClause, clauseList) -> do
      cursorType' <- reduceWeakType h cursorType >>= elaborate' h
      switchSpec <- getSwitchSpec m cursorType'
      case switchSpec of
        LiteralSwitch -> do
          when (DT.isUnreachable fallbackClause) $ do
            raiseLiteralNonExhaustivePatternMatching m
          fallbackClause' <- elaborateDecisionTree h ctx mOrig m fallbackClause
          clauseList' <- mapM (elaborateClause h mOrig cursor ctx) clauseList
          return $ DT.Switch (cursor, cursorType') (fallbackClause', clauseList')
        ConsSwitch consList -> do
          let activeConsList = DT.getConstructors clauseList
          let diff = S.difference (S.fromList consList) (S.fromList activeConsList)
          if S.size diff == 0
            then do
              clauseList' <- mapM (elaborateClause h mOrig cursor ctx) clauseList
              return $ DT.Switch (cursor, cursorType') (DT.Unreachable, clauseList')
            else do
              case fallbackClause of
                DT.Unreachable -> do
                  (rootIdent, tBase) <- makeTree mOrig ctx
                  uncoveredPatterns <- forM (S.toList diff) $ \(consDD, isConstLike) -> do
                    (_, keys) <- KeyArg.lookup (keyArgHandle h) m consDD
                    let expArgNum = length keys
                    let args = map (const (holeIdent, Node (Just holeLiteral) True [])) [1 .. expArgNum]
                    let tBase' = graft cursor (Node (Just $ DD.localLocator consDD) isConstLike args) tBase
                    return $ patternTreeToText $ suppress (rootIdent, tBase')
                  let uncoveredPatterns' = T.concat $ flip map uncoveredPatterns $ \ex -> do
                        "| " <> ex <> " => ...\n"
                  raiseError mOrig $
                    "This pattern matching does not cover the following:\n" <> uncoveredPatterns'
                _ -> do
                  fallbackClause' <- elaborateDecisionTree h ctx mOrig m fallbackClause
                  clauseList' <- mapM (elaborateClause h mOrig cursor ctx) clauseList
                  return $ DT.Switch (cursor, cursorType') (fallbackClause', clauseList')

elaborateClause :: Handle -> Hint -> Ident -> ClauseContext -> DT.Case WT.WeakTerm -> EIO (DT.Case TM.Term)
elaborateClause h mOrig cursor ctx decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      cont' <- elaborateDecisionTree h ctx mOrig mPat cont
      return $ DT.LiteralCase mPat i cont'
    DT.ConsCase record@DT.ConsCaseRecord {..} -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      dataTerms' <- mapM (elaborate' h) dataTerms
      dataTypes' <- mapM (elaborate' h) dataTypes
      consArgs' <- mapM (elaborateWeakBinder h) consArgs
      let consArgIdents = map (\(_, x, _) -> x) consArgs
      let consContext = (cursor, (Just consDD, isConstLike, consArgIdents))
      cont' <- elaborateDecisionTree h (consContext : ctx) mOrig mCons cont
      return $
        DT.ConsCase
          record
            { DT.dataArgs = zip dataTerms' dataTypes',
              DT.consArgs = consArgs',
              DT.cont = cont'
            }

raiseNonStrictType :: Hint -> WT.WeakTerm -> EIO a
raiseNonStrictType m t = do
  raiseError m $
    "Expected:\n  an integer, a float, or a pointer\nFound:\n  "
      <> toText t

raiseNonDecimalType :: Hint -> Integer -> WT.WeakTerm -> EIO a
raiseNonDecimalType m x t = do
  raiseError m $
    "The term `"
      <> T.pack (show x)
      <> "` is an integer, but its type is: "
      <> toText t

raiseNonFloatType :: Hint -> Double -> WT.WeakTerm -> EIO a
raiseNonFloatType m x t = do
  raiseError m $
    "The term `"
      <> T.pack (show x)
      <> "` is a float, but its type is: "
      <> toText t

raiseLiteralNonExhaustivePatternMatching :: Hint -> EIO a
raiseLiteralNonExhaustivePatternMatching m =
  raiseError m "Pattern matching on literals must have a fallback clause"

raiseEmptyNonExhaustivePatternMatching :: Hint -> EIO a
raiseEmptyNonExhaustivePatternMatching m =
  raiseError m "Empty pattern matching can only be performed on empty ADT values"

reduceType :: Handle -> WT.WeakTerm -> EIO TM.Term
reduceType h e = do
  reduceWeakType h e >>= elaborate' h

data SwitchSpec
  = LiteralSwitch
  | ConsSwitch [(DD.DefiniteDescription, IsConstLike)]

getSwitchSpec :: Hint -> TM.Term -> EIO SwitchSpec
getSwitchSpec m cursorType = do
  case cursorType of
    _ :< TM.Data (AttrD.Attr {..}) _ _ -> do
      return $ ConsSwitch consNameList
    _ :< TM.Prim (P.Type (PT.Int _)) -> do
      return LiteralSwitch
    _ :< TM.Prim (P.Type PT.Rune) -> do
      return LiteralSwitch
    _ ->
      raiseError m $
        "This term is expected to be an ADT value or a literal, but found:\n"
          <> toText (weaken cursorType)

reduceWeakType :: Handle -> WT.WeakTerm -> EIO WT.WeakTerm
reduceWeakType h e = do
  e' <- reduce h e
  case e' of
    m :< WT.Hole hole es ->
      fillHole h m hole es >>= reduceWeakType h
    m :< WT.PiElim False (_ :< WT.VarGlobal _ name) impArgs args -> do
      mLam <- liftIO $ WeakDef.lookup' (weakDefHandle h) name
      case mLam of
        Just lam ->
          reduceWeakType h $ m :< WT.PiElim False lam impArgs args
        Nothing -> do
          return e'
    _ ->
      return e'

fillHole ::
  Handle ->
  Hint ->
  HID.HoleID ->
  [WT.WeakTerm] ->
  EIO WT.WeakTerm
fillHole h m holeID es = do
  holeSubst <- liftIO $ Hole.getSubst (holeHandle h)
  case HS.lookup holeID holeSubst of
    Nothing ->
      raiseError m $ "Could not instantiate the hole here: " <> T.pack (show holeID)
    Just (xs, e)
      | length xs == length es -> do
          let s = IntMap.fromList $ zip (map Ident.toInt xs) (map Right es)
          liftIO $ Subst.subst (substHandle h) s e
      | otherwise ->
          raiseError m "Arity mismatch"

-- viewStmt :: WeakStmt -> IO ()
-- viewStmt stmt = do
--   case stmt of
--     WeakStmtDefine _ _ m x impArgs expArgs codType e -> do
--       let attr = AttrL.Attr {lamKind = LK.Normal Nothing codType, identity = 0}
--       putStrLn $ T.unpack $ DD.reify x <> "\n" <> toText (m :< WT.Pi impArgs expArgs codType) <> "\n" <> toText (m :< WT.PiIntro attr impArgs expArgs e)
--     _ ->
--       return ()
