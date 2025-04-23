module Move.Scene.Elaborate (elaborate, elaborate') where

import Control.Comonad.Cofree
import Control.Monad
import Data.Bifunctor
import Data.Bitraversable (bimapM)
import Data.List (unzip5, zip5)
import Data.Set qualified as S
import Data.Text qualified as T
import Move.Context.App
import Move.Context.Cache qualified as Cache
import Move.Context.DataDefinition qualified as DataDefinition
import Move.Context.Decl qualified as Decl
import Move.Context.Definition qualified as Definition
import Move.Context.EIO (EIO, raiseError, toApp)
import Move.Context.Elaborate
import Move.Context.Env qualified as Env
import Move.Context.Gensym qualified as Gensym
import Move.Context.KeyArg qualified as KeyArg
import Move.Context.Path qualified as Path
import Move.Context.RawImportSummary qualified as RawImportSummary
import Move.Context.Remark qualified as Remark
import Move.Context.SymLoc qualified as SymLoc
import Move.Context.Throw qualified as Throw
import Move.Context.TopCandidate qualified as TopCandidate
import Move.Context.Type qualified as Type
import Move.Context.WeakDefinition qualified as WeakDefinition
import Move.Scene.Elaborate.EnsureAffinity qualified as EnsureAffinity
import Move.Scene.Elaborate.Infer qualified as Infer
import Move.Scene.Elaborate.Unify qualified as Unify
import Move.Scene.Term.Inline qualified as TM
import Rule.Annotation qualified as AN
import Rule.Attr.Data qualified as AttrD
import Rule.Attr.Lam qualified as AttrL
import Rule.BaseLowType qualified as BLT
import Rule.BasePrimType qualified as BPT
import Rule.Binder
import Rule.Cache qualified as Cache
import Rule.Const (holeLiteral)
import Rule.DecisionTree qualified as DT
import Rule.DeclarationName qualified as DN
import Rule.DefiniteDescription qualified as DD
import Rule.Error qualified as E
import Rule.Foreign qualified as F
import Rule.Geist qualified as G
import Rule.Hint
import Rule.Ident
import Rule.Ident.Reify qualified as Ident
import Rule.IsConstLike (IsConstLike)
import Rule.LamKind qualified as LK
import Rule.Magic qualified as M
import Rule.Prim qualified as P
import Rule.PrimNumSize
import Rule.PrimType qualified as PT
import Rule.PrimValue qualified as PV
import Rule.Remark qualified as R
import Rule.Remark qualified as Remark
import Rule.Stmt
import Rule.StmtKind
import Rule.Target
import Rule.Term qualified as TM
import Rule.Term.Weaken
import Rule.WeakPrim qualified as WP
import Rule.WeakPrimValue qualified as WPV
import Rule.WeakTerm qualified as WT
import Rule.WeakTerm.ToText

elaborate :: Target -> Either Cache.Cache [WeakStmt] -> App [Stmt]
elaborate t cacheOrStmt = do
  initialize
  case cacheOrStmt of
    Left cache -> do
      let stmtList = Cache.stmtList cache
      forM_ stmtList insertStmt
      let remarkList = Cache.remarkList cache
      Remark.insertToGlobalRemarkList remarkList
      Gensym.setCount $ Cache.countSnapshot cache
      return stmtList
    Right stmtList -> do
      analyzeStmtList stmtList >>= synthesizeStmtList t

analyzeStmtList :: [WeakStmt] -> App [WeakStmt]
analyzeStmtList stmtList = do
  forM stmtList $ \stmt -> do
    h <- Infer.new
    stmt' <- toApp $ Infer.inferStmt h stmt
    insertWeakStmt stmt'
    return stmt'

synthesizeStmtList :: Target -> [WeakStmt] -> App [Stmt]
synthesizeStmtList t stmtList = do
  -- mapM_ viewStmt stmtList
  hUnify <- Unify.new
  getConstraintEnv >>= toApp . Unify.unify hUnify >>= setHoleSubst
  (stmtList', affineErrorList) <- bimap concat concat . unzip <$> mapM elaborateStmt stmtList
  unless (null affineErrorList) $ do
    Throw.throw $ E.MakeError affineErrorList
  -- mapM_ (viewStmt . weakenStmt) stmtList'
  source <- Env.getCurrentSource
  remarkList <- Remark.getRemarkList
  localVarTree <- SymLoc.get
  topCandidate <- TopCandidate.get
  rawImportSummary <- RawImportSummary.get
  countSnapshot <- Gensym.getCount
  h <- Path.new
  toApp $
    Cache.saveCache h t source $
      Cache.Cache
        { Cache.stmtList = stmtList',
          Cache.remarkList = remarkList,
          Cache.countSnapshot = countSnapshot
        }
  toApp $
    Cache.saveCompletionCache h t source $
      Cache.CompletionCache
        { Cache.localVarTree = localVarTree,
          Cache.topCandidate = topCandidate,
          Cache.rawImportSummary = rawImportSummary
        }
  Remark.insertToGlobalRemarkList remarkList
  return stmtList'

elaborateStmt :: WeakStmt -> App ([Stmt], [R.Remark])
elaborateStmt stmt = do
  case stmt of
    WeakStmtDefine isConstLike stmtKind m x impArgs expArgs codType e -> do
      stmtKind' <- elaborateStmtKind stmtKind
      e' <- elaborate' e
      impArgs' <- mapM elaborateWeakBinder impArgs
      expArgs' <- mapM elaborateWeakBinder expArgs
      codType' <- elaborate' codType
      let dummyAttr = AttrL.Attr {lamKind = LK.Normal codType', identity = 0}
      h <- EnsureAffinity.new
      remarks <- toApp $ EnsureAffinity.ensureAffinity h $ m :< TM.PiIntro dummyAttr impArgs' expArgs' e'
      e'' <- TM.inline m e'
      codType'' <- TM.inline m codType'
      when isConstLike $ do
        unless (TM.isValue e'') $ do
          Throw.raiseError m "Could not reduce the body of this definition into a constant"
      let result = StmtDefine isConstLike stmtKind' (SavedHint m) x impArgs' expArgs' codType'' e''
      insertStmt result
      return ([result], remarks)
    WeakStmtNominal _ geistList -> do
      mapM_ elaborateGeist geistList
      return ([], [])
    WeakStmtForeign foreignList -> do
      foreignList' <- forM foreignList $ \(F.Foreign m externalName domList cod) -> do
        domList' <- mapM strictify domList
        cod' <- mapM strictify cod
        return $ F.Foreign m externalName domList' cod'
      return ([StmtForeign foreignList'], [])

elaborateGeist :: G.Geist WT.WeakTerm -> App (G.Geist TM.Term)
elaborateGeist G.Geist {..} = do
  impArgs' <- mapM elaborateWeakBinder impArgs
  expArgs' <- mapM elaborateWeakBinder expArgs
  cod' <- elaborate' cod
  return $ G.Geist {impArgs = impArgs', expArgs = expArgs', cod = cod', ..}

insertStmt :: Stmt -> App ()
insertStmt stmt = do
  case stmt of
    StmtDefine _ stmtKind (SavedHint m) f impArgs expArgs t e -> do
      Type.insert f $ weaken $ m :< TM.Pi impArgs expArgs t
      Definition.insert (toOpacity stmtKind) f (impArgs ++ expArgs) e
    StmtForeign _ -> do
      return ()
  insertWeakStmt $ weakenStmt stmt
  insertStmtKindInfo stmt

insertWeakStmt :: WeakStmt -> App ()
insertWeakStmt stmt = do
  case stmt of
    WeakStmtDefine _ stmtKind m f impArgs expArgs codType e -> do
      WeakDefinition.insert (toOpacity stmtKind) m f impArgs expArgs codType e
    WeakStmtNominal {} -> do
      return ()
    WeakStmtForeign foreignList ->
      forM_ foreignList $ \(F.Foreign _ externalName domList cod) -> do
        domList' <- mapM (elaborate' >=> return . weaken) domList
        cod' <- mapM (elaborate' >=> return . weaken) cod
        Decl.insWeakDeclEnv (DN.Ext externalName) domList' cod'

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
    StmtForeign {} ->
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
    m :< WT.Pi impArgs expArgs t -> do
      impArgs' <- mapM elaborateWeakBinder impArgs
      expArgs' <- mapM elaborateWeakBinder expArgs
      t' <- elaborate' t
      return $ m :< TM.Pi impArgs' expArgs' t'
    m :< WT.PiIntro kind impArgs expArgs e -> do
      kind' <- elaborateLamAttr kind
      impArgs' <- mapM elaborateWeakBinder impArgs
      expArgs' <- mapM elaborateWeakBinder expArgs
      e' <- elaborate' e
      return $ m :< TM.PiIntro kind' impArgs' expArgs' e'
    m :< WT.PiElim e es -> do
      e' <- elaborate' e
      es' <- mapM elaborate' es
      return $ m :< TM.PiElim e' es'
    m :< WT.PiElimExact {} -> do
      Throw.raiseCritical m "Scene.Elaborate.elaborate': found a remaining `exact`"
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
      rootCursor <- Gensym.newIdentForHole
      let rootElem = (rootCursor, (Nothing, True, os))
      tree' <- elaborateDecisionTree [rootElem] m m tree
      when (DT.isUnreachable tree') $ do
        forM_ ts' $ \t -> do
          t' <- reduceType (weaken t)
          switchSpec <- getSwitchSpec m t'
          case switchSpec of
            LiteralSwitch -> do
              toApp $ raiseEmptyNonExhaustivePatternMatching m
            ConsSwitch consList -> do
              unless (null consList) $
                toApp $
                  raiseEmptyNonExhaustivePatternMatching m
      return $ m :< TM.DataElim isNoetic (zip3 os es' ts') tree'
    m :< WT.Box t -> do
      t' <- elaborate' t
      return $ m :< TM.Box t'
    m :< WT.BoxNoema t -> do
      t' <- elaborate' t
      return $ m :< TM.BoxNoema t'
    m :< WT.BoxIntro letSeq e -> do
      letSeq' <- mapM elaborateLet letSeq
      e' <- elaborate' e
      return $ m :< TM.BoxIntro letSeq' e'
    _ :< WT.BoxIntroQuote e -> do
      elaborate' e
    m :< WT.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      castSeq' <- mapM elaborateLet castSeq
      mxt' <- elaborateWeakBinder mxt
      e1' <- elaborate' e1
      uncastSeq' <- mapM elaborateLet uncastSeq
      e2' <- elaborate' e2
      return $ m :< TM.BoxElim castSeq' mxt' e1' uncastSeq' e2'
    _ :< WT.Actual e -> do
      elaborate' e
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
              (size, t') <- strictifyDecimalType m x t
              case size of
                Right intSize ->
                  return $ m :< TM.Prim (P.Value (PV.Int t' intSize x))
                Left floatSize ->
                  return $ m :< TM.Prim (P.Value (PV.Float t' floatSize (fromInteger x)))
            WPV.Float t x -> do
              (size, t') <- strictifyFloatType m x t
              return $ m :< TM.Prim (P.Value (PV.Float t' size x))
            WPV.Op op ->
              return $ m :< TM.Prim (P.Value (PV.Op op))
            WPV.StaticText t text -> do
              t' <- elaborate' t
              return $ m :< TM.Prim (P.Value (PV.StaticText t' text))
            WPV.Rune r ->
              return $ m :< TM.Prim (P.Value (PV.Rune r))
    m :< WT.Magic (M.WeakMagic magic) -> do
      case magic of
        M.External domList cod name args varArgs -> do
          domList' <- mapM strictify domList
          cod' <- mapM strictify cod
          args' <- mapM elaborate' args
          let (vArgs, vTypes) = unzip varArgs
          vArgs' <- mapM elaborate' vArgs
          vTypes' <- mapM strictify vTypes
          return $ m :< TM.Magic (M.External domList' cod' name args' (zip vArgs' vTypes'))
        M.Cast from to value -> do
          from' <- elaborate' from
          to' <- elaborate' to
          value' <- elaborate' value
          return $ m :< TM.Magic (M.Cast from' to' value')
        M.Store t unit value pointer -> do
          t' <- strictify t
          unit' <- elaborate' unit
          value' <- elaborate' value
          pointer' <- elaborate' pointer
          return $ m :< TM.Magic (M.Store t' unit' value' pointer')
        M.Load t pointer -> do
          t' <- strictify t
          pointer' <- elaborate' pointer
          return $ m :< TM.Magic (M.Load t' pointer')
        M.Alloca t size -> do
          t' <- strictify t
          size' <- elaborate' size
          return $ m :< TM.Magic (M.Alloca t' size')
        M.Global name t -> do
          t' <- strictify t
          return $ m :< TM.Magic (M.Global name t')
        M.OpaqueValue e -> do
          e' <- elaborate' e
          return $ m :< TM.Magic (M.OpaqueValue e')
    m :< WT.Annotation remarkLevel annot e -> do
      e' <- elaborate' e
      case annot of
        AN.Type t -> do
          t' <- elaborate' t
          let message = "Admitted: `" <> toText (weaken t') <> "`"
          let typeRemark = Remark.newRemark m remarkLevel message
          Remark.insertRemark typeRemark
          return e'
    m :< WT.Resource dd resourceID unitType discarder copier -> do
      unitType' <- elaborate' unitType
      discarder' <- elaborate' discarder
      copier' <- elaborate' copier
      return $ m :< TM.Resource dd resourceID unitType' discarder' copier'
    m :< WT.Use {} -> do
      Throw.raiseCritical m "Scene.Elaborate.elaborate': found a remaining `use`"
    m :< WT.Void ->
      return $ m :< TM.Void

strictify :: WT.WeakTerm -> App BLT.BaseLowType
strictify t@(mt :< _) =
  strictify' mt t

strictify' :: Hint -> WT.WeakTerm -> App BLT.BaseLowType
strictify' m t = do
  t' <- reduceWeakType t >>= elaborate'
  case t' of
    _ :< TM.Prim (P.Type (PT.Int size)) ->
      return $ BLT.PrimNum $ BPT.Int $ BPT.Explicit size
    _ :< TM.Prim (P.Type (PT.Float size)) ->
      return $ BLT.PrimNum $ BPT.Float $ BPT.Explicit size
    _ :< TM.Prim (P.Type PT.Pointer) ->
      return BLT.Pointer
    _ :< TM.Data (AttrD.Attr {consNameList = [(consName, _)]}) _ [] -> do
      consType <- Type.lookup m consName
      case consType of
        _ :< WT.Pi impArgs expArgs _
          | [(_, _, arg)] <- impArgs ++ expArgs -> do
              strictify' m arg
        _ ->
          raiseNonStrictType m (weaken t')
    _ :< _ ->
      raiseNonStrictType m (weaken t')

raiseNonStrictType :: Hint -> WT.WeakTerm -> App a
raiseNonStrictType m t = do
  Throw.raiseError m $
    "Expected:\n  an integer, a float, or a pointer\nFound:\n  "
      <> toText t

strictifyDecimalType :: Hint -> Integer -> WT.WeakTerm -> App (Either FloatSize IntSize, TM.Term)
strictifyDecimalType m x t = do
  t' <- reduceWeakType t >>= elaborate'
  case t' of
    _ :< TM.Prim (P.Type (PT.Int size)) ->
      return (Right size, t')
    _ :< TM.Prim (P.Type (PT.Float size)) ->
      return (Left size, t')
    _ :< TM.Data (AttrD.Attr {consNameList = [(consName, _)]}) _ [] -> do
      consType <- Type.lookup m consName
      case consType of
        _ :< WT.Pi impArgs expArgs _
          | [(_, _, arg)] <- impArgs ++ expArgs -> do
              strictifyDecimalType m x arg
        _ ->
          raiseNonDecimalType m x (weaken t')
    _ :< _ ->
      raiseNonDecimalType m x (weaken t')

raiseNonDecimalType :: Hint -> Integer -> WT.WeakTerm -> App a
raiseNonDecimalType m x t = do
  Throw.raiseError m $
    "The term `"
      <> T.pack (show x)
      <> "` is an integer, but its type is: "
      <> toText t

strictifyFloatType :: Hint -> Double -> WT.WeakTerm -> App (FloatSize, TM.Term)
strictifyFloatType m x t = do
  t' <- reduceWeakType t >>= elaborate'
  case t' of
    _ :< TM.Prim (P.Type (PT.Float size)) ->
      return (size, t')
    _ :< TM.Data (AttrD.Attr {consNameList = [(consName, _)]}) _ [] -> do
      consType <- Type.lookup m consName
      case consType of
        _ :< WT.Pi impArgs expArgs _
          | [(_, _, arg)] <- impArgs ++ expArgs -> do
              strictifyFloatType m x arg
        _ ->
          raiseNonFloatType m x (weaken t')
    _ :< _ ->
      raiseNonFloatType m x (weaken t')

raiseNonFloatType :: Hint -> Double -> WT.WeakTerm -> App a
raiseNonFloatType m x t = do
  Throw.raiseError m $
    "The term `"
      <> T.pack (show x)
      <> "` is a float, but its type is: "
      <> toText t

elaborateWeakBinder :: BinderF WT.WeakTerm -> App (BinderF TM.Term)
elaborateWeakBinder (m, x, t) = do
  t' <- elaborate' t
  return (m, x, t')

elaborateLet :: (BinderF WT.WeakTerm, WT.WeakTerm) -> App (BinderF TM.Term, TM.Term)
elaborateLet (xt, e) = do
  xt' <- elaborateWeakBinder xt
  e' <- elaborate' e
  return (xt', e')

elaborateLamAttr :: AttrL.Attr WT.WeakTerm -> App (AttrL.Attr TM.Term)
elaborateLamAttr (AttrL.Attr {lamKind, identity}) =
  case lamKind of
    LK.Normal codType -> do
      codType' <- elaborate' codType
      return $ AttrL.Attr {lamKind = LK.Normal codType', identity}
    LK.Fix xt -> do
      xt' <- elaborateWeakBinder xt
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

makeTree :: Hint -> ClauseContext -> App (Ident, PatternTree)
makeTree m ctx =
  case ctx of
    [] ->
      Throw.raiseCritical m "Scene.Elaborate.makeTree: invalid argument (empty context)"
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
  ClauseContext ->
  Hint ->
  Hint ->
  DT.DecisionTree WT.WeakTerm ->
  App (DT.DecisionTree TM.Term)
elaborateDecisionTree ctx mOrig m tree =
  case tree of
    DT.Leaf xs letSeq body -> do
      letSeq' <- mapM (bimapM elaborateWeakBinder elaborate') letSeq
      body' <- elaborate' body
      return $ DT.Leaf xs letSeq' body'
    DT.Unreachable ->
      return DT.Unreachable
    DT.Switch (cursor, cursorType) (fallbackClause, clauseList) -> do
      cursorType' <- reduceWeakType cursorType >>= elaborate'
      switchSpec <- getSwitchSpec m cursorType'
      case switchSpec of
        LiteralSwitch -> do
          when (DT.isUnreachable fallbackClause) $ do
            toApp $ raiseLiteralNonExhaustivePatternMatching m
          fallbackClause' <- elaborateDecisionTree ctx mOrig m fallbackClause
          clauseList' <- mapM (elaborateClause mOrig cursor ctx) clauseList
          return $ DT.Switch (cursor, cursorType') (fallbackClause', clauseList')
        ConsSwitch consList -> do
          let activeConsList = DT.getConstructors clauseList
          let diff = S.difference (S.fromList consList) (S.fromList activeConsList)
          if S.size diff == 0
            then do
              clauseList' <- mapM (elaborateClause mOrig cursor ctx) clauseList
              return $ DT.Switch (cursor, cursorType') (DT.Unreachable, clauseList')
            else do
              case fallbackClause of
                DT.Unreachable -> do
                  (rootIdent, tBase) <- makeTree mOrig ctx
                  uncoveredPatterns <- forM (S.toList diff) $ \(consDD, isConstLike) -> do
                    h <- KeyArg.new
                    (_, keys) <- toApp $ KeyArg.lookup h m consDD
                    let expArgNum = length keys
                    let args = map (const (holeIdent, Node (Just holeLiteral) True [])) [1 .. expArgNum]
                    let tBase' = graft cursor (Node (Just $ DD.localLocator consDD) isConstLike args) tBase
                    return $ patternTreeToText $ suppress (rootIdent, tBase')
                  let uncoveredPatterns' = T.concat $ flip map uncoveredPatterns $ \ex -> do
                        "| " <> ex <> " => ...\n"
                  Throw.raiseError mOrig $
                    "This pattern matching does not cover the following:\n" <> uncoveredPatterns'
                _ -> do
                  fallbackClause' <- elaborateDecisionTree ctx mOrig m fallbackClause
                  clauseList' <- mapM (elaborateClause mOrig cursor ctx) clauseList
                  return $ DT.Switch (cursor, cursorType') (fallbackClause', clauseList')

elaborateClause :: Hint -> Ident -> ClauseContext -> DT.Case WT.WeakTerm -> App (DT.Case TM.Term)
elaborateClause mOrig cursor ctx decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      cont' <- elaborateDecisionTree ctx mOrig mPat cont
      return $ DT.LiteralCase mPat i cont'
    DT.ConsCase record@DT.ConsCaseRecord {..} -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      dataTerms' <- mapM elaborate' dataTerms
      dataTypes' <- mapM elaborate' dataTypes
      consArgs' <- mapM elaborateWeakBinder consArgs
      let consArgIdents = map (\(_, x, _) -> x) consArgs
      let consContext = (cursor, (Just consDD, isConstLike, consArgIdents))
      cont' <- elaborateDecisionTree (consContext : ctx) mOrig mCons cont
      return $
        DT.ConsCase
          record
            { DT.dataArgs = zip dataTerms' dataTypes',
              DT.consArgs = consArgs',
              DT.cont = cont'
            }

raiseLiteralNonExhaustivePatternMatching :: Hint -> EIO a
raiseLiteralNonExhaustivePatternMatching m =
  raiseError m "Pattern matching on literals must have a fallback clause"

raiseEmptyNonExhaustivePatternMatching :: Hint -> EIO a
raiseEmptyNonExhaustivePatternMatching m =
  raiseError m "Empty pattern matching can only be performed on empty ADT values"

reduceType :: WT.WeakTerm -> App TM.Term
reduceType e = do
  reduceWeakType e >>= elaborate'

data SwitchSpec
  = LiteralSwitch
  | ConsSwitch [(DD.DefiniteDescription, IsConstLike)]

getSwitchSpec :: Hint -> TM.Term -> App SwitchSpec
getSwitchSpec m cursorType = do
  case cursorType of
    _ :< TM.Data (AttrD.Attr {..}) _ _ -> do
      return $ ConsSwitch consNameList
    _ :< TM.Prim (P.Type (PT.Int _)) -> do
      return LiteralSwitch
    _ :< TM.Prim (P.Type PT.Rune) -> do
      return LiteralSwitch
    _ ->
      Throw.raiseError m $
        "This term is expected to be an ADT value or a literal, but found:\n"
          <> toText (weaken cursorType)
