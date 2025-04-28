{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list comprehension" #-}

module Move.Scene.Elaborate.Infer
  ( Handle,
    new,
    inferStmt,
  )
where

import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.HashMap.Strict qualified as Map
import Data.IntMap qualified as IntMap
import Data.Text qualified as T
import Move.Context.App
import Move.Context.EIO (EIO, raiseCritical, raiseError)
import Move.Context.Elaborate qualified as Elaborate
import Move.Context.Env qualified as Env
import Move.Context.KeyArg qualified as KeyArg
import Move.Context.Locator qualified as Locator
import Move.Context.OptimizableData qualified as OptimizableData
import Move.Context.Type qualified as Type
import Move.Context.WeakDefinition qualified as WeakDefinition
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Elaborate.Handle.Constraint qualified as Constraint
import Move.Scene.Elaborate.Handle.Hole qualified as Hole
import Move.Scene.Elaborate.Handle.WeakDecl qualified as WeakDecl
import Move.Scene.Elaborate.Handle.WeakType qualified as WeakType
import Move.Scene.Elaborate.Unify qualified as Unify
import Move.Scene.Parse.Discern.Handle qualified as Discern
import Move.Scene.Parse.Discern.Name qualified as N
import Move.Scene.WeakTerm.Reduce qualified as Reduce
import Move.Scene.WeakTerm.Subst qualified as Subst
import Move.Scene.WeakTerm.Subst qualified as WT
import Rule.Annotation qualified as Annotation
import Rule.ArgNum qualified as AN
import Rule.Attr.Data qualified as AttrD
import Rule.Attr.DataIntro qualified as AttrDI
import Rule.Attr.Lam qualified as AttrL
import Rule.Attr.VarGlobal qualified as AttrVG
import Rule.Binder
import Rule.Const
import Rule.DecisionTree qualified as DT
import Rule.DeclarationName qualified as DN
import Rule.DefiniteDescription qualified as DD
import Rule.Discriminant qualified as D
import Rule.ForeignCodType qualified as FCT
import Rule.Geist qualified as G
import Rule.Hint
import Rule.HoleID qualified as HID
import Rule.HoleSubst qualified as HS
import Rule.Ident (Ident (..), isHole)
import Rule.Ident.Reify qualified as Ident
import Rule.Key (Key)
import Rule.LamKind qualified as LK
import Rule.Literal qualified as L
import Rule.Magic qualified as M
import Rule.Name qualified as N
import Rule.OptimizableData qualified as OD
import Rule.PrimOp
import Rule.PrimType qualified as PT
import Rule.Stmt
import Rule.StmtKind
import Rule.Term qualified as TM
import Rule.Term.FromPrimNum qualified as Term
import Rule.Term.Weaken
import Rule.WeakPrim qualified as WP
import Rule.WeakPrimValue qualified as WPV
import Rule.WeakTerm qualified as WT
import Rule.WeakTerm.ToText (toText)

type BoundVarEnv = [BinderF WT.WeakTerm]

data Handle
  = Handle
  { envHandle :: Env.Handle,
    substHandle :: Subst.Handle,
    reduceHandle :: Reduce.Handle,
    unifyHandle :: Unify.Handle,
    gensymHandle :: Gensym.Handle,
    discernHandle :: Discern.Handle,
    constraintHandle :: Constraint.Handle,
    weakTypeHandle :: WeakType.Handle,
    weakDeclHandle :: WeakDecl.Handle,
    weakDefHandle :: WeakDefinition.Handle,
    keyArgHandle :: KeyArg.Handle,
    holeHandle :: Hole.Handle,
    typeHandle :: Type.Handle,
    optDataHandle :: OptimizableData.Handle,
    varEnv :: BoundVarEnv
  }

new :: Elaborate.HandleEnv -> Env.Handle -> Gensym.Handle -> App Handle
new handleEnv@(Elaborate.HandleEnv {..}) envHandle gensymHandle = do
  substHandle <- Subst.new gensymHandle
  reduceHandle <- Reduce.new gensymHandle
  unifyHandle <- Unify.new handleEnv envHandle gensymHandle
  discernHandle <- Discern.new gensymHandle
  keyArgHandle <- KeyArg.new
  weakDeclHandle <- WeakDecl.new
  optDataHandle <- OptimizableData.new
  typeHandle <- Type.new
  weakDefHandle <- WeakDefinition.new gensymHandle
  let varEnv = []
  return Handle {..}

inferStmt :: Handle -> WeakStmt -> EIO WeakStmt
inferStmt h stmt =
  case stmt of
    WeakStmtDefine isConstLike stmtKind m x impArgs expArgs codType e -> do
      (impArgs', h') <- inferBinder' h impArgs
      (expArgs', h'') <- inferBinder' h' expArgs
      codType' <- inferType h'' codType
      liftIO $ insertType h'' x $ m :< WT.Pi impArgs' expArgs' codType'
      stmtKind' <- inferStmtKind h'' stmtKind
      (e', te) <- infer h'' e
      liftIO $ Constraint.insert (constraintHandle h'') codType' te
      when (DD.isEntryPoint x) $ do
        let _m = m {metaShouldSaveLocation = False}
        unitType <- getUnitType h'' _m
        liftIO $ Constraint.insert (constraintHandle h'') (m :< WT.Pi [] [] unitType) (m :< WT.Pi impArgs' expArgs' codType')
      return $ WeakStmtDefine isConstLike stmtKind' m x impArgs' expArgs' codType' e'
    WeakStmtNominal m geistList -> do
      geistList' <- mapM (inferGeist h) geistList
      return $ WeakStmtNominal m geistList'
    WeakStmtForeign foreignList ->
      return $ WeakStmtForeign foreignList

inferGeist :: Handle -> G.Geist WT.WeakTerm -> EIO (G.Geist WT.WeakTerm)
inferGeist h (G.Geist {..}) = do
  (impArgs', h') <- inferBinder' h impArgs
  (expArgs', h'') <- inferBinder' h' expArgs
  cod' <- inferType h'' cod
  liftIO $ insertType h'' name $ loc :< WT.Pi impArgs' expArgs' cod'
  return $ G.Geist {impArgs = impArgs', expArgs = expArgs', cod = cod', ..}

insertType :: Handle -> DD.DefiniteDescription -> WT.WeakTerm -> IO ()
insertType h dd t = do
  typeOrNone <- Type.lookupMaybe' (typeHandle h) dd
  case typeOrNone of
    Nothing ->
      return ()
    Just declaredType -> do
      Constraint.insert (constraintHandle h) declaredType t
  Type.insert' (typeHandle h) dd t

inferStmtKind :: Handle -> StmtKind WT.WeakTerm -> EIO (StmtKind WT.WeakTerm)
inferStmtKind h stmtKind =
  case stmtKind of
    Normal {} ->
      return stmtKind
    Data dataName dataArgs consInfoList -> do
      (dataArgs', varEnv) <- inferBinder' h dataArgs
      consInfoList' <- forM consInfoList $ \(m, dd, constLike, consArgs, discriminant) -> do
        (consArgs', _) <- inferBinder' varEnv consArgs
        return (m, dd, constLike, consArgs', discriminant)
      return $ Data dataName dataArgs' consInfoList'
    DataIntro consName dataArgs consArgs discriminant -> do
      (dataArgs', varEnv) <- inferBinder' h dataArgs
      (consArgs', _) <- inferBinder' varEnv consArgs
      return $ DataIntro consName dataArgs' consArgs' discriminant

getIntType :: Hint -> EIO WT.WeakTerm
getIntType m = do
  baseSize <- Env.getBaseSize m
  return $ WT.intTypeBySize m baseSize

getUnitType :: Handle -> Hint -> EIO WT.WeakTerm
getUnitType h m = do
  locator <- liftEither $ DD.getLocatorPair m coreUnit
  (unitDD, _) <- N.resolveName (discernHandle h) m (N.Locator locator)
  let attr = AttrVG.Attr {argNum = AN.fromInt 0, isConstLike = True}
  return $ m :< WT.piElim (m :< WT.VarGlobal attr unitDD) []

extendHandle :: BinderF WT.WeakTerm -> Handle -> Handle
extendHandle (mx, x, t) h = do
  if isHole x then h else h {varEnv = (mx, x, t) : varEnv h}

extendHandle' :: BinderF WT.WeakTerm -> Handle -> Handle
extendHandle' (mx, x, t) h = do
  h {varEnv = (mx, x, t) : varEnv h}

infer :: Handle -> WT.WeakTerm -> EIO (WT.WeakTerm, WT.WeakTerm)
infer h term =
  case term of
    _ :< WT.Tau ->
      return (term, term)
    m :< WT.Var x -> do
      _ :< t <- WeakType.lookup (weakTypeHandle h) m x
      return (term, m :< t)
    m :< WT.VarGlobal _ name -> do
      _ :< t <- Type.lookup' (typeHandle h) m name
      return (term, m :< t)
    m :< WT.Pi impArgs expArgs t -> do
      (impArgs', h') <- inferPiBinder h impArgs
      (expArgs', h'') <- inferPiBinder h' expArgs
      t' <- inferType h'' t
      return (m :< WT.Pi impArgs' expArgs' t', m :< WT.Tau)
    m :< WT.PiIntro attr@(AttrL.Attr {lamKind}) impArgs expArgs e -> do
      case lamKind of
        LK.Fix (mx, x, codType) -> do
          (impArgs', h') <- inferBinder' h impArgs
          (expArgs', h'') <- inferBinder' h' expArgs
          codType' <- inferType h'' codType
          let piType = m :< WT.Pi impArgs' expArgs' codType'
          liftIO $ WeakType.insert (weakTypeHandle h) x piType
          (e', tBody) <- infer h'' e
          liftIO $ Constraint.insert (constraintHandle h'') codType' tBody
          let term' = m :< WT.PiIntro (attr {AttrL.lamKind = LK.Fix (mx, x, codType')}) impArgs' expArgs' e'
          return (term', piType)
        LK.Normal codType -> do
          (impArgs', h') <- inferBinder' h impArgs
          (expArgs', h'') <- inferBinder' h' expArgs
          codType' <- inferType h'' codType
          (e', t') <- infer h'' e
          liftIO $ Constraint.insert (constraintHandle h'') codType' t'
          let term' = m :< WT.PiIntro (attr {AttrL.lamKind = LK.Normal codType'}) impArgs' expArgs' e'
          return (term', m :< WT.Pi impArgs' expArgs' t')
    m :< WT.PiElim e es -> do
      etl <- infer h e
      etls <- mapM (infer h) es
      inferPiElim h m etl etls
    m :< WT.PiElimExact e -> do
      (e', t) <- infer h e
      t' <- resolveType h t
      case t' of
        _ :< WT.Pi impArgs expArgs codType -> do
          holes <- liftIO $ mapM (const $ newHole h m $ varEnv h) impArgs
          let sub = IntMap.fromList $ zip (map (\(_, x, _) -> Ident.toInt x) impArgs) (map Right holes)
          (expArgs', _) <- liftIO $ Subst.subst' (substHandle h) sub expArgs
          let expArgs'' = map (\(_, x, _) -> m :< WT.Var x) expArgs'
          codType' <- liftIO $ Subst.subst (substHandle h) sub codType
          lamID <- liftIO $ Gensym.newCount (gensymHandle h)
          infer h $ m :< WT.PiIntro (AttrL.normal lamID codType') [] expArgs' (m :< WT.PiElim e' expArgs'')
        _ ->
          raiseError m $ "Expected a function type, but got: " <> toText t'
    m :< WT.Data attr name es -> do
      (es', _) <- mapAndUnzipM (infer h) es
      return (m :< WT.Data attr name es', m :< WT.Tau)
    m :< WT.DataIntro attr@(AttrDI.Attr {..}) consName dataArgs consArgs -> do
      (dataArgs', _) <- mapAndUnzipM (infer h) dataArgs
      (consArgs', _) <- mapAndUnzipM (infer h) consArgs
      let dataType = m :< WT.Data (AttrD.Attr {..}) dataName dataArgs'
      return (m :< WT.DataIntro attr consName dataArgs' consArgs', dataType)
    m :< WT.DataElim isNoetic oets tree -> do
      let (os, es, _) = unzip3 oets
      (es', ts') <- mapAndUnzipM (infer h) es
      liftIO $ forM_ (zip os ts') $ uncurry $ WeakType.insert (weakTypeHandle h)
      (tree', treeType) <- inferDecisionTree m h tree
      return (m :< WT.DataElim isNoetic (zip3 os es' ts') tree', treeType)
    m :< WT.Box t -> do
      t' <- inferType h t
      return (m :< WT.Box t', m :< WT.Tau)
    m :< WT.BoxNoema t -> do
      t' <- inferType h t
      return (m :< WT.BoxNoema t', m :< WT.Tau)
    m :< WT.BoxIntro letSeq e -> do
      (letSeq', h') <- inferQuoteSeq h letSeq FromNoema
      (e', t) <- infer h' e
      return (m :< WT.BoxIntro letSeq' e', m :< WT.Box t)
    m :< WT.BoxIntroQuote e -> do
      (e', t) <- infer h e
      liftIO $ Constraint.insertActualityConstraint (constraintHandle h) t
      return (m :< WT.BoxIntroQuote e', m :< WT.Box t)
    m :< WT.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      (castSeq', h1) <- inferQuoteSeq h castSeq ToNoema
      (e1', t1) <- infer h1 e1
      (mxt'@(mx, _, t1'), h2) <- inferBinder1 h1 mxt
      liftIO $ Constraint.insert (constraintHandle h1) (mx :< WT.Box t1') t1
      (uncastSeq', h3) <- inferQuoteSeq h2 uncastSeq FromNoema
      (e2', t2) <- infer h3 e2
      return (m :< WT.BoxElim castSeq' mxt' e1' uncastSeq' e2', t2)
    _ :< WT.Actual e -> do
      (e', t') <- infer h e
      liftIO $ Constraint.insertActualityConstraint (constraintHandle h) t'
      return (e', t')
    m :< WT.Let opacity (mx, x, t) e1 e2 -> do
      (e1', t1') <- infer h e1
      t' <- inferType h t >>= resolveType h
      liftIO $ WeakType.insert (weakTypeHandle h) x t'
      case opacity of
        WT.Noetic ->
          liftIO $ Constraint.insertActualityConstraint (constraintHandle h) t'
        _ ->
          return ()
      liftIO $ Constraint.insert (constraintHandle h) t' t1' -- run this before `infer h e2`
      (e2', t2') <- infer h e2 -- no context extension
      return (m :< WT.Let opacity (mx, x, t') e1' e2', t2')
    m :< WT.Hole holeID _ -> do
      let rawHoleID = HID.reify holeID
      mHoleInfo <- liftIO $ Hole.lookup (holeHandle h) rawHoleID
      case mHoleInfo of
        Just (_ :< holeTerm, _ :< holeType) -> do
          return (m :< holeTerm, m :< holeType)
        Nothing -> do
          let holeArgs = map (\(mx, x, _) -> mx :< WT.Var x) (varEnv h)
          let holeTerm = m :< WT.Hole holeID holeArgs
          holeType <- liftIO $ Gensym.newHole (gensymHandle h) m holeArgs
          liftIO $ Hole.insert (holeHandle h) rawHoleID holeTerm holeType
          return (holeTerm, holeType)
    m :< WT.Prim prim
      | WP.Type _ <- prim ->
          return (term, m :< WT.Tau)
      | WP.Value primValue <- prim ->
          case primValue of
            WPV.Int t v -> do
              t' <- inferType (h {varEnv = []}) t
              return (m :< WT.Prim (WP.Value (WPV.Int t' v)), t')
            WPV.Float t v -> do
              t' <- inferType (h {varEnv = []}) t
              return (m :< WT.Prim (WP.Value (WPV.Float t' v)), t')
            WPV.Op op -> do
              primOpType <- liftIO $ primOpToType h m op
              return (term, weaken primOpType)
            WPV.StaticText t text -> do
              t' <- inferType (h {varEnv = []}) t
              return (m :< WT.Prim (WP.Value (WPV.StaticText t' text)), m :< WT.BoxNoema t')
            WPV.Rune _ -> do
              return (m :< WT.Prim prim, m :< WT.Prim (WP.Type PT.Rune))
    m :< WT.Magic (M.WeakMagic magic) -> do
      case magic of
        M.Cast from to value -> do
          from' <- inferType h from
          to'@(_ :< toInner) <- inferType h to
          (value', t) <- infer h value
          liftIO $ Constraint.insert (constraintHandle h) from' t
          return (m :< WT.Magic (M.WeakMagic $ M.Cast from' to' value'), m :< toInner)
        M.Store t unit value pointer -> do
          t' <- inferType h t
          unit' <- inferType h unit
          (value', tValue) <- infer h value
          (pointer', tPointer) <- infer h pointer
          liftIO $ Constraint.insert (constraintHandle h) t' tValue
          liftIO $ Constraint.insert (constraintHandle h) (m :< WT.Prim (WP.Type PT.Pointer)) tPointer
          return (m :< WT.Magic (M.WeakMagic $ M.Store t' unit' value' pointer'), unit')
        M.Load t pointer -> do
          t' <- inferType h t
          (pointer', tPointer) <- infer h pointer
          liftIO $ Constraint.insert (constraintHandle h) (m :< WT.Prim (WP.Type PT.Pointer)) tPointer
          return (m :< WT.Magic (M.WeakMagic $ M.Load t' pointer'), t')
        M.Alloca lt size -> do
          (size', sizeType) <- infer h size
          intType <- getIntType m
          liftIO $ Constraint.insert (constraintHandle h) intType sizeType
          return (m :< WT.Magic (M.WeakMagic $ M.Alloca lt size'), m :< WT.Prim (WP.Type PT.Pointer))
        M.External _ _ funcName args varArgs -> do
          (domList, cod) <- WeakDecl.lookup (weakDeclHandle h) m (DN.Ext funcName)
          ensureArityCorrectness h term (length domList) (length args)
          (args', argTypes) <- mapAndUnzipM (infer h) args
          liftIO $ forM_ (zip domList argTypes) $ uncurry $ Constraint.insert (constraintHandle h)
          varArgs' <- forM varArgs $ \(e, t) -> do
            (e', t') <- infer h e
            t'' <- inferType h t
            liftIO $ Constraint.insert (constraintHandle h) t'' t'
            return (e', t')
          case cod of
            FCT.Cod (_ :< c) -> do
              let c' = m :< c
              return (m :< WT.Magic (M.WeakMagic $ M.External domList (FCT.Cod c') funcName args' varArgs'), c')
            FCT.Void -> do
              let voidType = m :< WT.Void
              return (m :< WT.Magic (M.WeakMagic $ M.External domList FCT.Void funcName args' varArgs'), voidType)
        M.Global name t -> do
          t' <- inferType h t
          return (m :< WT.Magic (M.WeakMagic $ M.Global name t'), t')
        M.OpaqueValue e -> do
          (e', t) <- infer h e
          return (m :< WT.Magic (M.WeakMagic $ M.OpaqueValue e'), t)
    m :< WT.Annotation logLevel annot e -> do
      (e', t) <- infer h e
      case annot of
        Annotation.Type _ -> do
          return (m :< WT.Annotation logLevel (Annotation.Type t) e', t)
    m :< WT.Resource dd resourceID unitType discarder copier -> do
      unitType' <- inferType h unitType
      (discarder', td) <- infer (h {varEnv = []}) discarder
      (copier', tc) <- infer (h {varEnv = []}) copier
      x <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "_"
      resourceType <- liftIO $ newHole h m []
      let tDiscard = m :< WT.Pi [] [(m, x, resourceType)] unitType'
      let tCopy = m :< WT.Pi [] [(m, x, resourceType)] resourceType
      liftIO $ Constraint.insert (constraintHandle h) tDiscard td
      liftIO $ Constraint.insert (constraintHandle h) tCopy tc
      return (m :< WT.Resource dd resourceID unitType' discarder' copier', m :< WT.Tau)
    m :< WT.Use e@(mt :< _) xts cont -> do
      (e', t') <- infer h e
      t'' <- resolveType h t'
      case t'' of
        _ :< WT.Data attr _ dataArgs
          | AttrD.Attr {..} <- attr,
            [(consDD, isConstLike')] <- consNameList -> do
              (_, keyList) <- KeyArg.lookup (keyArgHandle h) m consDD
              defaultKeyMap <- liftIO $ constructDefaultKeyMap h m keyList
              let specifiedKeyMap = Map.fromList $ flip map xts $ \(mx, x, t) -> (Ident.toText x, (mx, x, t))
              let keyMap = Map.union specifiedKeyMap defaultKeyMap
              reorderedArgs <- KeyArg.reorderArgs m keyList keyMap
              dataArgs' <- mapM (const $ liftIO $ newTypedHole h m (varEnv h)) [1 .. length dataArgs]
              cursor <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "cursor"
              od <- liftIO $ OptimizableData.lookup (optDataHandle h) consDD
              let freedVars = if mustBypassCursorDealloc od then [] else [cursor]
              liftIO $ WeakType.insert (weakTypeHandle h) cursor t''
              (tree', _ :< treeType) <-
                inferDecisionTree m h $
                  DT.Switch
                    (cursor, t'')
                    ( DT.Unreachable,
                      [ DT.ConsCase $
                          DT.ConsCaseRecord
                            { mCons = m,
                              consDD = consDD,
                              isConstLike = isConstLike',
                              disc = D.zero,
                              dataArgs = dataArgs',
                              consArgs = reorderedArgs,
                              cont = DT.Leaf freedVars (adjustCont m reorderedArgs) cont
                            }
                      ]
                    )
              return (m :< WT.DataElim False [(cursor, e', t'')] tree', m :< treeType)
          | otherwise -> do
              raiseError mt $ "Expected a single-constructor ADT, but found: " <> toText t''
        _ :< _ -> do
          raiseError mt $ "Expected an ADT, but found: " <> toText t''
    m :< WT.Void ->
      return (m :< WT.Void, m :< WT.Tau)

data CastDirection
  = FromNoema
  | ToNoema

inferQuoteSeq ::
  Handle ->
  [(BinderF WT.WeakTerm, WT.WeakTerm)] ->
  CastDirection ->
  EIO ([(BinderF WT.WeakTerm, WT.WeakTerm)], Handle)
inferQuoteSeq h letSeq castDirection = do
  let (xts, es) = unzip letSeq
  (xts', h') <- inferBinder' h xts
  (es', ts) <- mapAndUnzipM (infer h') es
  forM_ (zip xts' ts) $ \((m1, _, tInner), tOuter@(m2 :< _)) -> do
    case castDirection of
      ToNoema ->
        liftIO $ Constraint.insert (constraintHandle h) tInner (m2 :< WT.BoxNoema tOuter)
      FromNoema ->
        liftIO $ Constraint.insert (constraintHandle h) (m1 :< WT.BoxNoema tInner) tOuter
  return (zip xts' es', h')

mustBypassCursorDealloc :: Maybe OD.OptimizableData -> Bool
mustBypassCursorDealloc odOrNone =
  case odOrNone of
    Just OD.Enum ->
      True
    Just OD.Unary ->
      True
    _ ->
      False

adjustCont :: Hint -> [BinderF WT.WeakTerm] -> [(BinderF WT.WeakTerm, WT.WeakTerm)]
adjustCont m xts =
  case xts of
    [] ->
      []
    (mx, x, t) : rest ->
      ((mx, x, t), mx :< WT.Var (ignoreUse x)) : adjustCont m rest

ignoreUse :: Ident -> Ident
ignoreUse (I (x, i)) =
  I (expVarPrefix <> x, i)

constructDefaultKeyMap :: Handle -> Hint -> [Key] -> IO (Map.HashMap Key (BinderF WT.WeakTerm))
constructDefaultKeyMap h m keyList = do
  names <- mapM (const $ Gensym.newIdentForHole (gensymHandle h)) keyList
  ts <- mapM (const $ newHole h m $ varEnv h) names
  return $ Map.fromList $ zipWith (\k (v, t) -> (k, (m, v, t))) keyList $ zip names ts

inferArgs ::
  Handle ->
  WT.SubstWeakTerm ->
  Hint ->
  [(WT.WeakTerm, WT.WeakTerm)] ->
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  EIO WT.WeakTerm
inferArgs h sub m args1 args2 cod =
  case (args1, args2) of
    ([], []) ->
      liftIO $ Subst.subst (substHandle h) sub cod
    ((e, t) : ets, (_, x, tx) : xts) -> do
      tx' <- liftIO $ Subst.subst (substHandle h) sub tx
      liftIO $ Constraint.insert (constraintHandle h) tx' t
      inferArgs h (IntMap.insert (Ident.toInt x) (Right e) sub) m ets xts cod
    _ ->
      raiseCritical m "Invalid argument passed to inferArgs"

inferType :: Handle -> WT.WeakTerm -> EIO WT.WeakTerm
inferType h t = do
  (t', u) <- infer h t
  liftIO $ Constraint.insert (constraintHandle h) (WT.metaOf t :< WT.Tau) u
  return t'

inferPiBinder ::
  Handle ->
  [BinderF WT.WeakTerm] ->
  EIO ([BinderF WT.WeakTerm], Handle)
inferPiBinder h binder =
  case binder of
    [] -> do
      return ([], h)
    ((mx, x, t) : xts) -> do
      t' <- inferType h t
      liftIO $ WeakType.insert (weakTypeHandle h) x t'
      (xtls', h') <- inferPiBinder (extendHandle (mx, x, t') h) xts
      return ((mx, x, t') : xtls', h')

inferBinder' ::
  Handle ->
  [BinderF WT.WeakTerm] ->
  EIO ([BinderF WT.WeakTerm], Handle)
inferBinder' h binder =
  case binder of
    [] -> do
      return ([], h)
    ((mx, x, t) : xts) -> do
      t' <- inferType h t
      liftIO $ WeakType.insert (weakTypeHandle h) x t'
      (xts', h') <- inferBinder' (extendHandle' (mx, x, t') h) xts
      return ((mx, x, t') : xts', h')

inferBinder1 ::
  Handle ->
  BinderF WT.WeakTerm ->
  EIO (BinderF WT.WeakTerm, Handle)
inferBinder1 h (mx, x, t) = do
  t' <- inferType h t
  liftIO $ WeakType.insert (weakTypeHandle h) x t'
  return ((mx, x, t'), extendHandle' (mx, x, t') h)

inferPiElim ::
  Handle ->
  Hint ->
  (WT.WeakTerm, WT.WeakTerm) ->
  [(WT.WeakTerm, WT.WeakTerm)] ->
  EIO (WT.WeakTerm, WT.WeakTerm)
inferPiElim h m (e, t) expArgs = do
  t' <- resolveType h t
  case t' of
    _ :< WT.Pi impPiArgs expPiArgs cod -> do
      ensureArityCorrectness h e (length expPiArgs) (length expArgs)
      impArgs <- mapM (const $ liftIO $ newTypedHole h m $ varEnv h) [1 .. length impPiArgs]
      let args = impArgs ++ expArgs
      let piArgs = impPiArgs ++ expPiArgs
      _ :< cod' <- inferArgs h IntMap.empty m args piArgs cod
      return (m :< WT.PiElim e (map fst args), m :< cod')
    _ ->
      raiseError m $ "Expected a function type, but got: " <> toText t'

inferPiElimExplicit ::
  Handle ->
  Hint ->
  (WT.WeakTerm, WT.WeakTerm) ->
  [(WT.WeakTerm, WT.WeakTerm)] ->
  EIO (WT.WeakTerm, WT.WeakTerm)
inferPiElimExplicit h m (e, t) args = do
  t' <- resolveType h t
  case t' of
    _ :< WT.Pi impPiArgs expPiArgs cod -> do
      let piArgs = impPiArgs ++ expPiArgs
      ensureArityCorrectness h e (length piArgs) (length args)
      _ :< cod' <- inferArgs h IntMap.empty m args piArgs cod
      return (m :< WT.PiElim e (map fst args), m :< cod')
    _ ->
      raiseError m $ "Expected a function type, but got: " <> toText t'

newTypedHole :: Handle -> Hint -> BoundVarEnv -> IO (WT.WeakTerm, WT.WeakTerm)
newTypedHole h m varEnv = do
  i <- HID.HoleID <$> Gensym.newCount (gensymHandle h)
  j <- HID.HoleID <$> Gensym.newCount (gensymHandle h)
  let holeArgs = map (\(mx, x, _) -> mx :< WT.Var x) varEnv
  let holeTerm = m :< WT.Hole i holeArgs
  let holeType = m :< WT.Hole j holeArgs
  Hole.insert (holeHandle h) (HID.reify i) holeTerm holeType
  return (holeTerm, holeType)

ensureArityCorrectness :: Handle -> WT.WeakTerm -> Int -> Int -> EIO ()
ensureArityCorrectness h function expected found = do
  when (expected /= found) $ do
    case function of
      m :< WT.VarGlobal _ name -> do
        mainModule <- Env.getMainModule (envHandle h)
        let name' = Locator.getReadableDD mainModule name
        raiseError m $
          "The function `"
            <> name'
            <> "` expects "
            <> T.pack (show expected)
            <> " arguments, but found "
            <> T.pack (show found)
            <> "."
      m :< _ ->
        raiseError m $
          "This function expects "
            <> T.pack (show expected)
            <> " arguments, but found "
            <> T.pack (show found)
            <> "."

primOpToType :: Handle -> Hint -> PrimOp -> IO TM.Term
primOpToType h m op = do
  let (domList, cod) = getTypeInfo op
  let domList' = map (Term.fromPrimNum m) domList
  xs <- mapM (const (Gensym.newIdentFromText (gensymHandle h) "_")) domList'
  let xts = zipWith (\x t -> (m, x, t)) xs domList'
  let cod' = Term.fromPrimNum m cod
  return $ m :< TM.Pi [] xts cod'

inferLet ::
  Handle ->
  (BinderF WT.WeakTerm, WT.WeakTerm) ->
  EIO (BinderF WT.WeakTerm, WT.WeakTerm)
inferLet h ((mx, x, t), e1) = do
  (e1', t1') <- infer h e1
  t' <- inferType h t >>= resolveType h
  liftIO $ WeakType.insert (weakTypeHandle h) x t'
  liftIO $ Constraint.insert (constraintHandle h) t' t1'
  return ((mx, x, t'), e1')

inferDecisionTree ::
  Hint ->
  Handle ->
  DT.DecisionTree WT.WeakTerm ->
  EIO (DT.DecisionTree WT.WeakTerm, WT.WeakTerm)
inferDecisionTree m h tree =
  case tree of
    DT.Leaf ys letSeq body -> do
      letSeq' <- mapM (inferLet h) letSeq
      (body', answerType) <- infer h body
      return (DT.Leaf ys letSeq' body', answerType)
    DT.Unreachable -> do
      hole <- liftIO $ newHole h m (varEnv h)
      return (DT.Unreachable, hole)
    DT.Switch (cursor, _) clauseList -> do
      _ :< cursorType <- WeakType.lookup (weakTypeHandle h) m cursor
      let cursorType' = m :< cursorType
      (clauseList', answerType) <- inferClauseList m h cursorType' clauseList
      return (DT.Switch (cursor, cursorType') clauseList', answerType)

inferClauseList ::
  Hint ->
  Handle ->
  WT.WeakTerm ->
  DT.CaseList WT.WeakTerm ->
  EIO (DT.CaseList WT.WeakTerm, WT.WeakTerm)
inferClauseList m h cursorType (fallbackClause, clauseList) = do
  (clauseList', answerTypeList) <- flip mapAndUnzipM clauseList $ inferClause h cursorType
  let mAns = getClauseHint answerTypeList m
  hole <- liftIO $ newHole h mAns (varEnv h)
  (fallbackClause', fallbackAnswerType) <- inferDecisionTree mAns h fallbackClause
  liftIO $ forM_ (answerTypeList ++ [fallbackAnswerType]) $ Constraint.insert (constraintHandle h) hole
  return ((fallbackClause', clauseList'), fallbackAnswerType)

getClauseHint :: [WT.WeakTerm] -> Hint -> Hint
getClauseHint ts fallbackHint =
  case ts of
    (m :< _) : _ ->
      m
    _ ->
      fallbackHint

inferClause ::
  Handle ->
  WT.WeakTerm ->
  DT.Case WT.WeakTerm ->
  EIO (DT.Case WT.WeakTerm, WT.WeakTerm)
inferClause h cursorType@(_ :< cursorTypeInner) decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat literal cont -> do
      (cont', tCont) <- inferDecisionTree mPat h cont
      case literal of
        L.Int _ -> do
          liftIO $ Constraint.insertIntegerConstraint (constraintHandle h) (mPat :< cursorTypeInner)
        L.Rune _ ->
          liftIO $ Constraint.insert (constraintHandle h) cursorType (mPat :< WT.Prim (WP.Type PT.Rune))
      return (DT.LiteralCase mPat literal cont', tCont)
    DT.ConsCase record@DT.ConsCaseRecord {..} -> do
      let m = mCons
      let (dataTermList, _) = unzip dataArgs
      typedDataArgs' <- mapM (infer h) dataTermList
      (consArgs', extendedVarEnv) <- inferBinder' h consArgs
      let argNum = AN.fromInt $ length dataArgs + length consArgs
      let attr = AttrVG.Attr {..}
      consTerm <- infer h $ m :< WT.VarGlobal attr consDD
      (_, tPat) <- inferPiElimExplicit h m consTerm $ typedDataArgs' ++ map (\(mx, x, t) -> (mx :< WT.Var x, t)) consArgs'
      liftIO $ Constraint.insert (constraintHandle h) cursorType tPat
      (cont', tCont) <- inferDecisionTree m extendedVarEnv cont
      return
        ( DT.ConsCase
            record
              { DT.dataArgs = typedDataArgs',
                DT.consArgs = consArgs',
                DT.cont = cont'
              },
          tCont
        )

resolveType :: Handle -> WT.WeakTerm -> EIO WT.WeakTerm
resolveType h t = do
  sub <- Unify.unifyCurrentConstraints (unifyHandle h)
  reduceWeakType' h sub t

reduceWeakType' :: Handle -> HS.HoleSubst -> WT.WeakTerm -> EIO WT.WeakTerm
reduceWeakType' h sub e = do
  e' <- Reduce.reduce (reduceHandle h) e
  case e' of
    m :< WT.Hole hole es ->
      case HS.lookup hole sub of
        Nothing ->
          return e'
        Just (xs, body)
          | length xs == length es -> do
              let s = IntMap.fromList $ zip (map Ident.toInt xs) (map Right es)
              liftIO (WT.subst (substHandle h) s body) >>= reduceWeakType' h sub
          | otherwise ->
              raiseError m "Arity mismatch"
    m :< WT.PiElim (_ :< WT.VarGlobal _ name) args -> do
      lamOrNone <- liftIO $ lookupDefinition h name
      case lamOrNone of
        Just lam ->
          reduceWeakType' h sub $ m :< WT.PiElim lam args
        Nothing -> do
          return e'
    _ ->
      return e'

lookupDefinition :: Handle -> DD.DefiniteDescription -> IO (Maybe WT.WeakTerm)
lookupDefinition h name = do
  WeakDefinition.lookup' (weakDefHandle h) name

newHole :: Handle -> Hint -> BoundVarEnv -> IO WT.WeakTerm
newHole h m varEnv = do
  Gensym.newHole (gensymHandle h) m $ map (\(mx, x, _) -> mx :< WT.Var x) varEnv
