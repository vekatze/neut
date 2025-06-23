{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list comprehension" #-}

module Kernel.Elaborate.Move.Internal.Infer
  ( Handle,
    inferStmt,
  )
where

import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IntMap qualified as IntMap
import Data.Text qualified as T
import Error.Move.Run (raiseCritical, raiseError)
import Error.Rule.EIO (EIO)
import Gensym.Move.Gensym qualified as Gensym
import Kernel.Common.Move.Handle.Global.Env qualified as Env
import Kernel.Common.Move.Handle.Global.Platform qualified as Platform
import Kernel.Common.Move.Handle.Global.Type qualified as Type
import Kernel.Common.Rule.Handle.Global.Platform qualified as Platform
import Kernel.Common.Rule.ReadableDD
import Kernel.Elaborate.Move.Internal.Handle.Constraint qualified as Constraint
import Kernel.Elaborate.Move.Internal.Handle.Elaborate
import Kernel.Elaborate.Move.Internal.Handle.Hole qualified as Hole
import Kernel.Elaborate.Move.Internal.Handle.WeakDecl qualified as WeakDecl
import Kernel.Elaborate.Move.Internal.Handle.WeakDef qualified as WeakDef
import Kernel.Elaborate.Move.Internal.Handle.WeakType qualified as WeakType
import Kernel.Elaborate.Move.Internal.Unify qualified as Unify
import Kernel.Elaborate.Rule.HoleSubst qualified as HS
import Language.Common.Move.CreateSymbol qualified as Gensym
import Language.Common.Rule.Annotation qualified as Annotation
import Language.Common.Rule.ArgNum qualified as AN
import Language.Common.Rule.Attr.Data qualified as AttrD
import Language.Common.Rule.Attr.DataIntro qualified as AttrDI
import Language.Common.Rule.Attr.Lam qualified as AttrL
import Language.Common.Rule.Attr.VarGlobal qualified as AttrVG
import Language.Common.Rule.Binder
import Language.Common.Rule.DecisionTree qualified as DT
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.ForeignCodType qualified as FCT
import Language.Common.Rule.Geist qualified as G
import Language.Common.Rule.HoleID qualified as HID
import Language.Common.Rule.Ident (isHole)
import Language.Common.Rule.Ident.Reify qualified as Ident
import Language.Common.Rule.ImpArgs qualified as ImpArgs
import Language.Common.Rule.LamKind qualified as LK
import Language.Common.Rule.Literal qualified as L
import Language.Common.Rule.Magic qualified as M
import Language.Common.Rule.PiKind qualified as PK
import Language.Common.Rule.PrimOp
import Language.Common.Rule.PrimType qualified as PT
import Language.Common.Rule.StmtKind
import Language.Common.Rule.StmtKind qualified as SK
import Language.LowComp.Rule.DeclarationName qualified as DN
import Language.Term.Rule.Term qualified as TM
import Language.Term.Rule.Term.FromPrimNum qualified as Term
import Language.Term.Rule.Term.Weaken
import Language.WeakTerm.Move.CreateHole qualified as WT
import Language.WeakTerm.Move.Subst qualified as Subst
import Language.WeakTerm.Rule.WeakPrim qualified as WP
import Language.WeakTerm.Rule.WeakPrimValue qualified as WPV
import Language.WeakTerm.Rule.WeakStmt
import Language.WeakTerm.Rule.WeakTerm qualified as WT
import Language.WeakTerm.Rule.WeakTerm.ToText (toText)
import Logger.Rule.Hint

type BoundVarEnv = [BinderF WT.WeakTerm]

inferStmt :: Handle -> WeakStmt -> EIO WeakStmt
inferStmt h stmt =
  case stmt of
    WeakStmtDefine isConstLike stmtKind m x impArgs expArgs codType e -> do
      (impArgs', h') <- inferImpBinder h impArgs
      (expArgs', h'') <- inferBinder' h' expArgs
      codType' <- inferType h'' codType
      case stmtKind of
        SK.DataIntro {} -> do
          liftIO $ insertType h'' x $ m :< WT.Pi (PK.DataIntro isConstLike) impArgs' expArgs' codType'
        _ ->
          liftIO $ insertType h'' x $ m :< WT.Pi (PK.Normal isConstLike) impArgs' expArgs' codType'
      stmtKind' <- inferStmtKind h'' stmtKind
      (e', te) <- infer h'' e
      liftIO $ Constraint.insert (constraintHandle h'') codType' te
      case getMainUnitType stmtKind of
        Just unitType -> do
          let expected = m :< WT.Pi PK.normal [] [] unitType
          let actual = m :< WT.Pi PK.normal impArgs' expArgs' codType'
          liftIO $ Constraint.insert (constraintHandle h'') expected actual
        Nothing ->
          return ()
      return $ WeakStmtDefine isConstLike stmtKind' m x impArgs' expArgs' codType' e'
    WeakStmtNominal m geistList -> do
      geistList' <- mapM (inferGeist h) geistList
      return $ WeakStmtNominal m geistList'
    WeakStmtForeign foreignList ->
      return $ WeakStmtForeign foreignList

inferGeist :: Handle -> G.Geist WT.WeakTerm -> EIO (G.Geist WT.WeakTerm)
inferGeist h (G.Geist {..}) = do
  (impArgs', h') <- inferImpBinder h impArgs
  (expArgs', h'') <- inferBinder' h' expArgs
  cod' <- inferType h'' cod
  liftIO $ insertType h'' name $ loc :< WT.Pi PK.normal impArgs' expArgs' cod'
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
    Main opacity t -> do
      t' <- inferType h t
      return $ Main opacity t'
    Data dataName dataArgs consInfoList -> do
      (dataArgs', varEnv) <- inferBinder' h dataArgs
      consInfoList' <- forM consInfoList $ \(m, dd, constLike, consArgs, discriminant) -> do
        (consArgs', _) <- inferBinder' varEnv consArgs
        return (m, dd, constLike, consArgs', discriminant)
      return $ Data dataName dataArgs' consInfoList'
    DataIntro consName dataArgs expConsArgs discriminant -> do
      (dataArgs', varEnv) <- inferBinder' h dataArgs
      (expConsArgs', _) <- inferBinder' varEnv expConsArgs
      return $ DataIntro consName dataArgs' expConsArgs' discriminant

getIntType :: Platform.Handle -> Hint -> EIO WT.WeakTerm
getIntType h m = do
  let baseSize = Platform.getDataSizeValue h
  return $ WT.intTypeBySize m baseSize

getMainUnitType :: StmtKind WT.WeakTerm -> Maybe WT.WeakTerm
getMainUnitType stmtKind = do
  case stmtKind of
    Main _ unitType ->
      return unitType
    _ ->
      Nothing

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
    m :< WT.Pi piKind impArgs expArgs t -> do
      (impArgs', h') <- inferImpBinder h impArgs
      (expArgs', h'') <- inferPiBinder h' expArgs
      t' <- inferType h'' t
      return (m :< WT.Pi piKind impArgs' expArgs' t', m :< WT.Tau)
    m :< WT.PiIntro attr@(AttrL.Attr {lamKind}) impArgs expArgs e -> do
      case lamKind of
        LK.Fix (mx, x, codType) -> do
          (impArgs', h') <- inferImpBinder h impArgs
          (expArgs', h'') <- inferBinder' h' expArgs
          codType' <- inferType h'' codType
          let impArgsWithDefaults = impArgs'
          let piType = m :< WT.Pi PK.normal impArgsWithDefaults expArgs' codType'
          liftIO $ WeakType.insert (weakTypeHandle h) x piType
          (e', tBody) <- infer h'' e
          liftIO $ Constraint.insert (constraintHandle h'') codType' tBody
          let term' = m :< WT.PiIntro (attr {AttrL.lamKind = LK.Fix (mx, x, codType')}) impArgs' expArgs' e'
          return (term', piType)
        LK.Normal name codType -> do
          (impArgs', h') <- inferImpBinder h impArgs
          (expArgs', h'') <- inferBinder' h' expArgs
          codType' <- inferType h'' codType
          (e', t') <- infer h'' e
          liftIO $ Constraint.insert (constraintHandle h'') codType' t'
          let term' = m :< WT.PiIntro (attr {AttrL.lamKind = LK.Normal name codType'}) impArgs' expArgs' e'
          let impArgsWithDefaults = impArgs'
          return (term', m :< WT.Pi PK.normal impArgsWithDefaults expArgs' t')
    m :< WT.PiElim _ e impArgs expArgs -> do
      etl <- infer h e
      impArgs' <- ImpArgs.traverseImpArgs (infer h) impArgs
      expArgs' <- mapM (infer h) expArgs
      inferPiElim h m etl impArgs' expArgs'
    m :< WT.PiElimExact e -> do
      (e', t) <- infer h e
      t' <- resolveType h t
      case t' of
        _ :< WT.Pi _ impArgs expArgs codType -> do
          let impBinders = map fst impArgs
          impValues <- mapM (createImpArgValue h m) impArgs
          let sub = IntMap.fromList $ zip (map (\(_, x, _) -> Ident.toInt x) impBinders) (map Right impValues)
          (expArgs', sub') <- liftIO $ Subst.subst' (substHandle h) sub expArgs
          let expArgs'' = map (\(_, x, _) -> m :< WT.Var x) expArgs'
          codType' <- liftIO $ Subst.subst (substHandle h) sub' codType
          lamID <- liftIO $ Gensym.newCount (gensymHandle h)
          infer h $ m :< WT.PiIntro (AttrL.normal lamID codType') [] expArgs' (m :< WT.PiElim False e' ImpArgs.Unspecified expArgs'')
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
      letSeq' <- inferQuoteSeq h letSeq FromNoema
      (e', t) <- infer h e
      return (m :< WT.BoxIntro letSeq' e', m :< WT.Box t)
    m :< WT.BoxIntroQuote e -> do
      (e', t) <- infer h e
      liftIO $ Constraint.insertActualityConstraint (constraintHandle h) t
      return (m :< WT.BoxIntroQuote e', m :< WT.Box t)
    m :< WT.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      castSeq' <- inferQuoteSeq h castSeq ToNoema
      (e1', t1) <- infer h e1
      mxt'@(mx, _, t1') <- inferBinder1 h mxt
      liftIO $ Constraint.insert (constraintHandle h) (mx :< WT.Box t1') t1
      uncastSeq' <- inferQuoteSeq h uncastSeq FromNoema
      (e2', t2) <- infer h e2
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
          holeType <- liftIO $ WT.createHole (gensymHandle h) m holeArgs
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
          intType <- getIntType (platformHandle h) m
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
        M.CallType func arg1 arg2 -> do
          func' <- inferType h func
          (arg1', t1) <- infer h arg1
          (arg2', _) <- infer h arg2
          intType <- getIntType (platformHandle h) m
          liftIO $ Constraint.insert (constraintHandle h) intType t1
          resultType <- liftIO $ newHole h m (varEnv h)
          return (m :< WT.Magic (M.WeakMagic $ M.CallType func' arg1' arg2'), resultType)
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
      let tDiscard = m :< WT.Pi PK.normal [] [(m, x, resourceType)] unitType'
      let tCopy = m :< WT.Pi PK.normal [] [(m, x, resourceType)] resourceType
      liftIO $ Constraint.insert (constraintHandle h) tDiscard td
      liftIO $ Constraint.insert (constraintHandle h) tCopy tc
      return (m :< WT.Resource dd resourceID unitType' discarder' copier', m :< WT.Tau)
    m :< WT.Void ->
      return (m :< WT.Void, m :< WT.Tau)

data CastDirection
  = FromNoema
  | ToNoema

inferQuoteSeq ::
  Handle ->
  [(BinderF WT.WeakTerm, WT.WeakTerm)] ->
  CastDirection ->
  EIO [(BinderF WT.WeakTerm, WT.WeakTerm)]
inferQuoteSeq h letSeq castDirection = do
  let (xts, es) = unzip letSeq
  xts' <- inferBinder'' h xts
  (es', ts) <- mapAndUnzipM (infer h) es
  forM_ (zip xts' ts) $ \((m1, _, tInner), tOuter@(m2 :< _)) -> do
    case castDirection of
      ToNoema ->
        liftIO $ Constraint.insert (constraintHandle h) tInner (m2 :< WT.BoxNoema tOuter)
      FromNoema ->
        liftIO $ Constraint.insert (constraintHandle h) (m1 :< WT.BoxNoema tInner) tOuter
  return (zip xts' es')

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

inferImpBinder ::
  Handle ->
  [(BinderF WT.WeakTerm, Maybe WT.WeakTerm)] ->
  EIO ([(BinderF WT.WeakTerm, Maybe WT.WeakTerm)], Handle)
inferImpBinder h binderList =
  case binderList of
    [] -> do
      return ([], h)
    (((mx, x, t), mDefaultValue) : rest) -> do
      t' <- inferType h t
      mDefaultValue' <-
        case mDefaultValue of
          Nothing ->
            return Nothing
          Just defaultValue -> do
            (defaultValue', defaultType) <- infer h defaultValue
            liftIO $ Constraint.insert (constraintHandle h) t' defaultType
            return (Just defaultValue')
      liftIO $ WeakType.insert (weakTypeHandle h) x t'
      (rest', h') <- inferImpBinder (extendHandle (mx, x, t') h) rest
      return (((mx, x, t'), mDefaultValue') : rest', h')

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

inferBinder'' ::
  Handle ->
  [BinderF WT.WeakTerm] ->
  EIO [BinderF WT.WeakTerm]
inferBinder'' h binder =
  case binder of
    [] -> do
      return []
    ((mx, x, t) : xts) -> do
      t' <- inferType h t
      liftIO $ WeakType.insert (weakTypeHandle h) x t'
      xts' <- inferBinder'' h xts
      return $ (mx, x, t') : xts'

inferBinder1 ::
  Handle ->
  BinderF WT.WeakTerm ->
  EIO (BinderF WT.WeakTerm)
inferBinder1 h (mx, x, t) = do
  t' <- inferType h t
  liftIO $ WeakType.insert (weakTypeHandle h) x t'
  return (mx, x, t')

resolvePartiallySpecifiedArgs ::
  Handle ->
  Hint ->
  [Maybe (WT.WeakTerm, WT.WeakTerm)] ->
  [(BinderF WT.WeakTerm, Maybe WT.WeakTerm)] ->
  EIO [(WT.WeakTerm, WT.WeakTerm)]
resolvePartiallySpecifiedArgs h m partialArgs impParams = do
  let pairs = zip partialArgs impParams
  mapM resolveArg pairs
  where
    resolveArg (mArg, (_, mDefault)) = do
      case mArg of
        Just arg ->
          return arg
        Nothing -> do
          case mDefault of
            Just defaultValue ->
              infer h defaultValue
            Nothing -> do
              liftIO $ newTypedHole h m (varEnv h)

inferPiElim ::
  Handle ->
  Hint ->
  (WT.WeakTerm, WT.WeakTerm) ->
  ImpArgs.ImpArgs (WT.WeakTerm, WT.WeakTerm) ->
  [(WT.WeakTerm, WT.WeakTerm)] ->
  EIO (WT.WeakTerm, WT.WeakTerm)
inferPiElim h m (e, t) impArgs expArgs = do
  t' <- resolveType h t
  case t' of
    _ :< WT.Pi _ impParams expParams cod -> do
      ensureArityCorrectness h e (length expParams) (length expArgs)
      impArgs' <- do
        case impArgs of
          ImpArgs.Unspecified -> do
            mapM (createImpArgValueFromParam h m) impParams
          ImpArgs.FullySpecified impArgs' -> do
            ensureImplicitArityCorrectness h e (length impParams) (length impArgs')
            return impArgs'
          ImpArgs.PartiallySpecified impArgs' -> do
            resolvePartiallySpecifiedArgs h m impArgs' impParams
      let impBinders = map fst impParams
      let piArgs = impBinders ++ expParams
      _ :< cod' <- inferArgs h IntMap.empty m (impArgs' ++ expArgs) piArgs cod
      return (m :< WT.PiElim False e (ImpArgs.FullySpecified (map fst impArgs')) (map fst expArgs), m :< cod')
    _ :< WT.BoxNoema (_ :< WT.Pi _ impParams expParams cod) -> do
      ensureArityCorrectness h e (length expParams) (length expArgs)
      impArgs' <- do
        case impArgs of
          ImpArgs.Unspecified -> do
            mapM (const $ liftIO $ newTypedHole h m $ varEnv h) [1 .. length impParams]
          ImpArgs.FullySpecified impArgs' -> do
            ensureImplicitArityCorrectness h e (length impParams) (length impArgs')
            return impArgs'
          ImpArgs.PartiallySpecified impArgs' -> do
            resolvePartiallySpecifiedArgs h m impArgs' impParams
      let impBinders = map fst impParams
      let piArgs = impBinders ++ expParams
      _ :< cod' <- inferArgs h IntMap.empty m (impArgs' ++ expArgs) piArgs cod
      return (m :< WT.PiElim True e (ImpArgs.FullySpecified (map fst impArgs')) (map fst expArgs), m :< cod')
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
        let mainModule = Env.getMainModule (envHandle h)
        let name' = readableDD mainModule name
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

ensureImplicitArityCorrectness :: Handle -> WT.WeakTerm -> Int -> Int -> EIO ()
ensureImplicitArityCorrectness h function expected found = do
  when (expected /= found) $ do
    case function of
      m :< WT.VarGlobal _ name -> do
        let mainModule = Env.getMainModule (envHandle h)
        let name' = readableDD mainModule name
        raiseError m $
          "The function `"
            <> name'
            <> "` expects "
            <> T.pack (show expected)
            <> " implicit arguments, but found "
            <> T.pack (show found)
            <> "."
      m :< _ ->
        raiseError m $
          "This function expects "
            <> T.pack (show expected)
            <> " implicit arguments, but found "
            <> T.pack (show found)
            <> "."

primOpToType :: Handle -> Hint -> PrimOp -> IO TM.Term
primOpToType h m op = do
  let (domList, cod) = getTypeInfo op
  let domList' = map (Term.fromPrimNum m) domList
  xs <- mapM (const (Gensym.newIdentFromText (gensymHandle h) "_")) domList'
  let xts = zipWith (\x t -> (m, x, t)) xs domList'
  let cod' = Term.fromPrimNum m cod
  return $ m :< TM.Pi PK.normal [] xts cod'

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
      let dataArgs' = ImpArgs.FullySpecified $ map fst typedDataArgs'
      consTerm@(_, consType) <- infer h $ m :< WT.PiElim False (m :< WT.VarGlobal attr consDD) dataArgs' []
      let impConsArgs = ImpArgs.FullySpecified []
      let expConsArgs = map (\(mx, x, t) -> (mx :< WT.Var x, t)) consArgs'
      if isConstLike
        then do
          liftIO $ Constraint.insert (constraintHandle h) cursorType consType
        else do
          (_, tPat) <- inferPiElim h m consTerm impConsArgs expConsArgs
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
  sub <- Unify.unifyCurrentConstraints h
  reduceWeakType' h sub t

reduceWeakType' :: Handle -> HS.HoleSubst -> WT.WeakTerm -> EIO WT.WeakTerm
reduceWeakType' h sub e = do
  e' <- reduce h e
  case e' of
    m :< WT.Hole hole es ->
      case HS.lookup hole sub of
        Nothing ->
          return e'
        Just (xs, body)
          | length xs == length es -> do
              let s = IntMap.fromList $ zip (map Ident.toInt xs) (map Right es)
              liftIO (Subst.subst (substHandle h) s body) >>= reduceWeakType' h sub
          | otherwise ->
              raiseError m "Arity mismatch"
    m :< WT.PiElim False (_ :< WT.VarGlobal _ name) impArgs args -> do
      lamOrNone <- liftIO $ lookupDefinition h name
      case lamOrNone of
        Just lam ->
          reduceWeakType' h sub $ m :< WT.PiElim False lam impArgs args
        Nothing -> do
          return e'
    _ ->
      return e'

lookupDefinition :: Handle -> DD.DefiniteDescription -> IO (Maybe WT.WeakTerm)
lookupDefinition h name = do
  WeakDef.lookup' (weakDefHandle h) name

newHole :: Handle -> Hint -> BoundVarEnv -> IO WT.WeakTerm
newHole h m varEnv = do
  (e, _) <- newTypedHole h m varEnv
  return e

createImpArgValue :: Handle -> Hint -> (BinderF WT.WeakTerm, Maybe WT.WeakTerm) -> EIO WT.WeakTerm
createImpArgValue h m ((_, _, paramType), maybeDefault) =
  case maybeDefault of
    Just defaultValue -> do
      (defaultValue', defaultType) <- infer h defaultValue
      liftIO $ Constraint.insert (constraintHandle h) paramType defaultType
      return defaultValue'
    Nothing -> do
      (holeValue, holeType) <- liftIO $ newTypedHole h m (varEnv h)
      liftIO $ Constraint.insert (constraintHandle h) paramType holeType
      return holeValue

createImpArgValueFromParam ::
  Handle ->
  Hint ->
  (BinderF WT.WeakTerm, Maybe WT.WeakTerm) ->
  EIO (WT.WeakTerm, WT.WeakTerm)
createImpArgValueFromParam h m ((_, _, paramType), maybeDefault) =
  case maybeDefault of
    Just defaultValue -> do
      (defaultValue', defaultType) <- infer h defaultValue
      liftIO $ Constraint.insert (constraintHandle h) paramType defaultType
      return (defaultValue', defaultType)
    Nothing -> do
      liftIO $ newTypedHole h m (varEnv h)
