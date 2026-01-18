{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list comprehension" #-}

module Kernel.Elaborate.Internal.Infer
  ( Handle,
    inferStmt,
  )
where

import App.App (App)
import App.Run (raiseError)
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.HashMap.Strict qualified as Map
import Data.IntMap qualified as IntMap
import Data.Set qualified as S
import Data.Text qualified as T
import Gensym.Gensym qualified as Gensym
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.Handle.Global.Type qualified as Type
import Kernel.Common.ReadableDD
import Kernel.Elaborate.Internal.Handle.Constraint qualified as Constraint
import Kernel.Elaborate.Internal.Handle.Elaborate
import Kernel.Elaborate.Internal.Handle.Hole qualified as Hole
import Kernel.Elaborate.Internal.Handle.WeakDecl qualified as WeakDecl
import Kernel.Elaborate.Internal.Handle.WeakType qualified as WeakType
import Kernel.Elaborate.Internal.Handle.WeakTypeDef qualified as WeakTypeDef
import Kernel.Elaborate.Internal.Unify qualified as Unify
import Kernel.Elaborate.TypeHoleSubst qualified as THS
import Language.Common.Annotation qualified as Annotation
import Language.Common.ArgNum qualified as AN
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.Attr.VarGlobal qualified as AttrVG
import Language.Common.BaseName qualified as BN
import Language.Common.Binder
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.DecisionTree qualified as DT
import Language.Common.DefaultArgs qualified as DefaultArgs
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.Geist qualified as G
import Language.Common.HoleID qualified as HID
import Language.Common.Ident (isHole)
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.ImpArgs qualified as ImpArgs
import Language.Common.LamKind qualified as LK
import Language.Common.Literal qualified as L
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.Common.PiKind qualified as PK
import Language.Common.PrimOp
import Language.Common.PrimType qualified as PT
import Language.Common.SourceLocator qualified as SL
import Language.Common.StmtKind qualified as SK
import Language.Common.StrictGlobalLocator qualified as SGL
import Language.LowComp.DeclarationName qualified as DN
import Language.WeakTerm.CreateHole qualified as WT
import Language.WeakTerm.FreeVars (freeVars)
import Language.WeakTerm.Subst (SubstEntry (..))
import Language.WeakTerm.Subst qualified as Subst
import Language.WeakTerm.ToText (toTextType)
import Language.WeakTerm.WeakPrimValue qualified as WPV
import Language.WeakTerm.WeakStmt
import Language.WeakTerm.WeakTerm qualified as WT
import Logger.Hint

type BoundVarEnv = [BinderF WT.WeakType]

inferStmt :: Handle -> WeakStmt -> App WeakStmt
inferStmt h stmt =
  case stmt of
    WeakStmtDefineTerm isConstLike stmtKind m x impArgs expArgs defaultArgs codType e -> do
      (impArgs', h') <- inferImpBinder h impArgs
      (expArgs', h'') <- inferBinder' h' expArgs
      (defaultArgs', h''') <- inferImpBinderWithDefaults h'' defaultArgs
      let defaultBinders = map fst defaultArgs'
      codType' <- inferType h''' codType
      case stmtKind of
        SK.Macro -> do
          forM_ (expArgs' ++ map fst defaultArgs') $ \(mx, _, t) ->
            checkIsCodeType h''' mx t
        _ ->
          return ()
      case stmtKind of
        SK.DataIntro {} -> do
          liftIO $ insertType h''' x $ m :< WT.Pi (PK.DataIntro isConstLike) impArgs' expArgs' defaultBinders codType'
        _ ->
          liftIO $ insertType h''' x $ m :< WT.Pi (PK.Normal isConstLike) impArgs' expArgs' defaultBinders codType'
      stmtKind' <- inferStmtKindTerm h''' stmtKind
      (e', te) <- infer h''' e
      liftIO $ Constraint.insert (constraintHandle h''') codType' te
      case getMainUnitType stmtKind of
        Just unitType -> do
          let expected = m :< WT.Pi PK.normal [] [] [] unitType
          let actual = m :< WT.Pi PK.normal impArgs' expArgs' defaultBinders codType'
          liftIO $ Constraint.insert (constraintHandle h''') expected actual
        Nothing ->
          return ()
      return $ WeakStmtDefineTerm isConstLike stmtKind' m x impArgs' expArgs' defaultArgs' codType' e'
    WeakStmtDefineType isConstLike stmtKind m x impArgs expArgs defaultArgs codType body -> do
      (impArgs', h') <- inferImpBinder h impArgs
      (expArgs', h'') <- inferBinder' h' expArgs
      (defaultArgs', h''') <- inferImpBinderWithDefaults h'' defaultArgs
      let defaultBinders = map fst defaultArgs'
      codType' <- inferType h''' codType
      liftIO $ insertType h''' x $ m :< WT.Pi (PK.Normal isConstLike) impArgs' expArgs' defaultBinders codType'
      body' <- inferType h''' body
      stmtKind' <- inferStmtKindType h''' stmtKind
      return $ WeakStmtDefineType isConstLike stmtKind' m x impArgs' expArgs' defaultArgs' codType' body'
    WeakStmtDefineResource m dd resourceID unitType discarder copier -> do
      unitType' <- inferType h unitType
      (discarder', _) <- infer h discarder
      (copier', _) <- infer h copier
      let piType = m :< WT.Pi (PK.Normal True) [] [] [] (m :< WT.Tau)
      liftIO $ insertType h dd piType
      return $ WeakStmtDefineResource m dd resourceID unitType' discarder' copier'
    WeakStmtVariadic kind m dd -> do
      return $ WeakStmtVariadic kind m dd
    WeakStmtNominal m geistList -> do
      geistList' <- forM geistList $ \(tag, geist) -> do
        geist' <- inferGeist h geist
        return (tag, geist')
      return $ WeakStmtNominal m geistList'
    WeakStmtForeign foreignList ->
      return $ WeakStmtForeign foreignList

inferGeist :: Handle -> G.Geist WT.WeakType WT.WeakTerm -> App (G.Geist WT.WeakType WT.WeakTerm)
inferGeist h (G.Geist {..}) = do
  (impArgs', h') <- inferImpBinder h impArgs
  (expArgs', h'') <- inferBinder' h' expArgs
  (defaultArgs', h''') <- inferImpBinderWithDefaults h'' defaultArgs
  let defaultBinders = map fst defaultArgs'
  cod' <- inferType h''' cod
  liftIO $ insertType h''' name $ loc :< WT.Pi PK.normal impArgs' expArgs' defaultBinders cod'
  return $ G.Geist {impArgs = impArgs', defaultArgs = defaultArgs', expArgs = expArgs', cod = cod', ..}

insertType :: Handle -> DD.DefiniteDescription -> WT.WeakType -> IO ()
insertType h dd t = do
  typeOrNone <- Type.lookupMaybe' (typeHandle h) dd
  case typeOrNone of
    Nothing ->
      return ()
    Just declaredType -> do
      Constraint.insert (constraintHandle h) declaredType t
  Type.insert' (typeHandle h) dd t

inferStmtKindTerm :: Handle -> SK.StmtKindTerm WT.WeakType -> App (SK.StmtKindTerm WT.WeakType)
inferStmtKindTerm h stmtKind =
  case stmtKind of
    SK.Define ->
      return SK.Define
    SK.Inline ->
      return SK.Inline
    SK.Macro ->
      return SK.Macro
    SK.MacroInline ->
      return SK.MacroInline
    SK.Main t -> do
      t' <- inferType h t
      return $ SK.Main t'
    SK.DataIntro consName dataArgs expConsArgs discriminant -> do
      (dataArgs', varEnv) <- inferBinder' h dataArgs
      (expConsArgs', _) <- inferBinder' varEnv expConsArgs
      return $ SK.DataIntro consName dataArgs' expConsArgs' discriminant

inferStmtKindType :: Handle -> SK.StmtKindType WT.WeakType -> App (SK.StmtKindType WT.WeakType)
inferStmtKindType h stmtKind =
  case stmtKind of
    SK.Alias ->
      return SK.Alias
    SK.AliasOpaque ->
      return SK.AliasOpaque
    SK.Data dataName dataArgs consInfoList -> do
      (dataArgs', varEnv) <- inferBinder' h dataArgs
      consInfoList' <- forM consInfoList $ \(m, dd, constLike, consArgs, discriminant) -> do
        (consArgs', _) <- inferBinder' varEnv consArgs
        return (m, dd, constLike, consArgs', discriminant)
      return $ SK.Data dataName dataArgs' consInfoList'

getIntType :: Platform.Handle -> Hint -> App WT.WeakType
getIntType h m = do
  let baseSize = Platform.getDataSize h
  return $ WT.intTypeBySize m baseSize

getMainUnitType :: SK.StmtKindTerm WT.WeakType -> Maybe WT.WeakType
getMainUnitType stmtKind =
  case stmtKind of
    SK.Main unitType ->
      return unitType
    _ ->
      Nothing

extendHandle :: BinderF WT.WeakType -> Handle -> Handle
extendHandle (mx, x, t) h =
  if isHole x then h else h {varEnv = (mx, x, t) : varEnv h}

infer :: Handle -> WT.WeakTerm -> App (WT.WeakTerm, WT.WeakType)
infer h term =
  case term of
    m :< WT.Var x -> do
      _ :< t <- WeakType.lookup (weakTypeHandle h) m x
      return (term, m :< t)
    m :< WT.VarGlobal _ name -> do
      _ :< t <- Type.lookup' (typeHandle h) m name
      return (term, m :< t)
    m :< WT.PiIntro attr@(AttrL.Attr {lamKind}) impArgs expArgs defaultArgs e -> do
      case lamKind of
        LK.Fix opacity (mx, x, codType) -> do
          (impArgs', h') <- inferImpBinder h impArgs
          (expArgs', h'') <- inferBinder' h' expArgs
          (defaultArgs', h''') <- inferImpBinderWithDefaults h'' defaultArgs
          let defaultBinders = map fst defaultArgs'
          codType' <- inferType h''' codType
          let piType = m :< WT.Pi PK.normal impArgs' expArgs' defaultBinders codType'
          liftIO $ WeakType.insert (weakTypeHandle h) x piType
          (e', tBody) <- infer h''' e
          liftIO $ Constraint.insert (constraintHandle h''') codType' tBody
          let term' = m :< WT.PiIntro (attr {AttrL.lamKind = LK.Fix opacity (mx, x, codType')}) impArgs' expArgs' defaultArgs' e'
          return (term', piType)
        LK.Normal name codType -> do
          (impArgs', h') <- inferImpBinder h impArgs
          (expArgs', h'') <- inferBinder' h' expArgs
          (defaultArgs', h''') <- inferImpBinderWithDefaults h'' defaultArgs
          let defaultBinders = map fst defaultArgs'
          codType' <- inferType h''' codType
          (e', t') <- infer h''' e
          liftIO $ Constraint.insert (constraintHandle h''') codType' t'
          let term' = m :< WT.PiIntro (attr {AttrL.lamKind = LK.Normal name codType'}) impArgs' expArgs' defaultArgs' e'
          return (term', m :< WT.Pi PK.normal impArgs' expArgs' defaultBinders t')
    m :< WT.PiElim _ e impArgs expArgs defaultArgs -> do
      etl <- infer h e
      impArgs' <- ImpArgs.traverseImpArgs (inferType h) impArgs
      defaultArgs' <- DefaultArgs.traverseDefaultArgs (infer h) defaultArgs
      expArgs' <- mapM (infer h) expArgs
      inferPiElim h m etl impArgs' defaultArgs' expArgs'
    m :< WT.PiElimExact e -> do
      (e', t) <- infer h e
      t' <- resolveType h t
      case t' of
        _ :< WT.Pi _ impArgs expArgs defaultArgs codType -> do
          impArgs' <- mapM (const $ liftIO $ newTypeHole h m (varEnv h)) impArgs
          let impIds = map (\(_, x, _) -> x) impArgs
          let subType = IntMap.fromList $ zip (map Ident.toInt impIds) (map Type impArgs')
          (expArgs', subType') <- liftIO $ Subst.subst' (substHandle h) subType expArgs
          let expArgs'' = map (\(mx, x, _) -> mx :< WT.Var x) expArgs'
          unless (null defaultArgs) $ do
            raiseError m "exact application does not support default parameters"
          codType' <- liftIO $ Subst.substType (substHandle h) subType' codType
          lamID <- liftIO $ Gensym.newCount (gensymHandle h)
          infer h $ m :< WT.PiIntro (AttrL.normal lamID codType') [] expArgs' [] (m :< WT.PiElim False e' (ImpArgs.FullySpecified impArgs') expArgs'' (DefaultArgs.ByKey []))
        _ ->
          raiseError m $ "Expected a function type, but got: " <> toTextType t'
    m :< WT.DataIntro attr@(AttrDI.Attr {..}) consName dataArgs consArgs -> do
      (dataArgs', _) <- mapAndUnzipM (inferTypeWithKind h) dataArgs
      (consArgs', _) <- mapAndUnzipM (infer h) consArgs
      consNameList' <- forM consNameList $ \(cn, binders, cl) -> do
        binders' <- inferBinder'' h binders
        return (cn, binders', cl)
      let attr' = attr {AttrDI.consNameList = consNameList'}
      let dataType = m :< WT.Data (AttrD.Attr {consNameList = consNameList', isConstLike}) dataName dataArgs'
      return (m :< WT.DataIntro attr' consName dataArgs' consArgs', dataType)
    m :< WT.DataElim isNoetic oets tree -> do
      let (os, es, _) = unzip3 oets
      (es', ts') <- mapAndUnzipM (infer h) es
      liftIO $ forM_ (zip os ts') $ uncurry $ WeakType.insert (weakTypeHandle h)
      (tree', treeType) <- inferDecisionTree m h tree
      return (m :< WT.DataElim isNoetic (zip3 os es' ts') tree', treeType)
    m :< WT.BoxIntro letSeq e -> do
      letSeq' <- inferQuoteSeq h letSeq FromNoema
      (e', t) <- infer h e
      return (m :< WT.BoxIntro letSeq' e', m :< WT.Box t)
    m :< WT.BoxIntroLift e -> do
      (e', t) <- infer h e
      liftIO $ Constraint.insertActualityConstraint (constraintHandle h) t
      return (m :< WT.BoxIntroLift e', m :< WT.Box t)
    m :< WT.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      castSeq' <- inferQuoteSeq h castSeq ToNoema
      (e1', t1) <- infer h e1
      mxt'@(mx, _, t1') <- inferBinder1 h mxt
      liftIO $ Constraint.insert (constraintHandle h) (mx :< WT.Box t1') t1
      uncastSeq' <- inferQuoteSeq h uncastSeq FromNoema
      (e2', t2) <- infer h e2
      return (m :< WT.BoxElim castSeq' mxt' e1' uncastSeq' e2', t2)
    m :< WT.CodeIntro e -> do
      (e', t) <- infer h e
      return (m :< WT.CodeIntro e', m :< WT.Code t)
    m :< WT.CodeElim e -> do
      (e', t1) <- infer h e
      tInner <- liftIO $ newTypeHole h m (varEnv h)
      liftIO $ Constraint.insert (constraintHandle h) (m :< WT.Code tInner) t1
      return (m :< WT.CodeElim e', tInner)
    m :< WT.TauIntro ty -> do
      ty' <- inferType h ty
      return (m :< WT.TauIntro ty', m :< WT.Tau)
    m :< WT.TauElim (mx, x) e1 e2 -> do
      (e1', t1') <- infer h e1
      let tau = mx :< WT.Tau
      liftIO $ Constraint.insert (constraintHandle h) tau t1'
      liftIO $ WeakType.insert (weakTypeHandle h) x tau
      let h' = extendHandle (mx, x, tau) h
      (e2', t2') <- infer h' e2
      return (m :< WT.TauElim (mx, x) e1' e2', t2')
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
      liftIO $ Constraint.insert (constraintHandle h) t' t1'
      (e2', t2') <- infer h e2
      return (m :< WT.Let opacity (mx, x, t') e1' e2', t2')
    m :< WT.Prim prim ->
      case prim of
        WPV.Int t v -> do
          t' <- inferType (h {varEnv = []}) t
          return (m :< WT.Prim (WPV.Int t' v), t')
        WPV.Float t v -> do
          t' <- inferType (h {varEnv = []}) t
          return (m :< WT.Prim (WPV.Float t' v), t')
        WPV.Op op -> do
          primOpType <- liftIO $ primOpToType h m op
          return (m :< WT.Prim prim, primOpType)
        WPV.StaticText t text -> do
          t' <- inferType (h {varEnv = []}) t
          return (m :< WT.Prim (WPV.StaticText t' text), m :< WT.BoxNoema t')
        WPV.Rune _ -> do
          return (m :< WT.Prim prim, m :< WT.PrimType PT.Rune)
    m :< WT.Magic (M.WeakMagic magic) -> do
      case magic of
        M.LowMagic lowMagic -> do
          case lowMagic of
            LM.Cast from to value -> do
              from' <- inferType h from
              to' <- inferType h to
              (value', t) <- infer h value
              liftIO $ Constraint.insert (constraintHandle h) from' t
              return (m :< WT.Magic (M.WeakMagic $ M.LowMagic $ LM.Cast from' to' value'), to')
            LM.Store t unit value pointer -> do
              t' <- inferType h t
              unit' <- inferType h unit
              (value', tValue) <- infer h value
              (pointer', tPointer) <- infer h pointer
              liftIO $ Constraint.insert (constraintHandle h) t' tValue
              liftIO $ Constraint.insert (constraintHandle h) (m :< WT.PrimType PT.Pointer) tPointer
              return (m :< WT.Magic (M.WeakMagic $ M.LowMagic $ LM.Store t' unit' value' pointer'), unit')
            LM.Load t pointer -> do
              t' <- inferType h t
              (pointer', tPointer) <- infer h pointer
              liftIO $ Constraint.insert (constraintHandle h) (m :< WT.PrimType PT.Pointer) tPointer
              return (m :< WT.Magic (M.WeakMagic $ M.LowMagic $ LM.Load t' pointer'), t')
            LM.Alloca lt size -> do
              (size', sizeType) <- infer h size
              intType <- getIntType (platformHandle h) m
              lt' <- inferType h lt
              liftIO $ Constraint.insert (constraintHandle h) intType sizeType
              return (m :< WT.Magic (M.WeakMagic $ M.LowMagic $ LM.Alloca lt' size'), m :< WT.PrimType PT.Pointer)
            LM.External _ _ funcName args varArgs -> do
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
                FCT.Cod c -> do
                  return (m :< WT.Magic (M.WeakMagic $ M.LowMagic $ LM.External domList (FCT.Cod c) funcName args' varArgs'), c)
                FCT.Void -> do
                  let voidType = m :< WT.Void
                  return (m :< WT.Magic (M.WeakMagic $ M.LowMagic $ LM.External domList FCT.Void funcName args' varArgs'), voidType)
            LM.Global name t -> do
              t' <- inferType h t
              return (m :< WT.Magic (M.WeakMagic $ M.LowMagic $ LM.Global name t'), t')
            LM.OpaqueValue e -> do
              (e', t) <- infer h e
              return (m :< WT.Magic (M.WeakMagic $ M.LowMagic $ LM.OpaqueValue e'), t)
            LM.CallType func arg1 arg2 -> do
              (func', _) <- infer h func
              (arg1', t1) <- infer h arg1
              (arg2', _) <- infer h arg2
              intType <- getIntType (platformHandle h) m
              liftIO $ Constraint.insert (constraintHandle h) intType t1
              resultType <- liftIO $ newTypeHole h m (varEnv h)
              return (m :< WT.Magic (M.WeakMagic $ M.LowMagic $ LM.CallType func' arg1' arg2'), resultType)
        M.GetTypeTag mid typeTagExpr typeExpr -> do
          typeTagExpr' <- inferType h typeTagExpr
          typeExpr' <- inferType h typeExpr
          return (m :< WT.Magic (M.WeakMagic $ M.GetTypeTag mid typeTagExpr' typeExpr'), typeTagExpr')
        M.GetDataArgs sgl listExpr typeExpr -> do
          (listExpr', _) <- inferTypeWithKind h listExpr
          typeExpr' <- inferType h typeExpr
          listType <- inferType h $ m :< WT.TyApp listExpr' [m :< WT.Tau]
          return (m :< WT.Magic (M.WeakMagic $ M.GetDataArgs sgl listExpr' typeExpr'), listType)
        M.GetConsSize typeExpr -> do
          typeExpr' <- inferType h typeExpr
          intType <- getIntType (platformHandle h) m
          return (m :< WT.Magic (M.WeakMagic $ M.GetConsSize typeExpr'), intType)
        M.GetWrapperContentType typeExpr -> do
          typeExpr' <- inferType h typeExpr
          return (m :< WT.Magic (M.WeakMagic $ M.GetWrapperContentType typeExpr'), m :< WT.Tau)
        M.GetVectorContentType sgl typeExpr -> do
          typeExpr' <- inferType h typeExpr
          return (m :< WT.Magic (M.WeakMagic $ M.GetVectorContentType sgl typeExpr'), m :< WT.Tau)
        M.GetNoemaContentType typeExpr -> do
          typeExpr' <- inferType h typeExpr
          return (m :< WT.Magic (M.WeakMagic $ M.GetNoemaContentType typeExpr'), m :< WT.Tau)
        M.GetBoxContentType typeExpr -> do
          typeExpr' <- inferType h typeExpr
          return (m :< WT.Magic (M.WeakMagic $ M.GetBoxContentType typeExpr'), m :< WT.Tau)
        M.GetConstructorArgTypes sgl listExpr typeExpr index -> do
          (listExpr', _) <- inferTypeWithKind h listExpr
          typeExpr' <- inferType h typeExpr
          (index', indexType) <- infer h index
          intType <- getIntType (platformHandle h) m
          liftIO $ Constraint.insert (constraintHandle h) intType indexType
          listType <- inferType h $ m :< WT.TyApp listExpr' [m :< WT.Tau]
          return (m :< WT.Magic (M.WeakMagic $ M.GetConstructorArgTypes sgl listExpr' typeExpr' index'), listType)
        M.GetConsName textType typeExpr index -> do
          textType' <- inferType h textType
          typeExpr' <- inferType h typeExpr
          (index', indexType) <- infer h index
          intType <- getIntType (platformHandle h) m
          liftIO $ Constraint.insert (constraintHandle h) intType indexType
          return (m :< WT.Magic (M.WeakMagic $ M.GetConsName textType' typeExpr' index'), m :< WT.BoxNoema textType')
        M.GetConsConstFlag boolType typeExpr index -> do
          boolType' <- inferType h boolType
          typeExpr' <- inferType h typeExpr
          (index', indexType) <- infer h index
          intType <- getIntType (platformHandle h) m
          liftIO $ Constraint.insert (constraintHandle h) intType indexType
          return (m :< WT.Magic (M.WeakMagic $ M.GetConsConstFlag boolType' typeExpr' index'), boolType')
        M.ShowType textTypeExpr typeExpr -> do
          textTypeExpr' <- inferType h textTypeExpr
          typeExpr' <- inferType h typeExpr
          return (m :< WT.Magic (M.WeakMagic $ M.ShowType textTypeExpr' typeExpr'), m :< WT.BoxNoema textTypeExpr')
        M.TextCons textTypeExpr rune text -> do
          textTypeExpr' <- inferType h textTypeExpr
          (rune', runeType) <- infer h rune
          (text', textType) <- infer h text
          liftIO $ Constraint.insert (constraintHandle h) (m :< WT.PrimType PT.Rune) runeType
          liftIO $ Constraint.insert (constraintHandle h) (m :< WT.BoxNoema textTypeExpr') textType
          return (m :< WT.Magic (M.WeakMagic $ M.TextCons textTypeExpr' rune' text'), m :< WT.BoxNoema textTypeExpr')
        M.TextUncons moduleID text -> do
          (text', textType) <- infer h text
          let textSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.textLocator}
          let textDD = DD.newByGlobalLocator textSGL BN.textType
          let expected = m :< WT.TVarGlobal (AttrVG.Attr {argNum = AN.zero, isConstLike = True}) textDD
          liftIO $ Constraint.insert (constraintHandle h) (m :< WT.BoxNoema expected) textType
          let eitherSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.eitherLocator}
          let unitSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.unitLocator}
          let pairSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.pairLocator}
          let eitherTypeDD = DD.newByGlobalLocator eitherSGL BN.eitherType
          let unitTypeDD = DD.newByGlobalLocator unitSGL BN.unitType
          let pairTypeDD = DD.newByGlobalLocator pairSGL BN.pairType
          let eitherTypeVar = m :< WT.TVarGlobal (AttrVG.Attr {argNum = AN.fromInt 3, isConstLike = False}) eitherTypeDD
          let unitTypeVar = m :< WT.TVarGlobal (AttrVG.Attr {argNum = AN.zero, isConstLike = True}) unitTypeDD
          let pairTypeVar = m :< WT.TVarGlobal (AttrVG.Attr {argNum = AN.fromInt 4, isConstLike = False}) pairTypeDD
          let runeType = m :< WT.PrimType PT.Rune
          let pairType = m :< WT.TyApp pairTypeVar [runeType, textType]
          let eitherType = m :< WT.TyApp eitherTypeVar [unitTypeVar, pairType]
          return (m :< WT.Magic (M.WeakMagic $ M.TextUncons moduleID text'), eitherType)
        M.CompileError typeExpr msg -> do
          typeExpr' <- inferType h typeExpr
          (msg', msgType) <- infer h msg
          liftIO $ Constraint.insert (constraintHandle h) (m :< WT.BoxNoema typeExpr') msgType
          resultType <- liftIO $ newTypeHole h m (varEnv h)
          return (m :< WT.Magic (M.WeakMagic $ M.CompileError typeExpr' msg'), resultType)
    m :< WT.Annotation logLevel annot e -> do
      (e', t) <- infer h e
      case annot of
        Annotation.Type _ -> do
          return (m :< WT.Annotation logLevel (Annotation.Type t) e', t)

inferType :: Handle -> WT.WeakType -> App WT.WeakType
inferType h ty = do
  (ty', kind) <- inferTypeWithKind h ty
  let tau = WT.metaOfType ty :< WT.Tau
  liftIO $ Constraint.insert (constraintHandle h) tau kind
  return ty'

inferTypeWithKind :: Handle -> WT.WeakType -> App (WT.WeakType, WT.WeakType)
inferTypeWithKind h ty =
  case ty of
    _ :< WT.Tau ->
      return (ty, ty)
    m :< WT.TVar x -> do
      _ :< k <- WeakType.lookup (weakTypeHandle h) m x
      return (ty, m :< k)
    m :< WT.TVarGlobal _ name -> do
      _ :< k <- Type.lookup' (typeHandle h) m name
      return (ty, m :< k)
    m :< WT.TyApp t args -> do
      (t', k) <- inferTypeWithKind h t
      argsWithKinds <- mapM (inferTypeWithKind h) args
      let args' = map fst argsWithKinds
      let argKinds = map snd argsWithKinds
      k' <- resolveType h k
      case k' of
        _ :< WT.Pi _ impParams expParams _ cod -> do
          let impParamIds = map (\(_, x, _) -> x) impParams
          impParams' <- mapM (const $ liftIO $ newTypeHole h m (varEnv h)) impParams
          let subType = IntMap.fromList $ zip (map Ident.toInt impParamIds) (map Type impParams')
          (expParams', _) <- substTypeBinder h subType expParams
          ensureTypeArityCorrectness m (length expParams') (length args')
          forM_ (zip expParams' argKinds) $ \((_, _, tParam), tArg) ->
            liftIO $ Constraint.insert (constraintHandle h) tParam tArg
          _ :< cod' <- liftIO $ Subst.substType (substHandle h) subType cod
          return (m :< WT.TyApp t' args', m :< cod')
        _ ->
          raiseError m $ "Expected a function type, but got: " <> toTextType k'
    m :< WT.Pi piKind impArgs expArgs defaultArgs t -> do
      (impArgs', h') <- inferImpBinder h impArgs
      (expArgs', h'') <- inferBinder' h' expArgs
      (defaultArgs', h''') <- inferBinder' h'' defaultArgs
      t' <- inferType h''' t
      return (m :< WT.Pi piKind impArgs' expArgs' defaultArgs' t', m :< WT.Tau)
    m :< WT.Data attr name es -> do
      (es', _) <- mapAndUnzipM (inferTypeWithKind h) es
      attr' <- inferAttrData h attr
      return (m :< WT.Data attr' name es', m :< WT.Tau)
    m :< WT.Box t -> do
      t' <- inferType h t
      return (m :< WT.Box t', m :< WT.Tau)
    m :< WT.BoxNoema t -> do
      t' <- inferType h t
      return (m :< WT.BoxNoema t', m :< WT.Tau)
    m :< WT.Code t -> do
      t' <- inferType h t
      return (m :< WT.Code t', m :< WT.Tau)
    m :< WT.PrimType {} ->
      return (ty, m :< WT.Tau)
    m :< WT.Void ->
      return (ty, m :< WT.Tau)
    m :< WT.Resource dd resourceID -> do
      return (m :< WT.Resource dd resourceID, m :< WT.Tau)
    m :< WT.TypeHole holeID _ -> do
      let rawHoleID = HID.reify holeID
      mHoleInfo <- liftIO $ Hole.lookup (holeHandle h) rawHoleID
      case mHoleInfo of
        Just (_ :< holeTerm, _ :< holeType) -> do
          return (m :< holeTerm, m :< holeType)
        Nothing -> do
          let holeArgs = map (\(mx, x, _) -> mx :< WT.TVar x) (varEnv h)
          let holeTerm = m :< WT.TypeHole holeID holeArgs
          holeType <- liftIO $ WT.createTypeHole (gensymHandle h) m holeArgs
          liftIO $ Hole.insert (holeHandle h) rawHoleID holeTerm holeType
          return (holeTerm, holeType)

inferAttrData :: Handle -> AttrD.Attr DD.DefiniteDescription (BinderF WT.WeakType) -> App (AttrD.Attr DD.DefiniteDescription (BinderF WT.WeakType))
inferAttrData h attr = do
  let consNameList = AttrD.consNameList attr
  consNameList' <- forM consNameList $ \(cn, binders, cl) -> do
    binders' <- inferBinder'' h binders
    return (cn, binders', cl)
  return $ attr {AttrD.consNameList = consNameList'}

inferImpBinder :: Handle -> [BinderF WT.WeakType] -> App ([BinderF WT.WeakType], Handle)
inferImpBinder h binderList =
  case binderList of
    [] ->
      return ([], h)
    (mx, x, t) : rest -> do
      t' <- inferType h t
      liftIO $ WeakType.insert (weakTypeHandle h) x t'
      let h' = extendHandle (mx, x, t') h
      (rest', h'') <- inferImpBinder h' rest
      return ((mx, x, t') : rest', h'')

inferImpBinderWithDefaults :: Handle -> [(BinderF WT.WeakType, WT.WeakTerm)] -> App ([(BinderF WT.WeakType, WT.WeakTerm)], Handle)
inferImpBinderWithDefaults h binderList =
  case binderList of
    [] ->
      return ([], h)
    ((mx, x, t), defaultValue) : rest -> do
      t' <- inferType h t
      (defaultValue', defaultType) <- infer h defaultValue
      unless (S.null (freeVars defaultValue')) $ do
        raiseError mx "Default argument must be closed"
      liftIO $ Constraint.insert (constraintHandle h) t' defaultType
      liftIO $ WeakType.insert (weakTypeHandle h) x t'
      (rest', h') <- inferImpBinderWithDefaults h rest
      return (((mx, x, t'), defaultValue') : rest', h')

inferBinder' :: Handle -> [BinderF WT.WeakType] -> App ([BinderF WT.WeakType], Handle)
inferBinder' h binder =
  case binder of
    [] ->
      return ([], h)
    (mx, x, t) : xts -> do
      t' <- inferType h t
      liftIO $ WeakType.insert (weakTypeHandle h) x t'
      (xts', h') <- inferBinder' h xts
      return ((mx, x, t') : xts', h')

inferBinder'' :: Handle -> [BinderF WT.WeakType] -> App [BinderF WT.WeakType]
inferBinder'' h binder =
  case binder of
    [] ->
      return []
    (mx, x, t) : xts -> do
      t' <- inferType h t
      liftIO $ WeakType.insert (weakTypeHandle h) x t'
      xts' <- inferBinder'' h xts
      return $ (mx, x, t') : xts'

inferBinder1 :: Handle -> BinderF WT.WeakType -> App (BinderF WT.WeakType)
inferBinder1 h (mx, x, t) = do
  t' <- inferType h t
  liftIO $ WeakType.insert (weakTypeHandle h) x t'
  return (mx, x, t')

inferQuoteSeq :: Handle -> [(BinderF WT.WeakType, WT.WeakTerm)] -> CastDirection -> App [(BinderF WT.WeakType, WT.WeakTerm)]
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

inferArgsTypes ::
  Handle ->
  Subst.Subst ->
  Hint ->
  [(WT.WeakType, WT.WeakType)] ->
  [BinderF WT.WeakType] ->
  App Subst.Subst
inferArgsTypes h sub m args params =
  case (args, params) of
    ([], []) ->
      return sub
    ((ty, tyKind) : rest, (_, x, paramKind) : xts) -> do
      paramKind' <- liftIO $ Subst.substType (substHandle h) sub paramKind
      liftIO $ Constraint.insert (constraintHandle h) paramKind' tyKind
      inferArgsTypes h (IntMap.insert (Ident.toInt x) (Type ty) sub) m rest xts
    _ ->
      raiseError m "Invalid argument passed to inferArgsTypes"

inferArgsTerms ::
  Handle ->
  Subst.Subst ->
  Hint ->
  [(WT.WeakTerm, WT.WeakType)] ->
  [BinderF WT.WeakType] ->
  WT.WeakType ->
  App WT.WeakType
inferArgsTerms h sub m args params cod =
  case (args, params) of
    ([], []) ->
      liftIO $ Subst.substType (substHandle h) sub cod
    ((_, argType) : rest, (_, _, paramType) : xts) -> do
      paramType' <- liftIO $ Subst.substType (substHandle h) sub paramType
      liftIO $ Constraint.insert (constraintHandle h) paramType' argType
      inferArgsTerms h sub m rest xts cod
    _ ->
      raiseError m "Invalid argument passed to inferArgsTerms"

inferPiElim ::
  Handle ->
  Hint ->
  (WT.WeakTerm, WT.WeakType) ->
  ImpArgs.ImpArgs WT.WeakType ->
  DefaultArgs.DefaultArgs (WT.WeakTerm, WT.WeakType) ->
  [(WT.WeakTerm, WT.WeakType)] ->
  App (WT.WeakTerm, WT.WeakType)
inferPiElim h m (e, t) impArgs defaultArgsSpec expArgs = do
  t' <- resolveType h t
  case t' of
    _ :< WT.Pi _ impArgsParam expParams defaultParams cod -> do
      inferCore False impArgsParam expParams defaultParams cod
    _ :< WT.BoxNoema (_ :< WT.Pi _ impArgsParam expParams defaultParams cod) ->
      inferCore True impArgsParam expParams defaultParams cod
    _ ->
      raiseError m $ "Expected a function type, but got: " <> toTextType t'
  where
    inferCore isNoetic impArgsParam expParams defaultParams cod = do
      ensureArityCorrectness h e (length expParams) (length expArgs)
      impArgsTyped <- case impArgs of
        ImpArgs.Unspecified ->
          mapM (createImpArgFromParam h m) impArgsParam
        ImpArgs.FullySpecified impArgs' -> do
          ensureImplicitArityCorrectness h e (length impArgsParam) (length impArgs')
          mapM (inferTypeWithKind h) impArgs'
      subType <- inferArgsTypes h IntMap.empty m impArgsTyped impArgsParam
      let impArgs' = map fst impArgsTyped
      let expArgs' = map fst expArgs
      _ :< cod' <- inferArgsTerms h subType m expArgs expParams cod
      defaultArgsOverrides <- resolveDefaultOverrides e defaultParams defaultArgsSpec
      (defaultParams', _) <- substTypeBinder h subType defaultParams
      forM_ (zip defaultParams' defaultArgsOverrides) $ \((_, _, tParam), mOverride) -> do
        case mOverride of
          Just (_, tArg) -> do
            tParam' <- inferType h tParam
            liftIO $ Constraint.insert (constraintHandle h) tParam' tArg
          Nothing ->
            return ()
      let defaultArgsAligned = DefaultArgs.Aligned (map (fmap fst) defaultArgsOverrides)
      return (m :< WT.PiElim isNoetic e (ImpArgs.FullySpecified impArgs') expArgs' defaultArgsAligned, m :< cod')

createImpArgFromParam ::
  Handle ->
  Hint ->
  BinderF WT.WeakType ->
  App (WT.WeakType, WT.WeakType)
createImpArgFromParam h m (_, _, _paramKind) = do
  (holeType, holeKind) <- liftIO $ newTypedHole h m (varEnv h)
  return (holeType, holeKind) -- fixme: constrain paramKind

resolveDefaultOverrides ::
  WT.WeakTerm ->
  [BinderF WT.WeakType] ->
  DefaultArgs.DefaultArgs a ->
  App [Maybe a]
resolveDefaultOverrides function defaultParams defaultArgs =
  let defaultKeys = map (\(_, x, _) -> Ident.toText x) defaultParams
      (m :< _) = function
   in case defaultArgs of
        DefaultArgs.ByKey kvs -> do
          let (ks, _) = unzip kvs
          ensureDefaultKeyLinearity m ks
          checkDefaultKeyRedundancy m defaultKeys ks
          let keyMap = Map.fromList kvs
          return $ map (`Map.lookup` keyMap) defaultKeys
        DefaultArgs.Aligned xs -> do
          when (length xs /= length defaultParams) $ do
            raiseError m "Default argument arity mismatch"
          return xs

newTypeHole :: Handle -> Hint -> BoundVarEnv -> IO WT.WeakType
newTypeHole h m varEnv = do
  fst <$> newTypedHole h m varEnv

newTypedHole :: Handle -> Hint -> BoundVarEnv -> IO (WT.WeakType, WT.WeakType)
newTypedHole h m varEnv = do
  i <- HID.HoleID <$> Gensym.newCount (gensymHandle h)
  j <- HID.HoleID <$> Gensym.newCount (gensymHandle h)
  let holeArgs = map (\(mx, x, _) -> mx :< WT.TVar x) varEnv
  let holeType = m :< WT.TypeHole i holeArgs
  let holeKind = m :< WT.TypeHole j holeArgs
  Hole.insert (holeHandle h) (HID.reify i) holeType holeKind
  return (holeType, holeKind)

data CastDirection
  = FromNoema
  | ToNoema

inferDecisionTree ::
  Hint ->
  Handle ->
  DT.DecisionTree WT.WeakType WT.WeakTerm ->
  App (DT.DecisionTree WT.WeakType WT.WeakTerm, WT.WeakType)
inferDecisionTree m h tree =
  case tree of
    DT.Leaf ys letSeq body -> do
      letSeq' <- mapM (inferLet h) letSeq
      (body', answerType) <- infer h body
      return (DT.Leaf ys letSeq' body', answerType)
    DT.Unreachable -> do
      hole <- liftIO $ newTypeHole h m (varEnv h)
      return (DT.Unreachable, hole)
    DT.Switch (cursor, _) clauseList -> do
      cursorType <- WeakType.lookup (weakTypeHandle h) m cursor
      (clauseList', answerType) <- inferClauseList m h cursorType clauseList
      return (DT.Switch (cursor, cursorType) clauseList', answerType)

inferClauseList :: Hint -> Handle -> WT.WeakType -> DT.CaseList WT.WeakType WT.WeakTerm -> App (DT.CaseList WT.WeakType WT.WeakTerm, WT.WeakType)
inferClauseList m h cursorType (fallbackClause, clauseList) = do
  (clauseList', answerTypeList) <- flip mapAndUnzipM clauseList $ inferClause h cursorType
  let mAns = getClauseHint answerTypeList m
  hole <- liftIO $ newTypeHole h mAns (varEnv h)
  (fallbackClause', fallbackAnswerType) <- inferDecisionTree mAns h fallbackClause
  liftIO $ forM_ (answerTypeList ++ [fallbackAnswerType]) $ Constraint.insert (constraintHandle h) hole
  return ((fallbackClause', clauseList'), fallbackAnswerType)

getClauseHint :: [WT.WeakType] -> Hint -> Hint
getClauseHint ts fallbackHint =
  case ts of
    (m :< _) : _ ->
      m
    _ ->
      fallbackHint

inferClause :: Handle -> WT.WeakType -> DT.Case WT.WeakType WT.WeakTerm -> App (DT.Case WT.WeakType WT.WeakTerm, WT.WeakType)
inferClause h cursorType decisionCase =
  case decisionCase of
    DT.LiteralCase mPat literal cont -> do
      (cont', tCont) <- inferDecisionTree mPat h cont
      case literal of
        L.Int _ ->
          liftIO $ Constraint.insertIntegerConstraint (constraintHandle h) cursorType
        L.Rune _ ->
          liftIO $ Constraint.insert (constraintHandle h) cursorType (mPat :< WT.PrimType PT.Rune)
      return (DT.LiteralCase mPat literal cont', tCont)
    DT.ConsCase record@DT.ConsCaseRecord {..} -> do
      let m = mCons
      let (dataTermList, _) = unzip dataArgs
      typedDataArgs' <- mapM (inferTypeWithKind h) dataTermList
      (consArgs', _) <- inferBinder' h consArgs
      let argNum = AN.fromInt $ length dataArgs + length consArgs
      let attr = AttrVG.Attr {..}
      let dataArgs' = ImpArgs.FullySpecified $ map fst typedDataArgs'
      consTerm@(_, consType) <- infer h $ m :< WT.PiElim False (m :< WT.VarGlobal attr consDD) dataArgs' [] (DefaultArgs.ByKey [])
      if isConstLike
        then liftIO $ Constraint.insert (constraintHandle h) cursorType consType
        else do
          let impConsArgs = ImpArgs.FullySpecified []
          let expConsArgs = map (\(mx, x, t) -> (mx :< WT.Var x, t)) consArgs'
          (_, tPat) <- inferPiElim h m consTerm impConsArgs (DefaultArgs.ByKey []) expConsArgs
          liftIO $ Constraint.insert (constraintHandle h) cursorType tPat
      (cont', tCont) <- inferDecisionTree m h cont
      return
        ( DT.ConsCase
            record
              { DT.dataArgs = typedDataArgs',
                DT.consArgs = consArgs',
                DT.cont = cont'
              },
          tCont
        )

inferLet :: Handle -> (BinderF WT.WeakType, WT.WeakTerm) -> App (BinderF WT.WeakType, WT.WeakTerm)
inferLet h ((mx, x, t), e1) = do
  (e1', t1') <- infer h e1
  t' <- inferType h t >>= resolveType h
  liftIO $ WeakType.insert (weakTypeHandle h) x t'
  liftIO $ Constraint.insert (constraintHandle h) t' t1'
  return ((mx, x, t'), e1')

resolveType :: Handle -> WT.WeakType -> App WT.WeakType
resolveType h t = do
  sub <- Unify.unifyCurrentConstraints h
  reduceWeakType' h sub t

reduceWeakType' :: Handle -> THS.TypeHoleSubst -> WT.WeakType -> App WT.WeakType
reduceWeakType' h holeSubst t = do
  t' <- reduceType h t
  case t' of
    m :< WT.TypeHole holeID args -> do
      case THS.lookup holeID holeSubst of
        Nothing ->
          return t'
        Just (xs, body)
          | length xs == length args -> do
              let sub = IntMap.fromList $ zip (map Ident.toInt xs) (map Type args)
              body' <- liftIO $ Subst.substType (substHandle h) sub body
              reduceWeakType' h holeSubst body'
          | otherwise ->
              raiseError m "Arity mismatch"
    _ :< WT.TyApp (_ :< WT.TVarGlobal _ name) args -> do
      mDef <- liftIO $ WeakTypeDef.lookup' (weakTypeDefHandle h) name
      case mDef of
        Just def
          | length args == length (WeakTypeDef.typeDefBinders def) -> do
              let varList = map (\(_, x, _) -> Ident.toInt x) (WeakTypeDef.typeDefBinders def)
              let sub = IntMap.fromList $ zip varList (map Type args)
              body' <- liftIO $ Subst.substType (substHandle h) sub (WeakTypeDef.typeDefBody def)
              reduceWeakType' h holeSubst body'
        _ ->
          return t'
    m :< WT.BoxNoema tInner -> do
      tInner' <- reduceWeakType' h holeSubst tInner
      return $ m :< WT.BoxNoema tInner'
    _ ->
      return t'

-- reduceWeakType' :: Handle -> THS.TypeHoleSubst -> WT.WeakType -> App WT.WeakType
-- reduceWeakType' h sub e = do
--   e' <- reduceType h e
--   if THS.fillableType e' sub
--     then fillType h sub e' >>= reduceWeakType' h sub
--     else return e'

ensureArityCorrectness :: Handle -> WT.WeakTerm -> Int -> Int -> App ()
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

ensureImplicitArityCorrectness :: Handle -> WT.WeakTerm -> Int -> Int -> App ()
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

ensureDefaultKeyLinearity :: Hint -> [T.Text] -> App ()
ensureDefaultKeyLinearity m ks = do
  let counts = Map.fromListWith (+) $ map (\k -> (k, 1 :: Int)) ks
  let nonLinear = Map.keys $ Map.filter (> 1) counts
  unless (null nonLinear) $ do
    raiseError m $
      "The following fields are defined more than once:\n"
        <> showKeyList nonLinear

checkDefaultKeyRedundancy :: Hint -> [T.Text] -> [T.Text] -> App ()
checkDefaultKeyRedundancy m allowedKeys ks = do
  let allowedSet = S.fromList allowedKeys
  let diff = filter (`S.notMember` allowedSet) ks
  unless (null diff) $ do
    raiseError m $ "The following field(s) are redundant:\n" <> showKeyList diff

showKeyList :: [T.Text] -> T.Text
showKeyList ks =
  T.intercalate "\n" $ map ("- " <>) ks

ensureTypeArityCorrectness :: Hint -> Int -> Int -> App ()
ensureTypeArityCorrectness m expected found = do
  when (expected /= found) $ do
    raiseError m $
      "This type expects "
        <> T.pack (show expected)
        <> " arguments, but found "
        <> T.pack (show found)
        <> "."

primOpToType :: Handle -> Hint -> PrimOp -> IO WT.WeakType
primOpToType h m op = do
  let (domList, cod) = getTypeInfo op
  xs <- mapM (const (Gensym.newIdentFromText (gensymHandle h) "_")) domList
  let domList' = map (\pt -> m :< WT.PrimType pt) domList
  let xts = zipWith (\x t -> (m, x, t)) xs domList'
  let cod' = m :< WT.PrimType cod
  return $ m :< WT.Pi PK.normal [] xts [] cod'

checkIsCodeType :: Handle -> Hint -> WT.WeakType -> App ()
checkIsCodeType h m t = do
  tInner <- liftIO $ newTypeHole h m (varEnv h)
  liftIO $ Constraint.insert (constraintHandle h) (m :< WT.Code tInner) t

substTypeBinder :: Handle -> Subst.Subst -> [BinderF WT.WeakType] -> App ([BinderF WT.WeakType], Subst.Subst)
substTypeBinder h sub mxts = do
  liftIO $ Subst.subst' (substHandle h) sub mxts
