module Kernel.Elaborate.Elaborate
  ( getWeakTypeEnv,
    elaborate,
    elaborate',
    elaborateType,
  )
where

import App.App (App)
import App.Error qualified as E
import App.Run (raiseCritical, raiseError)
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
import Gensym.Trick qualified as Gensym
import Kernel.Common.Cache qualified as Cache
import Kernel.Common.Const (holeLiteral)
import Kernel.Common.Handle.Global.GlobalRemark qualified as GlobalRemark
import Kernel.Common.Handle.Global.KeyArg qualified as KeyArg
import Kernel.Common.Handle.Global.Type qualified as Type
import Kernel.Common.ManageCache qualified as Cache
import Kernel.Common.Target hiding (Main)
import Kernel.Elaborate.Internal.EnsureAffinity qualified as EnsureAffinity
import Kernel.Elaborate.Internal.Handle.Constraint qualified as Constraint
import Kernel.Elaborate.Internal.Handle.Def qualified as Definition
import Kernel.Elaborate.Internal.Handle.Elaborate
import Kernel.Elaborate.Internal.Handle.Hole qualified as Hole
import Kernel.Elaborate.Internal.Handle.LocalLogs qualified as LocalLogs
import Kernel.Elaborate.Internal.Handle.TypeDef qualified as TypeDef
import Kernel.Elaborate.Internal.Handle.WeakDecl qualified as WeakDecl
import Kernel.Elaborate.Internal.Handle.WeakDef qualified as WeakDef
import Kernel.Elaborate.Internal.Handle.WeakType qualified as WeakType
import Kernel.Elaborate.Internal.Handle.WeakTypeDef qualified as WeakTypeDef
import Kernel.Elaborate.Internal.Infer qualified as Infer
import Kernel.Elaborate.Internal.Unify qualified as Unify
import Kernel.Elaborate.TypeHoleSubst qualified as THS
import Language.Common.Annotation qualified as AN
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.BaseLowType qualified as BLT
import Language.Common.BasePrimType qualified as BPT
import Language.Common.Binder
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.DecisionTree qualified as DT
import Language.Common.DefaultArgs qualified as DefaultArgs
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Foreign qualified as F
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.Geist qualified as G
import Language.Common.HoleID qualified as HID
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.ImpArgs qualified as ImpArgs
import Language.Common.IsConstLike (IsConstLike)
import Language.Common.LamKind qualified as LK
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.Common.PiKind qualified as PK
import Language.Common.PrimNumSize
import Language.Common.PrimType qualified as PT
import Language.Common.StmtKind qualified as SK
import Language.LowComp.DeclarationName qualified as DN
import Language.Term.Inline qualified as Inline
import Language.Term.PrimValue qualified as PV
import Language.Term.Stmt
import Language.Term.Term qualified as TM
import Language.Term.Weaken
import Language.WeakTerm.Subst (SubstEntry (..))
import Language.WeakTerm.Subst qualified as Subst
import Language.WeakTerm.ToText
import Language.WeakTerm.WeakPrimValue qualified as WPV
import Language.WeakTerm.WeakStmt
import Language.WeakTerm.WeakTerm qualified as WT
import Logger.Hint
import Logger.Log qualified as L

getWeakTypeEnv :: Handle -> IO WeakType.WeakTypeEnv
getWeakTypeEnv h =
  WeakType.get $ weakTypeHandle h

elaborate :: Handle -> Target -> [L.Log] -> Either Cache.Cache [WeakStmt] -> App [Stmt]
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

analyzeStmtList :: Handle -> [WeakStmt] -> App [WeakStmt]
analyzeStmtList h stmtList = do
  forM stmtList $ \stmt -> do
    stmt' <- Infer.inferStmt h stmt
    insertWeakStmt h stmt'
    return stmt'

synthesizeStmtList :: Handle -> Target -> [L.Log] -> [WeakStmt] -> App [Stmt]
synthesizeStmtList h t logs stmtList = do
  -- mapM_ (liftIO . viewStmt) stmtList
  liftIO (Constraint.get (constraintHandle h)) >>= Unify.unify h >>= liftIO . Hole.setTypeSubst (holeHandle h)
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

elaborateStmt :: Handle -> WeakStmt -> App ([Stmt], [L.Log])
elaborateStmt h stmt = do
  case stmt of
    WeakStmtDefineTerm isConstLike stmtKind m x impArgs expArgs defaultArgs codType e -> do
      stmtKind' <- elaborateStmtKindTerm h stmtKind
      e' <- elaborate' h e
      impArgs' <- mapM (elaborateWeakBinder h) impArgs
      defaultArgs' <- forM defaultArgs $ \(binder, value) -> do
        binder' <- elaborateWeakBinder h binder
        value' <- elaborate' h value
        return (binder', value')
      expArgs' <- mapM (elaborateWeakBinder h) expArgs
      codType' <- elaborateType h codType
      let dummyAttr = AttrL.Attr {lamKind = LK.Normal Nothing codType', identity = 0}
      remarks <- do
        affHandle <- liftIO $ EnsureAffinity.new h
        EnsureAffinity.ensureAffinity affHandle $ m :< TM.PiIntro dummyAttr impArgs' expArgs' defaultArgs' e'
      e'' <-
        if not $ SK.isMacroStmtKind stmtKind
          then inline h m e'
          else return e'
      impArgs'' <- mapM (inlineBinder h) impArgs'
      defaultArgs'' <- forM defaultArgs' $ \(binder, value) -> do
        binder' <- inlineBinder h binder
        value' <- inline h m value
        return (binder', value')
      expArgs'' <- mapM (inlineBinder h) expArgs'
      codType'' <- inlineType h m codType'
      when isConstLike $ do
        unless (TM.isValue e'') $ do
          raiseError m "Could not reduce the body of this definition into a constant"
      let result = StmtDefine isConstLike stmtKind' (SavedHint m) x impArgs'' expArgs'' defaultArgs'' codType'' e''
      insertStmt h result
      return ([result], remarks)
    WeakStmtDefineType isConstLike stmtKind m x impArgs expArgs defaultArgs codType body -> do
      stmtKind' <- elaborateStmtKindType h stmtKind
      impArgs' <- mapM (elaborateWeakBinder h) impArgs
      defaultArgs' <- forM defaultArgs $ \(binder, value) -> do
        binder' <- elaborateWeakBinder h binder
        value' <- elaborate' h value
        return (binder', value')
      expArgs' <- mapM (elaborateWeakBinder h) expArgs
      codType' <- elaborateType h codType
      body' <- elaborateType h body
      impArgs'' <- mapM (inlineBinder h) impArgs'
      defaultArgs'' <- forM defaultArgs' $ \(binder, value) -> do
        binder' <- inlineBinder h binder
        value' <- inline h m value
        return (binder', value')
      expArgs'' <- mapM (inlineBinder h) expArgs'
      codType'' <- inlineType h m codType'
      body'' <- inlineType h m body'
      let result = StmtDefineType isConstLike stmtKind' (SavedHint m) x impArgs'' expArgs'' defaultArgs'' codType'' body''
      insertStmt h result
      return ([result], [])
    WeakStmtDefineResource m dd resourceID unitType discarder copier -> do
      unitType' <- elaborateType h unitType
      discarder' <- elaborate' h discarder
      copier' <- elaborate' h copier
      let result = StmtDefineResource (SavedHint m) dd resourceID unitType' discarder' copier'
      insertStmt h result
      return ([result], [])
    WeakStmtVariadic kind m dd -> do
      return ([StmtVariadic kind (SavedHint m) dd], [])
    WeakStmtNominal _ geistList -> do
      mapM_ (elaborateGeist h . snd) geistList
      return ([], [])
    WeakStmtForeign foreignList -> do
      foreignList' <- forM foreignList $ \(F.Foreign m externalName domList cod) -> do
        domList' <- mapM (strictify h) domList
        cod' <- mapM (strictify h) cod
        return $ F.Foreign m externalName domList' cod'
      return ([StmtForeign foreignList'], [])

elaborateGeist :: Handle -> G.Geist WT.WeakType WT.WeakTerm -> App (G.Geist TM.Type TM.Term)
elaborateGeist h (G.Geist {..}) = do
  impArgs' <- mapM (elaborateWeakBinder h) impArgs
  defaultArgs' <- forM defaultArgs $ \(binder, value) -> do
    binder' <- elaborateWeakBinder h binder
    value' <- elaborate' h value
    return (binder', value')
  expArgs' <- mapM (elaborateWeakBinder h) expArgs
  cod' <- elaborateType h cod
  return $ G.Geist {impArgs = impArgs', defaultArgs = defaultArgs', expArgs = expArgs', cod = cod', ..}

insertStmt :: Handle -> Stmt -> App ()
insertStmt h stmt = do
  case stmt of
    StmtDefine isConstLike stmtKind (SavedHint m) f impArgs expArgs defaultArgs t e -> do
      case stmtKind of
        SK.DataIntro {} ->
          liftIO $ Type.insert' (typeHandle h) f $ weakenType $ m :< TM.Pi (PK.DataIntro isConstLike) impArgs expArgs (map fst defaultArgs) t
        _ ->
          liftIO $ Type.insert' (typeHandle h) f $ weakenType $ m :< TM.Pi (PK.Normal isConstLike) impArgs expArgs (map fst defaultArgs) t
      liftIO $ Definition.insert' (defHandle h) f impArgs expArgs defaultArgs e t (stmtKindToDefKind stmtKind)
    StmtDefineType isConstLike stmtKind (SavedHint m) f impArgs expArgs defaultArgs t body -> do
      let allBinders = impArgs ++ expArgs ++ map fst defaultArgs
      liftIO $ Type.insert' (typeHandle h) f $ weakenType $ m :< TM.Pi (PK.Normal isConstLike) impArgs expArgs (map fst defaultArgs) t
      liftIO $ TypeDef.insert' (typeDefHandle h) (SK.toOpacityType stmtKind) f allBinders body
    StmtDefineResource (SavedHint m) dd resourceID _ _ _ -> do
      liftIO $ Type.insert' (typeHandle h) dd $ weakenType (m :< TM.Pi (PK.Normal True) [] [] [] (m :< TM.Tau))
      liftIO $ TypeDef.insert' (typeDefHandle h) (SK.toOpacityType SK.Alias) dd [] (m :< TM.Resource dd resourceID)
    StmtVariadic {} ->
      return ()
    StmtForeign _ -> do
      return ()
  insertWeakStmt h $ weakenStmt stmt

insertWeakStmt :: Handle -> WeakStmt -> App ()
insertWeakStmt h stmt = do
  case stmt of
    WeakStmtDefineTerm _ stmtKind m f impArgs expArgs defaultArgs codType e -> do
      liftIO $ WeakDef.insert' (weakDefHandle h) (SK.toOpacityTerm stmtKind) m f impArgs expArgs defaultArgs codType e
    WeakStmtDefineType _ stmtKind _ f impArgs expArgs defaultArgs _ body -> do
      let binders = impArgs ++ expArgs ++ map fst defaultArgs
      liftIO $ WeakTypeDef.insert' (weakTypeDefHandle h) (SK.toOpacityType stmtKind) f binders body
    WeakStmtDefineResource m dd resourceID _ _ _ -> do
      let resourceType = m :< WT.Resource dd resourceID
      liftIO $ WeakTypeDef.insert' (weakTypeDefHandle h) (SK.toOpacityType SK.Alias) dd [] resourceType
    WeakStmtNominal {} -> do
      return ()
    WeakStmtVariadic {} -> do
      return ()
    WeakStmtForeign foreignList ->
      forM_ foreignList $ \(F.Foreign _ externalName domList cod) -> do
        liftIO $ WeakDecl.insert (weakDeclHandle h) (DN.Ext externalName) domList cod

elaborateStmtKindTerm :: Handle -> SK.StmtKindTerm WT.WeakType -> App (SK.StmtKindTerm TM.Type)
elaborateStmtKindTerm h stmtKind =
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
      t' <- elaborateType h t
      return $ SK.Main t'
    SK.DataIntro dataName dataArgs expConsArgs discriminant -> do
      dataArgs' <- mapM (elaborateWeakBinder h) dataArgs
      expConsArgs' <- mapM (elaborateWeakBinder h) expConsArgs
      return $ SK.DataIntro dataName dataArgs' expConsArgs' discriminant

elaborateStmtKindType :: Handle -> SK.StmtKindType WT.WeakType -> App (SK.StmtKindType TM.Type)
elaborateStmtKindType h stmtKind =
  case stmtKind of
    SK.Alias ->
      return SK.Alias
    SK.AliasOpaque ->
      return SK.AliasOpaque
    SK.Data dataName dataArgs consInfoList -> do
      dataArgs' <- mapM (elaborateWeakBinder h) dataArgs
      let (ms, consNameList, constLikeList, consArgsList, discriminantList) = unzip5 consInfoList
      consArgsList' <- mapM (mapM $ elaborateWeakBinder h) consArgsList
      let consInfoList' = zip5 ms consNameList constLikeList consArgsList' discriminantList
      return $ SK.Data dataName dataArgs' consInfoList'

elaborate' :: Handle -> WT.WeakTerm -> App TM.Term
elaborate' h term = do
  case term of
    m :< WT.Var x ->
      return $ m :< TM.Var x
    m :< WT.VarGlobal name argNum ->
      return $ m :< TM.VarGlobal name argNum
    m :< WT.PiIntro kind impArgs expArgs defaultArgs e -> do
      kind' <- elaborateLamAttr h kind
      impArgs' <- mapM (elaborateWeakBinder h) impArgs
      defaultArgs' <- forM defaultArgs $ \(binder, value) -> do
        binder' <- elaborateWeakBinder h binder
        value' <- elaborate' h value
        return (binder', value')
      expArgs' <- mapM (elaborateWeakBinder h) expArgs
      e' <- elaborate' h e
      return $ m :< TM.PiIntro kind' impArgs' expArgs' defaultArgs' e'
    m :< WT.PiElim b e impArgs expArgs defaultArgs -> do
      e' <- elaborate' h e
      let impArgs' = ImpArgs.extract impArgs
      impArgs'' <- mapM (elaborateType h) impArgs'
      expArgs' <- mapM (elaborate' h) expArgs
      defaultArgs' <- case defaultArgs of
        DefaultArgs.Aligned args ->
          mapM (traverse (elaborate' h)) args
        DefaultArgs.ByKey _ ->
          raiseCritical m "Scene.Elaborate.elaborate': found a remaining `ByKey` default argument"
      return $ m :< TM.PiElim b e' impArgs'' expArgs' defaultArgs'
    m :< WT.PiElimExact {} -> do
      raiseCritical m "Scene.Elaborate.elaborate': found a remaining `exact`"
    m :< WT.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (elaborateType h) dataArgs
      consArgs' <- mapM (elaborate' h) consArgs
      attr' <- elaborateAttrDataIntro h attr
      return $ m :< TM.DataIntro attr' consName dataArgs' consArgs'
    m :< WT.DataElim isNoetic oets tree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (elaborate' h) es
      ts' <- mapM (elaborateType h) ts
      rootCursor <- liftIO $ Gensym.newIdentForHole (gensymHandle h)
      let rootElem = (rootCursor, (Nothing, True, os))
      tree' <- elaborateDecisionTree h [rootElem] m m tree
      when (DT.isUnreachable tree') $ do
        forM_ ts' $ \t -> do
          t' <- reduceWeakType h (weakenType t) >>= elaborateType h
          switchSpec <- getSwitchSpec m t'
          case switchSpec of
            LiteralSwitch -> do
              raiseEmptyNonExhaustivePatternMatching m
            ConsSwitch consList -> do
              unless (null consList) $
                raiseEmptyNonExhaustivePatternMatching m
      return $ m :< TM.DataElim isNoetic (zip3 os es' ts') tree'
    m :< WT.BoxIntro letSeq e -> do
      letSeq' <- mapM (elaborateLet h) letSeq
      e' <- elaborate' h e
      return $ m :< TM.BoxIntro letSeq' e'
    _ :< WT.BoxIntroLift e -> do
      elaborate' h e
    m :< WT.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      castSeq' <- mapM (elaborateLet h) castSeq
      mxt' <- elaborateWeakBinder h mxt
      e1' <- elaborate' h e1
      uncastSeq' <- mapM (elaborateLet h) uncastSeq
      e2' <- elaborate' h e2
      return $ m :< TM.BoxElim castSeq' mxt' e1' uncastSeq' e2'
    m :< WT.CodeIntro e -> do
      e' <- elaborate' h e
      return $ m :< TM.CodeIntro e'
    m :< WT.CodeElim e -> do
      e' <- elaborate' h e
      return $ m :< TM.CodeElim e'
    m :< WT.TauIntro ty -> do
      ty' <- elaborateType h ty
      return $ m :< TM.TauIntro ty'
    m :< WT.TauElim (mx, x) e1 e2 -> do
      e1' <- elaborate' h e1
      e2' <- elaborate' h e2
      return $ m :< TM.TauElim (mx, x) e1' e2'
    _ :< WT.Actual e -> do
      elaborate' h e
    m :< WT.Let opacity (mx, x, t) e1 e2 -> do
      e1' <- elaborate' h e1
      t' <- reduceWeakType h t >>= elaborateType h
      e2' <- elaborate' h e2
      return $ m :< TM.Let (WT.reifyOpacity opacity) (mx, x, t') e1' e2'
    m :< WT.Prim primValue -> do
      primValue' <- elaboratePrimValue h m primValue
      return $ m :< TM.Prim primValue'
    m :< WT.Magic (M.WeakMagic magic) -> do
      case magic of
        M.LowMagic lowMagic ->
          case lowMagic of
            LM.External domList cod name args varArgs -> do
              domList' <- mapM (strictify h) domList
              cod' <- case cod of
                FCT.Cod t -> do
                  t' <- strictify h t
                  return $ FCT.Cod t'
                FCT.Void ->
                  return FCT.Void
              args' <- mapM (elaborate' h) args
              let (vArgs, vTypes) = unzip varArgs
              vArgs' <- mapM (elaborate' h) vArgs
              vTypes' <- mapM (strictify h) vTypes
              return $ m :< TM.Magic (M.LowMagic $ LM.External domList' cod' name args' (zip vArgs' vTypes'))
            LM.Cast from to value -> do
              from' <- elaborateType h from
              to' <- elaborateType h to
              value' <- elaborate' h value
              return $ m :< TM.Magic (M.LowMagic $ LM.Cast from' to' value')
            LM.Store t unit value pointer -> do
              t' <- strictify h t
              unit' <- elaborateType h unit
              value' <- elaborate' h value
              pointer' <- elaborate' h pointer
              return $ m :< TM.Magic (M.LowMagic $ LM.Store t' unit' value' pointer')
            LM.Load t pointer -> do
              t' <- strictify h t
              pointer' <- elaborate' h pointer
              return $ m :< TM.Magic (M.LowMagic $ LM.Load t' pointer')
            LM.Alloca t size -> do
              t' <- strictify h t
              size' <- elaborate' h size
              return $ m :< TM.Magic (M.LowMagic $ LM.Alloca t' size')
            LM.Global name t -> do
              t' <- strictify h t
              return $ m :< TM.Magic (M.LowMagic $ LM.Global name t')
            LM.OpaqueValue e -> do
              e' <- elaborate' h e
              return $ m :< TM.Magic (M.LowMagic $ LM.OpaqueValue e')
            LM.CallType func arg1 arg2 -> do
              func' <- elaborate' h func
              arg1' <- elaborate' h arg1
              arg2' <- elaborate' h arg2
              return $ m :< TM.Magic (M.LowMagic $ LM.CallType func' arg1' arg2')
        M.GetTypeTag mid typeTagExpr typeExpr -> do
          typeTagExpr' <- elaborateType h typeTagExpr
          typeExpr' <- elaborateType h typeExpr
          return $ m :< TM.Magic (M.GetTypeTag mid typeTagExpr' typeExpr')
        M.GetDataArgs sgl listExpr typeExpr -> do
          listExpr' <- elaborateType h listExpr
          typeExpr' <- elaborateType h typeExpr
          return $ m :< TM.Magic (M.GetDataArgs sgl listExpr' typeExpr')
        M.GetConsSize typeExpr -> do
          typeExpr' <- elaborateType h typeExpr
          return $ m :< TM.Magic (M.GetConsSize typeExpr')
        M.GetWrapperContentType typeExpr -> do
          typeExpr' <- elaborateType h typeExpr
          return $ m :< TM.Magic (M.GetWrapperContentType typeExpr')
        M.GetVectorContentType sgl typeExpr -> do
          typeExpr' <- elaborateType h typeExpr
          return $ m :< TM.Magic (M.GetVectorContentType sgl typeExpr')
        M.GetNoemaContentType typeExpr -> do
          typeExpr' <- elaborateType h typeExpr
          return $ m :< TM.Magic (M.GetNoemaContentType typeExpr')
        M.GetBoxContentType typeExpr -> do
          typeExpr' <- elaborateType h typeExpr
          return $ m :< TM.Magic (M.GetBoxContentType typeExpr')
        M.GetConstructorArgTypes sgl listExpr typeExpr index -> do
          listExpr' <- elaborateType h listExpr
          typeExpr' <- elaborateType h typeExpr
          index' <- elaborate' h index
          return $ m :< TM.Magic (M.GetConstructorArgTypes sgl listExpr' typeExpr' index')
        M.GetConsName textType typeExpr index -> do
          textType' <- elaborateType h textType
          typeExpr' <- elaborateType h typeExpr
          index' <- elaborate' h index
          return $ m :< TM.Magic (M.GetConsName textType' typeExpr' index')
        M.GetConsConstFlag boolType typeExpr index -> do
          boolType' <- elaborateType h boolType
          typeExpr' <- elaborateType h typeExpr
          index' <- elaborate' h index
          return $ m :< TM.Magic (M.GetConsConstFlag boolType' typeExpr' index')
        M.ShowType textTypeExpr typeExpr -> do
          textTypeExpr' <- elaborateType h textTypeExpr
          typeExpr' <- elaborateType h typeExpr
          return $ m :< TM.Magic (M.ShowType textTypeExpr' typeExpr')
        M.TextCons textTypeExpr rune text -> do
          textTypeExpr' <- elaborateType h textTypeExpr
          rune' <- elaborate' h rune
          text' <- elaborate' h text
          return $ m :< TM.Magic (M.TextCons textTypeExpr' rune' text')
        M.TextUncons mid text -> do
          text' <- elaborate' h text
          return $ m :< TM.Magic (M.TextUncons mid text')
        M.CompileError typeExpr msg -> do
          typeExpr' <- elaborateType h typeExpr
          msg' <- elaborate' h msg
          return $ m :< TM.Magic (M.CompileError typeExpr' msg')
    m :< WT.Annotation remarkLevel annot e -> do
      e' <- elaborate' h e
      case annot of
        AN.Type t -> do
          t' <- elaborateType h t
          let message = "Admitted: `" <> toTextType (weakenType t') <> "`"
          let typeRemark = L.newLog m remarkLevel message
          liftIO $ LocalLogs.insert (localLogsHandle h) typeRemark
          return e'

elaborateType :: Handle -> WT.WeakType -> App TM.Type
elaborateType h ty =
  case ty of
    m :< WT.Tau ->
      return $ m :< TM.Tau
    m :< WT.TVar x ->
      return $ m :< TM.TVar x
    m :< WT.TVarGlobal attr name ->
      return $ m :< TM.TVarGlobal attr name
    m :< WT.TyApp t args -> do
      t' <- elaborateType h t
      args' <- mapM (elaborateType h) args
      return $ m :< TM.TyApp t' args'
    m :< WT.Pi piKind impArgs expArgs defaultArgs t -> do
      impArgs' <- mapM (elaborateWeakBinder h) impArgs
      expArgs' <- mapM (elaborateWeakBinder h) expArgs
      defaultArgs' <- mapM (elaborateWeakBinder h) defaultArgs
      t' <- elaborateType h t
      return $ m :< TM.Pi piKind impArgs' expArgs' defaultArgs' t'
    m :< WT.Data attr name es -> do
      es' <- mapM (elaborateType h) es
      attr' <- elaborateAttrData h attr
      return $ m :< TM.Data attr' name es'
    m :< WT.Box t -> do
      t' <- elaborateType h t
      return $ m :< TM.Box t'
    m :< WT.BoxNoema t -> do
      t' <- elaborateType h t
      return $ m :< TM.BoxNoema t'
    m :< WT.Code t -> do
      t' <- elaborateType h t
      return $ m :< TM.Code t'
    m :< WT.PrimType pt ->
      return $ m :< TM.PrimType pt
    m :< WT.Void ->
      return $ m :< TM.Void
    m :< WT.Resource dd resourceID -> do
      return $ m :< TM.Resource dd resourceID
    m :< WT.TypeHole hole es -> do
      fillHole h m hole es >>= elaborateType h

elaboratePrimValue :: Handle -> Hint -> WPV.WeakPrimValue WT.WeakType -> App (PV.PrimValue TM.Type)
elaboratePrimValue h m primValue =
  case primValue of
    WPV.Int t x -> do
      (size, t') <- strictifyDecimalType h m x t
      case size of
        Right intSize ->
          return $ PV.Int t' intSize x
        Left floatSize ->
          return $ PV.Float t' floatSize (fromInteger x)
    WPV.Float t x -> do
      (size, t') <- strictifyFloatType h m x t
      return $ PV.Float t' size x
    WPV.Op op ->
      return $ PV.Op op
    WPV.StaticText t text -> do
      t' <- elaborateType h t
      return $ PV.StaticText t' text
    WPV.Rune r ->
      return $ PV.Rune r

strictify :: Handle -> WT.WeakType -> App BLT.BaseLowType
strictify h t@(mt :< _) =
  strictify' h mt t

strictify' :: Handle -> Hint -> WT.WeakType -> App BLT.BaseLowType
strictify' h m t = do
  t' <- reduceWeakType h t >>= elaborateType h
  case t' of
    _ :< TM.PrimType (PT.Int size) ->
      return $ BLT.PrimNum $ BPT.Int $ BPT.Explicit size
    _ :< TM.PrimType (PT.Float size) ->
      return $ BLT.PrimNum $ BPT.Float $ BPT.Explicit size
    _ :< TM.PrimType PT.Pointer ->
      return BLT.Pointer
    _ :< TM.Data (AttrD.Attr {consNameList = [(consName, _, _)]}) _ [] -> do
      consType <- Type.lookup' (typeHandle h) m consName
      case consType of
        _ :< WT.Pi (PK.DataIntro False) _ [] [] (_ :< WT.Pi _ impArgs expArgs defaultArgs _)
          | [(_, _, arg)] <- impArgs ++ expArgs ++ defaultArgs -> do
              strictify' h m arg
        _ ->
          raiseNonStrictType m consType
    _ :< _ ->
      raiseNonStrictType m (weakenType t')

strictifyDecimalType :: Handle -> Hint -> Integer -> WT.WeakType -> App (Either FloatSize IntSize, TM.Type)
strictifyDecimalType h m x t = do
  t' <- reduceWeakType h t >>= elaborateType h
  case t' of
    _ :< TM.PrimType (PT.Int size) ->
      return (Right size, t')
    _ :< TM.PrimType (PT.Float size) ->
      return (Left size, t')
    _ :< TM.Data (AttrD.Attr {consNameList = [(consName, _, _)]}) _ [] -> do
      consType <- Type.lookup' (typeHandle h) m consName
      case consType of
        _ :< WT.Pi (PK.DataIntro False) _ [] [] (_ :< WT.Pi _ impArgs expArgs defaultArgs _)
          | [(_, _, arg)] <- impArgs ++ expArgs ++ defaultArgs -> do
              strictifyDecimalType h m x arg
        _ ->
          raiseNonDecimalType m x (weakenType t')
    _ :< _ ->
      raiseNonDecimalType m x (weakenType t')

strictifyFloatType :: Handle -> Hint -> Double -> WT.WeakType -> App (FloatSize, TM.Type)
strictifyFloatType h m x t = do
  t' <- reduceWeakType h t >>= elaborateType h
  case t' of
    _ :< TM.PrimType (PT.Float size) ->
      return (size, t')
    _ :< TM.Data (AttrD.Attr {consNameList = [(consName, _, _)]}) _ [] -> do
      consType <- Type.lookup' (typeHandle h) m consName
      case consType of
        _ :< WT.Pi (PK.DataIntro False) _ [] [] (_ :< WT.Pi _ impArgs expArgs defaultArgs _)
          | [(_, _, arg)] <- impArgs ++ expArgs ++ defaultArgs -> do
              strictifyFloatType h m x arg
        _ ->
          raiseNonFloatType m x (weakenType t')
    _ :< _ ->
      raiseNonFloatType m x (weakenType t')

elaborateWeakBinder :: Handle -> BinderF WT.WeakType -> App (BinderF TM.Type)
elaborateWeakBinder h (m, x, t) = do
  t' <- elaborateType h t
  return (m, x, t')

inlineType :: Handle -> Hint -> TM.Type -> App TM.Type
inlineType h m t = do
  dmap <- liftIO $ Definition.get' (defHandle h)
  typeDefMap <- liftIO $ TypeDef.get' (typeDefHandle h)
  inlineHandle <- liftIO $ Inline.new (gensymHandle h) dmap typeDefMap m (inlineLimit h)
  Inline.inlineType inlineHandle t

elaborateLet :: Handle -> (BinderF WT.WeakType, WT.WeakTerm) -> App (BinderF TM.Type, TM.Term)
elaborateLet h (xt, e) = do
  xt' <- elaborateWeakBinder h xt
  e' <- elaborate' h e
  return (xt', e')

elaborateLamAttr :: Handle -> AttrL.Attr WT.WeakType -> App (AttrL.Attr TM.Type)
elaborateLamAttr h (AttrL.Attr {lamKind, identity}) =
  case lamKind of
    LK.Normal name codType -> do
      codType' <- elaborateType h codType
      return $ AttrL.Attr {lamKind = LK.Normal name codType', identity}
    LK.Fix opacity xt -> do
      xt' <- elaborateWeakBinder h xt
      return $ AttrL.Attr {lamKind = LK.Fix opacity xt', identity}

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
  DT.DecisionTree WT.WeakType WT.WeakTerm ->
  App (DT.DecisionTree TM.Type TM.Term)
elaborateDecisionTree h ctx mOrig m tree =
  case tree of
    DT.Leaf xs letSeq body -> do
      letSeq' <- mapM (bimapM (elaborateWeakBinder h) (elaborate' h)) letSeq
      body' <- elaborate' h body
      return $ DT.Leaf xs letSeq' body'
    DT.Unreachable ->
      return DT.Unreachable
    DT.Switch (cursor, cursorType) (fallbackClause, clauseList) -> do
      cursorType' <- reduceWeakType h cursorType >>= elaborateType h
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
                    (_, keys, _) <- KeyArg.lookup (keyArgHandle h) m consDD
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

elaborateClause :: Handle -> Hint -> Ident -> ClauseContext -> DT.Case WT.WeakType WT.WeakTerm -> App (DT.Case TM.Type TM.Term)
elaborateClause h mOrig cursor ctx decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      cont' <- elaborateDecisionTree h ctx mOrig mPat cont
      return $ DT.LiteralCase mPat i cont'
    DT.ConsCase record@DT.ConsCaseRecord {..} -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      dataTerms' <- mapM (elaborateType h) dataTerms
      dataTypes' <- mapM (elaborateType h) dataTypes
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

raiseNonStrictType :: Hint -> WT.WeakType -> App a
raiseNonStrictType m t = do
  raiseError m $
    "Expected:\n  an integer, a float, or a pointer\nFound:\n  "
      <> toTextType t

raiseNonDecimalType :: Hint -> Integer -> WT.WeakType -> App a
raiseNonDecimalType m x t = do
  raiseError m $
    "The term `"
      <> T.pack (show x)
      <> "` is an integer, but its type is: "
      <> toTextType t

raiseNonFloatType :: Hint -> Double -> WT.WeakType -> App a
raiseNonFloatType m x t = do
  raiseError m $
    "The term `"
      <> T.pack (show x)
      <> "` is a float, but its type is: "
      <> toTextType t

raiseLiteralNonExhaustivePatternMatching :: Hint -> App a
raiseLiteralNonExhaustivePatternMatching m =
  raiseError m "Pattern matching on literals must have a fallback clause"

raiseEmptyNonExhaustivePatternMatching :: Hint -> App a
raiseEmptyNonExhaustivePatternMatching m =
  raiseError m "Empty pattern matching can only be performed on empty ADT values"

data SwitchSpec
  = LiteralSwitch
  | ConsSwitch [(DD.DefiniteDescription, IsConstLike)]

getSwitchSpec :: Hint -> TM.Type -> App SwitchSpec
getSwitchSpec m cursorType = do
  case cursorType of
    _ :< TM.Data (AttrD.Attr {..}) _ _ -> do
      return $ ConsSwitch $ map (\(name, _, cl) -> (name, cl)) consNameList
    _ :< TM.PrimType (PT.Int _) -> do
      return LiteralSwitch
    _ :< TM.PrimType PT.Rune -> do
      return LiteralSwitch
    _ ->
      raiseError m $
        "This term is expected to be an ADT value or a literal, but found:\n"
          <> toTextType (weakenType cursorType)

reduceWeakType :: Handle -> WT.WeakType -> App WT.WeakType
reduceWeakType h t = do
  t' <- reduceType h t
  case t' of
    m :< WT.TypeHole holeID args -> do
      fillHole h m holeID args >>= reduceWeakType h
    _ :< WT.TyApp (_ :< WT.TVarGlobal _ name) args -> do
      mDef <- liftIO $ WeakTypeDef.lookup' (weakTypeDefHandle h) name
      case mDef of
        Just def
          | length args == length (WeakTypeDef.typeDefBinders def) -> do
              let varList = map (\(_, x, _) -> Ident.toInt x) (WeakTypeDef.typeDefBinders def)
              let sub = IntMap.fromList $ zip varList (map Type args)
              body' <- liftIO $ Subst.substType (substHandle h) sub (WeakTypeDef.typeDefBody def)
              reduceWeakType h body'
        _ ->
          return t'
    _ ->
      return t'

fillHole ::
  Handle ->
  Hint ->
  HID.HoleID ->
  [WT.WeakType] ->
  App WT.WeakType
fillHole h m holeID es = do
  holeSubst <- liftIO $ Hole.getTypeSubst (holeHandle h)
  case THS.lookup holeID holeSubst of
    Nothing ->
      raiseError m $ "Could not instantiate the hole here: " <> T.pack (show holeID)
    Just (xs, e)
      | length xs == length es -> do
          let s = IntMap.fromList $ zip (map Ident.toInt xs) (map Type es)
          liftIO $ Subst.substType (substHandle h) s e
      | otherwise ->
          raiseError m "Arity mismatch"

elaborateAttrData ::
  Handle ->
  AttrD.Attr DD.DefiniteDescription (BinderF WT.WeakType) ->
  App (AttrD.Attr DD.DefiniteDescription (BinderF TM.Type))
elaborateAttrData h attr = do
  let consNameList = AttrD.consNameList attr
  consNameList' <- forM consNameList $ \(name, binders, isConstLike) -> do
    binders' <- forM binders $ \(mx, x, t) -> do
      t' <- elaborateType h t
      return (mx, x, t')
    return (name, binders', isConstLike)
  return $ attr {AttrD.consNameList = consNameList'}

elaborateAttrDataIntro ::
  Handle ->
  AttrDI.Attr DD.DefiniteDescription (BinderF WT.WeakType) ->
  App (AttrDI.Attr DD.DefiniteDescription (BinderF TM.Type))
elaborateAttrDataIntro h attr = do
  let consNameList = AttrDI.consNameList attr
  consNameList' <- forM consNameList $ \(name, binders, isConstLike) -> do
    binders' <- forM binders $ \(mx, x, t) -> do
      t' <- elaborateType h t
      return (mx, x, t')
    return (name, binders', isConstLike)
  return $ attr {AttrDI.consNameList = consNameList'}

stmtKindToDefKind :: SK.StmtKindTerm a -> Maybe Inline.DefKind
stmtKindToDefKind stmtKind =
  case stmtKind of
    SK.Inline ->
      Just Inline.Inline
    SK.Macro ->
      Just Inline.Macro
    SK.MacroInline ->
      Just Inline.MacroInline
    SK.DataIntro {} ->
      Just Inline.DataIntro
    _ ->
      Nothing

-- viewStmt :: WeakStmt -> IO ()
-- viewStmt stmt = do
--   case stmt of
--     WeakStmtDefineTerm _ _ m x impArgs expArgs defArgs codType e -> do
--       let attr = AttrL.Attr {lamKind = LK.Normal Nothing codType, identity = 0}
--       putStrLn $ T.unpack $ DD.reify x <> "\n" <> toTextType (m :< WT.Pi (PK.Normal False) impArgs defArgs expArgs codType) <> "\n" <> toText (m :< WT.PiIntro attr impArgs defArgs expArgs e)
--     _ ->
--       return ()
