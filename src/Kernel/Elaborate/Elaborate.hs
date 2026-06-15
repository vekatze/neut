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
import Data.ByteString qualified as BS
import Data.IORef
import Data.IntMap qualified as IntMap
import Data.Set qualified as S
import Data.Text qualified as T
import Gensym.Trick qualified as Gensym
import Kernel.Common.Cache qualified as Cache
import Kernel.Common.Const (holeLiteral)
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Data qualified as Data
import Kernel.Common.Handle.Global.GlobalRemark qualified as GlobalRemark
import Kernel.Common.Handle.Global.KeyArg qualified as KeyArg
import Kernel.Common.Handle.Global.OptimizableData qualified as OptimizableData
import Kernel.Common.Handle.Global.Resource qualified as Resource
import Kernel.Common.Handle.Global.Type qualified as Type
import Kernel.Common.ManageCache qualified as Cache
import Kernel.Common.OptimizableData qualified as OD
import Kernel.Common.ReadableDD qualified as ReadableDD
import Kernel.Common.Source qualified as Source
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
import Kernel.Parse.Internal.Handle.UnusedTopLevelName qualified as UnusedTopLevelName
import Kernel.Parse.Internal.Handle.UsedTopLevelName qualified as UsedTopLevelName
import Language.Common.Annotation qualified as AN
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.BaseLowType qualified as BLT
import Language.Common.BaseName qualified as BN
import Language.Common.BasePrimType qualified as BPT
import Language.Common.Binder
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.DataInfo qualified as DI
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
import Language.Common.ModuleID qualified as MID
import Language.Common.PiElimKind qualified as PEK
import Language.Common.PiKind qualified as PK
import Language.Common.PrimNumSize
import Language.Common.PrimType qualified as PT
import Language.Common.SourceLocator qualified as SL
import Language.Common.StmtKind qualified as SK
import Language.Common.StrictGlobalLocator qualified as SGL
import Language.Common.Text.Util (decodeUtf8Bytes)
import Language.LowComp.DeclarationName qualified as DN
import Language.Term.Inline qualified as Inline
import Language.Term.PrimValue qualified as PV
import Language.Term.Stmt
import Language.Term.Subst qualified as TmSubst
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
import Logger.LogLevel qualified as LL

getWeakTypeEnv :: Handle -> IO WeakType.WeakTypeEnv
getWeakTypeEnv h =
  WeakType.get $ weakTypeHandle h

elaborate :: Handle -> Target -> [L.Log] -> Either Cache.Cache [WeakStmt] -> App [Stmt]
elaborate h t logs cacheOrStmt = do
  case cacheOrStmt of
    Left cache -> do
      let stmtList = Cache.stmtList cache
      liftIO $
        UnusedTopLevelName.deleteMany (Global.unusedTopLevelNameHandle $ globalHandle h) $
          Cache.globalReferenceList cache
      forM_ stmtList $ insertStmt h
      liftIO $ GlobalRemark.insert (globalRemarkHandle h) logs
      liftIO $ Gensym.setCount (gensymHandle h) $ Cache.countSnapshot cache
      return stmtList
    Right stmtList -> do
      globalReferenceList <- liftIO $ UsedTopLevelName.get (usedTopLevelNameHandle h)
      analyzeStmtList h stmtList >>= synthesizeStmtList h t logs globalReferenceList

analyzeStmtList :: Handle -> [WeakStmt] -> App [WeakStmt]
analyzeStmtList h stmtList = do
  forM stmtList $ \stmt -> do
    stmt' <- Infer.inferStmt h stmt
    insertWeakStmt h stmt'
    return stmt'

synthesizeStmtList :: Handle -> Target -> [L.Log] -> [DD.DefiniteDescription] -> [WeakStmt] -> App [Stmt]
synthesizeStmtList h t logs globalReferenceList stmtList = do
  -- mapM_ (liftIO . viewStmt) stmtList
  liftIO (Constraint.get (constraintHandle h)) >>= Unify.unify h >>= liftIO . Hole.setTypeSubst (holeHandle h)
  (stmtList', affineErrorList) <- bimap concat concat . unzip <$> mapM (elaborateStmt h) stmtList
  unless (null affineErrorList) $ do
    throwError $ E.MakeError affineErrorList
  generatedStmtList <- liftIO $ reverse <$> readIORef (pendingSpecializationDefs h)
  let stmtList'' = stmtList' ++ generatedStmtList
  residualCheckList' <- liftIO $ reverse <$> readIORef (residualCheckList h)
  residualErrorList <- concat <$> mapM (checkResidualCheck h) residualCheckList'
  unless (null residualErrorList) $ do
    throwError $ E.MakeError residualErrorList
  -- mapM_ (liftIO . viewStmt . weakenStmt) stmtList'
  countSnapshot <- liftIO $ Gensym.getCount (gensymHandle h)
  localLogs <- liftIO $ LocalLogs.get (localLogsHandle h)
  let logs' = logs ++ localLogs
  Cache.saveCache (pathHandle h) t (currentSource h) $
    Cache.Cache
      { Cache.stmtList = stmtList'',
        Cache.remarkList = logs',
        Cache.globalReferenceList = globalReferenceList,
        Cache.countSnapshot = countSnapshot
      }
  liftIO $ GlobalRemark.insert (globalRemarkHandle h) logs'
  return stmtList''

elaborateStmt :: Handle -> WeakStmt -> App ([Stmt], [L.Log])
elaborateStmt h stmt = do
  case stmt of
    WeakStmtDefineTerm isConstLike stmtKind m x impArgs expArgs defaultArgs codType e -> do
      stmtKind' <- elaborateStmtKindTerm h stmtKind
      impArgs' <- mapM (elaborateWeakBinder h) impArgs
      expArgs' <- mapM (elaborateWeakBinder h) expArgs
      defaultArgs' <- forM defaultArgs $ \(binder, value) -> do
        binder' <- elaborateWeakBinder h binder
        value' <- elaborate' h value
        return (binder', value')
      codType' <- elaborateType h codType
      let mDefKind = stmtKindToDefKind stmtKind defaultArgs'
      e' <- elaborate' h e
      case mDefKind of
        Just Inline.NoInline -> do
          defaultArgsForSelf <- forM defaultArgs' $ \(binder, value) -> do
            value' <- inline h m value
            return (binder, value')
          liftIO $ Definition.insert' (defHandle h) x impArgs' expArgs' defaultArgsForSelf e' codType' mDefKind
        _ ->
          return ()
      remarks <- do
        affHandle <- liftIO $ EnsureAffinity.new h
        let dummyAttr = AttrL.Attr {lamKind = LK.Normal Nothing False codType', identity = 0}
        EnsureAffinity.ensureAffinity affHandle $ m :< TM.PiIntro dummyAttr impArgs' expArgs' defaultArgs' e'
      e'' <-
        if not $ SK.isMacroStmtKind stmtKind
          then inline h m e'
          else return e'
      impArgs'' <- mapM (inlineBinder h) impArgs'
      defaultArgs'' <- forM defaultArgs' $ \(binder, value) -> do
        binder' <- inlineBinder h binder
        value' <-
          if SK.isMacroStmtKind stmtKind
            then inlineWithoutResidualChecks h m value
            else inline h m value
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
    WeakStmtDefineResource m dd resourceID unitType discarder copier resourceSize -> do
      discarder' <- elaborate' h discarder
      copier' <- elaborate' h copier
      resourceSize' <- elaborate' h resourceSize
      unitType' <- elaborateType h unitType
      discarder'' <- inline h m discarder'
      copier'' <- inline h m copier'
      resourceSize'' <- inline h m resourceSize'
      case Resource.layoutOf resourceSize'' of
        Just _ ->
          return ()
        Nothing ->
          raiseError m "Could not reduce the size of this resource into an integer"
      unitType'' <- inlineType h m unitType'
      let result = StmtDefineResource (SavedHint m) dd resourceID unitType'' discarder'' copier'' resourceSize''
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

checkResidualCheck :: Handle -> Inline.ResidualCheck -> App [L.Log]
checkResidualCheck h check =
  case check of
    Inline.CheckActuality m t ->
      checkActualityType h m S.empty t
    Inline.CheckInteger m t ->
      checkIntegerCursorType h m t

checkActualityType :: Handle -> Hint -> S.Set DD.DefiniteDescription -> TM.Type -> App [L.Log]
checkActualityType h m dataNameSet t = do
  t' <- inlineType h m t
  case t' of
    _ :< TM.Tau ->
      return []
    _ :< TM.Data _ dataName dataArgs -> do
      let dataNameSet' = S.insert dataName dataNameSet
      logsFromArgs <- concat <$> mapM (checkActualityType h m dataNameSet') dataArgs
      if S.member dataName dataNameSet
        then return logsFromArgs
        else do
          dataConsArgsList <- getActualityConsArgTypes h m dataName dataArgs
          logsFromCons <- fmap concat $ forM dataConsArgsList $ \dataConsArgs -> do
            fmap concat $ forM dataConsArgs $ \(_, _, _, consArg) -> do
              checkActualityType h m dataNameSet' consArg
          return $ logsFromArgs ++ logsFromCons
    _ :< TM.Box tInner ->
      checkActualityType h m dataNameSet tInner
    _ :< TM.PrimType {} ->
      return []
    _ :< TM.Void ->
      return []
    _ :< TM.Resource {} ->
      return []
    _ ->
      return
        [ L.newLog m LL.Error $
            "A term of the following type might be noetic:\n  "
              <> toTextType (weakenType t')
        ]

getActualityConsArgTypes ::
  Handle ->
  Hint ->
  DD.DefiniteDescription ->
  [TM.Type] ->
  App [[BinderF TM.Type]]
getActualityConsArgTypes h m dataName dataArgs = do
  dataInfo <- lookupDataInfoFull h m dataName
  let dataBinders = DI.dataArgs dataInfo
  if length dataBinders == length dataArgs
    then do
      let binderIds = map (\(_, _, x, _) -> x) dataBinders
      let sub = IntMap.fromList $ zip (map Ident.toInt binderIds) (map TmSubst.Type dataArgs)
      forM (DI.consInfoList dataInfo) $ \consInfo -> do
        liftIO $ substActualityConsArgs h sub (DI.consArgs consInfo)
    else
      raiseCritical m $ "Could not specialize constructor metadata for `" <> DD.reify dataName <> "` due to arity mismatch"

substActualityConsArgs :: Handle -> TmSubst.Subst -> [BinderF TM.Type] -> IO [BinderF TM.Type]
substActualityConsArgs h sub consArgs =
  case consArgs of
    [] ->
      return []
    (m, k, x, t) : rest -> do
      let substHandle' = TmSubst.new (gensymHandle h)
      t' <- TmSubst.substType substHandle' sub t
      let opaque = m :< TM.Tau
      let sub' = IntMap.insert (Ident.toInt x) (TmSubst.Type opaque) sub
      rest' <- substActualityConsArgs h sub' rest
      return $ (m, k, x, t') : rest'

checkIntegerCursorType :: Handle -> Hint -> TM.Type -> App [L.Log]
checkIntegerCursorType h m t = do
  t' <- inlineType h m t
  case t' of
    _ :< TM.PrimType (PT.Int _) ->
      return []
    _ ->
      return
        [ L.newLog m LL.Error $
            "Expected:\n  an integer type\nFound:\n  "
              <> toTextType (weakenType t')
        ]

insertStmt :: Handle -> Stmt -> App ()
insertStmt h stmt = do
  case stmt of
    StmtDefine isConstLike stmtKind (SavedHint m) f impArgs expArgs defaultArgs t e -> do
      liftIO $ Type.insert' (typeHandle h) f $ weakenType $ m :< TM.Pi (PK.fromStmtKind stmtKind isConstLike) impArgs expArgs (map fst defaultArgs) t
      liftIO $ Definition.insert' (defHandle h) f impArgs expArgs defaultArgs e t (stmtKindToDefKind stmtKind defaultArgs)
    StmtDefineType isConstLike stmtKind (SavedHint m) f impArgs expArgs defaultArgs t body -> do
      let allBinders = impArgs ++ expArgs ++ map fst defaultArgs
      liftIO $ Type.insert' (typeHandle h) f $ weakenType $ m :< TM.Pi (PK.Normal isConstLike) impArgs expArgs (map fst defaultArgs) t
      registerDataTypeStmt h f stmtKind
      liftIO $ TypeDef.insert' (typeDefHandle h) (SK.toOpacityType stmtKind) f allBinders body
    StmtDefineResource (SavedHint m) dd resourceID _ _ _ resourceSize -> do
      liftIO $ Type.insert' (typeHandle h) dd $ weakenType (m :< TM.Pi (PK.Normal True) [] [] [] (m :< TM.Tau))
      liftIO $ TypeDef.insert' (typeDefHandle h) (SK.toOpacityType SK.Alias) dd [] (m :< TM.Resource dd resourceID)
      case Resource.layoutOf resourceSize of
        Just resourceSize' ->
          liftIO $ Resource.insert (Global.resourceHandle $ globalHandle h) dd resourceSize'
        Nothing ->
          return ()
    StmtVariadic {} ->
      return ()
    StmtForeign _ -> do
      return ()
  insertWeakStmt h $ weakenStmt stmt

insertWeakStmt :: Handle -> WeakStmt -> App ()
insertWeakStmt h stmt = do
  case stmt of
    WeakStmtDefineTerm _ stmtKind m f impArgs expArgs defaultArgs codType e -> do
      liftIO $ WeakDef.insert' (weakDefHandle h) (SK.toOpacityTerm stmtKind) (SK.isDestPassingStmtKind stmtKind) m f impArgs expArgs defaultArgs codType e
    WeakStmtDefineType _ stmtKind _ f impArgs expArgs defaultArgs _ body -> do
      let binders = impArgs ++ expArgs ++ map fst defaultArgs
      liftIO $ WeakTypeDef.insert' (weakTypeDefHandle h) (SK.toOpacityType stmtKind) f binders body
      registerWeakDataTypeStmt h f stmtKind
    WeakStmtDefineResource m dd resourceID _ _ _ _ -> do
      let resourceType = m :< WT.Resource dd resourceID
      liftIO $ WeakTypeDef.insert' (weakTypeDefHandle h) (SK.toOpacityType SK.Alias) dd [] resourceType
    WeakStmtNominal {} -> do
      return ()
    WeakStmtVariadic {} -> do
      return ()
    WeakStmtForeign foreignList ->
      forM_ foreignList $ \(F.Foreign _ externalName domList cod) -> do
        liftIO $ WeakDecl.insert (weakDeclHandle h) (DN.Ext externalName) domList cod

registerDataTypeStmt :: Handle -> DD.DefiniteDescription -> SK.StmtKindType TM.Type -> App ()
registerDataTypeStmt h dataName stmtKind =
  case stmtKind of
    SK.Data _ dataArgs consInfoList _ _ ->
      liftIO $
        Data.insert
          (dataHandle h)
          dataName
          DI.DataInfo
            { DI.dataArgs = dataArgs,
              DI.consInfoList = map snd consInfoList
            }
    _ ->
      return ()

registerWeakDataTypeStmt :: Handle -> DD.DefiniteDescription -> SK.StmtKindType WT.WeakType -> App ()
registerWeakDataTypeStmt h dataName stmtKind =
  case stmtKind of
    SK.Data _ dataArgs consInfoList _ _ ->
      liftIO $
        Data.insertWeak
          (dataHandle h)
          dataName
          DI.DataInfo
            { DI.dataArgs = dataArgs,
              DI.consInfoList = map snd consInfoList
            }
    _ ->
      return ()

lookupDataInfo :: Handle -> Hint -> DD.DefiniteDescription -> App [DI.ConsInfo (BinderF TM.Type)]
lookupDataInfo h m dataName = do
  dataInfoOrNone <- liftIO $ Data.lookup (dataHandle h) dataName
  case dataInfoOrNone of
    Just dataInfo ->
      return $ DI.consInfoList dataInfo
    Nothing ->
      raiseCritical m $ "Could not find constructor metadata for `" <> DD.reify dataName <> "`"

elaborateStmtKindTerm :: Handle -> SK.StmtKindTerm WT.WeakType -> App (SK.StmtKindTerm TM.Type)
elaborateStmtKindTerm h stmtKind =
  case stmtKind of
    SK.Define ->
      return SK.Define
    SK.DestPassing ->
      return SK.DestPassing
    SK.DestPassingInline ->
      return SK.DestPassingInline
    SK.Inline ->
      return SK.Inline
    SK.Constant ->
      return SK.Constant
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
    SK.Data dataName dataArgs consInfoList isNominal shouldOptimize -> do
      dataArgs' <- mapM (elaborateWeakBinder h) dataArgs
      dataArgs'' <- mapM (inlineBinder h) dataArgs'
      consInfoList' <- forM consInfoList $ \(savedHint, consInfo) -> do
        consArgs' <- mapM (elaborateWeakBinder h) (DI.consArgs consInfo)
        consArgs'' <- mapM (inlineBinder h) consArgs'
        layouts <- forM (zip (DI.consArgHints consInfo) consArgs'') $ \(hint, binder) -> do
          resolveFieldLayout h hint binder
        return (savedHint, consInfo {DI.consArgs = consArgs'', DI.consArgLayouts = layouts})
      return $ SK.Data dataName dataArgs'' consInfoList' isNominal shouldOptimize

resolveFieldLayout :: Handle -> DI.FieldHint -> BinderF TM.Type -> App DI.FieldLayout
resolveFieldLayout h hint (_, _, _, t) =
  case hint of
    DI.FieldAuto ->
      return DI.LayoutDirect
    DI.FieldMixed mMix ->
      resolveMixed h S.empty mMix t

resolveMixed :: Handle -> S.Set DD.DefiniteDescription -> Hint -> TM.Type -> App DI.FieldLayout
resolveMixed h visited m ty =
  case ty of
    _ :< TM.Data _ dataName dataArgs -> do
      optDataOrNone <- liftIO $ OptimizableData.lookup (optDataHandle h) dataName
      case optDataOrNone of
        Just OD.Enum ->
          raiseError m $ "the type `" <> showDD h dataName <> "` is an enum and cannot be mixed"
        Just OD.Unary ->
          if S.member dataName visited
            then raiseError m $ cannotMixRecursiveMessage h dataName
            else do
              innerType <- specializeUnaryDataType h m dataName dataArgs
              resolveMixed h (S.insert dataName visited) m innerType
        Nothing ->
          if S.member dataName visited
            then raiseError m $ cannotMixRecursiveMessage h dataName
            else do
              dataInfo <- lookupDataInfoFull h m dataName
              return $ DI.LayoutFlattened $ DI.dataTotalSlotCount (DI.dataArgs dataInfo) (DI.consInfoList dataInfo)
    _ :< TM.Resource dataName _ -> do
      resourceSizeOrNone <- liftIO $ Resource.lookup (Global.resourceHandle (globalHandle h)) dataName
      case resourceSizeOrNone of
        Just (Resource.Flattened slotCount) ->
          return $ DI.LayoutFlattened slotCount
        Just Resource.Direct ->
          raiseError m $ "the resource `" <> showDD h dataName <> "` has no fixed size and cannot be mixed"
        Nothing ->
          raiseError m $ "could not find the size of the resource `" <> showDD h dataName <> "`"
    _ :< TM.Pi {} ->
      return $ DI.LayoutFlattened DI.closureSlotCount
    _ :< TM.Tau ->
      cannotMixFieldType m "the type universe"
    _ :< TM.TVar {} ->
      cannotMixFieldType m "a type variable"
    _ :< TM.TVarGlobal {} ->
      cannotMixFieldType m "a nominal type"
    _ :< TM.TyApp {} ->
      cannotMixFieldType m "a nominal type"
    _ :< TM.Box {} ->
      cannotMixFieldType m "a box type"
    _ :< TM.BoxNoema {} ->
      cannotMixFieldType m "a noema type"
    _ :< TM.Code {} ->
      cannotMixFieldType m "a code type"
    _ :< TM.PrimType {} ->
      cannotMixFieldType m "a primitive type"
    _ :< TM.Void ->
      cannotMixFieldType m "the void type"

cannotMixFieldType :: Hint -> T.Text -> App a
cannotMixFieldType m typeDesc =
  raiseError m $ typeDesc <> " cannot be mixed"

cannotMixRecursiveMessage :: Handle -> DD.DefiniteDescription -> T.Text
cannotMixRecursiveMessage h dataName =
  "the recursive type `" <> showDD h dataName <> "` cannot be mixed"

lookupDataInfoFull :: Handle -> Hint -> DD.DefiniteDescription -> App (DI.DataInfo (BinderF TM.Type))
lookupDataInfoFull h m dataName = do
  dataInfoOrNone <- liftIO $ Data.lookup (dataHandle h) dataName
  case dataInfoOrNone of
    Just dataInfo ->
      return dataInfo
    Nothing ->
      raiseError m $ "could not find the layout of the type `" <> showDD h dataName <> "`"

specializeUnaryDataType :: Handle -> Hint -> DD.DefiniteDescription -> [TM.Type] -> App TM.Type
specializeUnaryDataType h m dataName dataArgs = do
  dataInfo <- lookupDataInfoFull h m dataName
  let dataBinders = DI.dataArgs dataInfo
  when (length dataBinders /= length dataArgs) $ do
    raiseError m $ "arity mismatch while mixing the unary type `" <> showDD h dataName <> "`"
  let binderIds = map (\(_, _, x, _) -> x) dataBinders
  let sub = IntMap.fromList $ zip (map Ident.toInt binderIds) (map TmSubst.Type dataArgs)
  case DI.consInfoList dataInfo of
    [DI.ConsInfo {DI.consArgs = [(_, _, _, t)]}] ->
      liftIO $ TmSubst.substType (TmSubst.new (gensymHandle h)) sub t
    _ ->
      raiseError m $ "broken unary metadata for `" <> showDD h dataName <> "`"

showDD :: Handle -> DD.DefiniteDescription -> T.Text
showDD h =
  ReadableDD.readableDD' (Source.sourceModule (currentSource h))

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
      b' <- PEK.traverseArg (elaborateType h) b
      e' <- elaborate' h e
      let impArgs' = ImpArgs.extract impArgs
      impArgs'' <- mapM (elaborateType h) impArgs'
      expArgs' <- mapM (elaborate' h) expArgs
      defaultArgs' <- case defaultArgs of
        DefaultArgs.Aligned args ->
          mapM (traverse (elaborate' h)) args
        DefaultArgs.ByKey _ ->
          raiseCritical m "Scene.Elaborate.elaborate': found a remaining `ByKey` default argument"
      return $ m :< TM.PiElim b' e' impArgs'' expArgs' defaultArgs'
    m :< WT.PiElimExact {} -> do
      raiseCritical m "Scene.Elaborate.elaborate': found a remaining `exact`"
    m :< WT.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (elaborateType h) dataArgs
      consArgs' <- mapM (elaborate' h) consArgs
      return $ m :< TM.DataIntro attr consName dataArgs' consArgs'
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
          switchSpec <- getSwitchSpec h m t'
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
    m :< WT.BoxIntroLift mt e -> do
      e' <- elaborate' h e
      t <- elaborateActualityMarkerType h m mt
      return $ m :< TM.BoxIntroLift t e'
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
    m :< WT.Actual mt e -> do
      e' <- elaborate' h e
      t <- elaborateActualityMarkerType h m mt
      return $ m :< TM.Actual t e'
    m :< WT.Let opacity (mx, k, x, t) e1 e2 -> do
      e1' <- elaborate' h e1
      t' <- reduceWeakType h t >>= elaborateType h
      e2' <- elaborate' h e2
      let letTerm = m :< TM.Let (WT.reifyOpacity opacity) (mx, k, x, t') e1' e2'
      case opacity of
        WT.Noetic ->
          return $ m :< TM.Actual t' letTerm
        _ ->
          return letTerm
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
        M.Calloc sizeType num size -> do
          sizeType' <- elaborateType h sizeType
          num' <- elaborate' h num
          size' <- elaborate' h size
          return $ m :< TM.Magic (M.Calloc sizeType' num' size')
        M.Malloc sizeType size -> do
          sizeType' <- elaborateType h sizeType
          size' <- elaborate' h size
          return $ m :< TM.Magic (M.Malloc sizeType' size')
        M.Realloc sizeType ptr size -> do
          sizeType' <- elaborateType h sizeType
          ptr' <- elaborate' h ptr
          size' <- elaborate' h size
          return $ m :< TM.Magic (M.Realloc sizeType' ptr' size')
        M.Free unitType ptr -> do
          unitType' <- elaborateType h unitType
          ptr' <- elaborate' h ptr
          return $ m :< TM.Magic (M.Free unitType' ptr')
        M.InspectType mid typeValueExpr typeExpr -> do
          typeValueExpr' <- elaborateType h typeValueExpr
          typeExpr' <- elaborateType h typeExpr
          return $ m :< TM.Magic (M.InspectType mid typeValueExpr' typeExpr')
        M.EqType moduleID typeExpr1 typeExpr2 -> do
          typeExpr1' <- elaborateType h typeExpr1
          typeExpr2' <- elaborateType h typeExpr2
          return $ m :< TM.Magic (M.EqType moduleID typeExpr1' typeExpr2')
        M.ShowType stringTypeExpr typeExpr -> do
          stringTypeExpr' <- elaborateType h stringTypeExpr
          typeExpr' <- elaborateType h typeExpr
          return $ m :< TM.Magic (M.ShowType stringTypeExpr' typeExpr')
        M.StringCons stringTypeExpr rune text -> do
          stringTypeExpr' <- elaborateType h stringTypeExpr
          rune' <- elaborate' h rune
          text' <- elaborate' h text
          return $ m :< TM.Magic (M.StringCons stringTypeExpr' rune' text')
        M.StringUncons mid text -> do
          text' <- elaborate' h text
          return $ m :< TM.Magic (M.StringUncons mid text')
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

elaborateActualityMarkerType :: Handle -> Hint -> Maybe WT.WeakType -> App TM.Type
elaborateActualityMarkerType h m mt =
  case mt of
    Just t ->
      elaborateType h t
    Nothing ->
      raiseCritical m "Found an untyped actuality marker during elaboration"

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
      return $ m :< TM.Data attr name es'
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
    WPV.String coreModuleID t bytes -> do
      strictifyStringLiteral h m coreModuleID bytes t
    WPV.NoeticString t text -> do
      t' <- elaborateType h t
      return $ PV.NoeticString t' text
    WPV.NoeticBinary t bytes -> do
      t' <- elaborateType h t
      return $ PV.NoeticBinary t' bytes
    WPV.Text text ->
      return $ PV.Text text
    WPV.Blob bytes ->
      return $ PV.Blob bytes
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
    _ :< TM.Data _ dataName [] -> do
      consInfoList <- lookupDataInfo h m dataName
      case consInfoList of
        [DI.ConsInfo {DI.consArgs = [(_, _, _, arg)]}] ->
          strictify' h m (weakenType arg)
        _ ->
          raiseNonStrictType m (weakenType t')
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
    _ :< TM.Data _ dataName [] -> do
      consInfoList <- lookupDataInfo h m dataName
      case consInfoList of
        [DI.ConsInfo {DI.consArgs = [(_, _, _, arg)]}] ->
          strictifyDecimalType h m x (weakenType arg)
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
    _ :< TM.Data _ dataName [] -> do
      consInfoList <- lookupDataInfo h m dataName
      case consInfoList of
        [DI.ConsInfo {DI.consArgs = [(_, _, _, arg)]}] ->
          strictifyFloatType h m x (weakenType arg)
        _ ->
          raiseNonFloatType m x (weakenType t')
    _ :< _ ->
      raiseNonFloatType m x (weakenType t')

strictifyStringLiteral :: Handle -> Hint -> MID.ModuleID -> BS.ByteString -> WT.WeakType -> App (PV.PrimValue TM.Type)
strictifyStringLiteral h m coreModuleID bytes t = do
  let stringDD = makeCoreStringDD coreModuleID
  let binaryDD = makeCoreBinaryDD coreModuleID
  t' <- reduceWeakType h t >>= elaborateType h
  case t' of
    _ :< TM.PrimType PT.Text -> do
      text <- decodeStringLiteralBytes m bytes
      return $ PV.Text text
    _ :< TM.PrimType PT.Blob -> do
      return $ PV.Blob bytes
    _ :< TM.BoxNoema inner -> do
      inner' <- reduceWeakType h (weakenType inner) >>= elaborateType h
      if isStringObjectType stringDD inner'
        then do
          text <- decodeStringLiteralBytes m bytes
          return $ PV.NoeticString inner' text
        else
          if isBinaryObjectType binaryDD inner'
            then return $ PV.NoeticBinary inner bytes
            else raiseNonStringType m bytes (weakenType t')
    _ :< _ ->
      raiseNonStringType m bytes (weakenType t')

decodeStringLiteralBytes :: Hint -> BS.ByteString -> App T.Text
decodeStringLiteralBytes m bytes = do
  case decodeUtf8Bytes bytes of
    Right text ->
      return text
    Left reason ->
      raiseError m $ "This string literal is not valid UTF-8.\nReason: " <> reason

isStringObjectType :: DD.DefiniteDescription -> TM.Type -> Bool
isStringObjectType stringDD t =
  case t of
    _ :< TM.Data _ dd [] ->
      dd == stringDD
    _ ->
      False

isBinaryObjectType :: DD.DefiniteDescription -> TM.Type -> Bool
isBinaryObjectType binaryDD t =
  case t of
    _ :< TM.Resource dd _ ->
      dd == binaryDD
    _ ->
      False

makeCoreStringDD :: MID.ModuleID -> DD.DefiniteDescription
makeCoreStringDD moduleID = do
  let stringSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.stringLocator}
  DD.newByGlobalLocator stringSGL BN.stringType

makeCoreBinaryDD :: MID.ModuleID -> DD.DefiniteDescription
makeCoreBinaryDD moduleID = do
  let binarySGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.binaryLocator}
  DD.newByGlobalLocator binarySGL BN.binary

elaborateWeakBinder :: Handle -> BinderF WT.WeakType -> App (BinderF TM.Type)
elaborateWeakBinder h (m, k, x, t) = do
  t' <- elaborateType h t
  return (m, k, x, t')

inlineType :: Handle -> Hint -> TM.Type -> App TM.Type
inlineType h m t = do
  dmap <- liftIO $ Definition.get' (defHandle h)
  typeDefMap <- liftIO $ TypeDef.get' (typeDefHandle h)
  inlineHandle <- liftIO $ Inline.new (gensymHandle h) dmap typeDefMap (dataHandle h) m (inlineLimit h) (specializationTable h) (pendingSpecializationDefs h) (residualCheckList h) False
  Inline.inlineType inlineHandle t

elaborateLet :: Handle -> (BinderF WT.WeakType, WT.WeakTerm) -> App (BinderF TM.Type, TM.Term)
elaborateLet h (xt, e) = do
  xt' <- elaborateWeakBinder h xt
  e' <- elaborate' h e
  return (xt', e')

elaborateLamAttr :: Handle -> AttrL.Attr WT.WeakType -> App (AttrL.Attr TM.Type)
elaborateLamAttr h (AttrL.Attr {lamKind, identity}) =
  case lamKind of
    LK.Normal name isDestPassing codType -> do
      codType' <- elaborateType h codType
      return $ AttrL.Attr {lamKind = LK.Normal name isDestPassing codType', identity}
    LK.Fix opacity isDestPassing xt -> do
      xt' <- elaborateWeakBinder h xt
      return $ AttrL.Attr {lamKind = LK.Fix opacity isDestPassing xt', identity}

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
      switchSpec <- getSwitchSpecForCaseList h m cursorType' clauseList
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
      let consArgIdents = map (\(_, _, x, _) -> x) consArgs
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

raiseNonStringType :: Hint -> BS.ByteString -> WT.WeakType -> App a
raiseNonStringType m bytes t = do
  raiseError m $
    "The term `"
      <> T.pack (show bytes)
      <> "` is a string literal, but its type is neither text, blob, &string, nor &binary: "
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

getSwitchSpec :: Handle -> Hint -> TM.Type -> App SwitchSpec
getSwitchSpec h m cursorType = do
  case cursorType of
    _ :< TM.Data _ dataName _ -> do
      consInfoList <- lookupDataInfo h m dataName
      return $ ConsSwitch $ map (\consInfo -> (DI.consName consInfo, DI.isConstLike consInfo)) consInfoList
    _ :< TM.PrimType (PT.Int _) -> do
      return LiteralSwitch
    _ :< TM.PrimType PT.Rune -> do
      return LiteralSwitch
    _ ->
      raiseError m $
        "This term is expected to be an ADT value or a literal, but found:\n"
          <> toTextType (weakenType cursorType)

getSwitchSpecForCaseList :: Handle -> Hint -> TM.Type -> [DT.Case t a] -> App SwitchSpec
getSwitchSpecForCaseList h m cursorType caseList =
  if any isLiteralCase caseList
    then return LiteralSwitch
    else getSwitchSpec h m cursorType

isLiteralCase :: DT.Case t a -> Bool
isLiteralCase decisionCase =
  case decisionCase of
    DT.LiteralCase {} ->
      True
    _ ->
      False

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
              let varList = map (\(_, _, x, _) -> Ident.toInt x) (WeakTypeDef.typeDefBinders def)
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

stmtKindToDefKind :: SK.StmtKindTerm a -> [(binder, b)] -> Maybe Inline.DefKind
stmtKindToDefKind stmtKind defaultArgs =
  case stmtKind of
    SK.DestPassing ->
      if null defaultArgs
        then Nothing
        else Just Inline.NoInline
    SK.DestPassingInline ->
      Just Inline.Inline
    SK.Inline ->
      Just Inline.Inline
    SK.Constant ->
      Just Inline.Inline
    SK.Macro ->
      Just Inline.Macro
    SK.MacroInline ->
      Just Inline.MacroInline
    SK.DataIntro {} ->
      Just Inline.DataIntro
    _ ->
      if null defaultArgs
        then Nothing
        else Just Inline.NoInline

-- viewStmt :: WeakStmt -> IO ()
-- viewStmt stmt = do
--   case stmt of
--     WeakStmtDefineTerm _ _ m x impArgs expArgs defArgs codType e -> do
--       let defArgs' = map fst defArgs
--       let attr = AttrL.Attr {lamKind = LK.Normal Nothing codType, identity = 0}
--       putStrLn $ T.unpack $ DD.reify x <> "\n" <> toTextType (m :< WT.Pi (PK.Normal False) impArgs expArgs defArgs' codType) <> "\n" <> toText (m :< WT.PiIntro attr impArgs expArgs defArgs e)
--     _ ->
--       return ()
