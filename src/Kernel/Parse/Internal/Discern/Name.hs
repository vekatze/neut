module Kernel.Parse.Internal.Discern.Name
  ( resolveName,
    resolveTypeName,
    resolveConstructor,
    resolveLocator,
    interpretGlobalName,
    interpretMetaConstant,
    interpretGlobalTypeName,
    resolveDefiniteDescription,
  )
where

import App.App (App)
import App.Run (raiseCritical, raiseError)
import Control.Comonad.Cofree hiding (section)
import Control.Monad
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Kernel.Common.Const qualified as C
import Kernel.Common.GlobalName qualified as GN
import Kernel.Common.Handle.Global.ModulePath (renderDD)
import Kernel.Common.Handle.Local.Locator qualified as Locator
import Kernel.Common.Handle.Local.Tag qualified as Tag
import Kernel.Common.TopNameMap (LookupResult (..))
import Kernel.Parse.Internal.Discern.Handle qualified as H
import Kernel.Parse.Internal.Handle.Alias qualified as Alias
import Kernel.Parse.Internal.Handle.NameMap qualified as NameMap
import Kernel.Parse.Internal.Handle.Unused qualified as Unused
import Kernel.Parse.NominalEnv (NominalEnv)
import Language.Common.ArgNum qualified as AN
import Language.Common.Attr.VarGlobal qualified as AttrVG
import Language.Common.BaseName qualified as BN
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.DefaultArgs qualified as DefaultArgs
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.ImpArgs qualified as ImpArgs
import Language.Common.IsConstLike
import Language.Common.IsDestPassing
import Language.Common.LocalLocator qualified as LL
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.Common.PiElimKind qualified as PEK
import Language.Common.PiKind qualified as PK
import Language.Common.PrimNumSize qualified as PNS
import Language.Common.PrimOp qualified as PO
import Language.Common.PrimType qualified as PT
import Language.Common.StrictGlobalLocator qualified as SGL
import Language.Common.VarKind qualified as VK
import Language.RawTerm.Locator qualified as L
import Language.RawTerm.Name
import Language.WeakTerm.CreateHole qualified as WT
import Language.WeakTerm.WeakPrimValue qualified as WPV
import Language.WeakTerm.WeakTerm qualified as WT
import Logger.Hint

{-# INLINE resolveName #-}
resolveName :: H.Handle -> Hint -> Name -> App (DD.DefiniteDescription, (Hint, GN.GlobalName))
resolveName h =
  resolveNameIn (H.nameEnv h) h

{-# INLINE resolveTypeName #-}
resolveTypeName :: H.Handle -> Hint -> Name -> App (DD.DefiniteDescription, (Hint, GN.GlobalName))
resolveTypeName h =
  resolveNameIn (H.typeNameEnv h) h

resolveNameIn :: NominalEnv -> H.Handle -> Hint -> Name -> App (DD.DefiniteDescription, (Hint, GN.GlobalName))
resolveNameIn localEnv h m name = do
  case name of
    Dotted headText _
      | Just _ <- lookup headText localEnv ->
          raiseError m $ "`" <> headText <> "` is not a namespace"
    _ -> do
      resolveNameWithoutLocals h m name

resolveNameWithoutLocals :: H.Handle -> Hint -> Name -> App (DD.DefiniteDescription, (Hint, GN.GlobalName))
resolveNameWithoutLocals h m name = do
  nameOrErr <- resolveNameOrError h m name
  case nameOrErr of
    Left err ->
      raiseError m err
    Right pair ->
      return pair

{-# INLINE resolveNameOrError #-}
resolveNameOrError :: H.Handle -> Hint -> Name -> App (Either T.Text (DD.DefiniteDescription, (Hint, GN.GlobalName)))
resolveNameOrError h m name =
  case name of
    Bare var -> do
      resolveBareNameOrErr h m var
    Dotted headText rest -> do
      resolveDottedNameOrErr h m headText $ NE.toList rest
    Locator l -> do
      Right <$> resolveLocator h m l True

resolveBareNameOrErr :: H.Handle -> Hint -> T.Text -> App (Either T.Text (DD.DefiniteDescription, (Hint, GN.GlobalName)))
resolveBareNameOrErr h m name = do
  localLocator <- liftEither $ LL.reflect m name
  ownOrNone <- lookupOwnName h m localLocator
  case ownOrNone of
    Just globalVar ->
      finalizeResolution h m localLocator Nothing globalVar
    Nothing -> do
      bindingOrNone <- liftIO $ Locator.lookupImportBinding (H.locatorHandle h) (LL.baseName localLocator)
      case bindingOrNone of
        Just (Locator.NamespaceView _ _) ->
          return $ Left $ "`" <> name <> "` is a namespace and must be followed by a member"
        Just (Locator.ImportedName dd mImportAlias) ->
          resolveSimpleCandidate h m name localLocator mImportAlias dd
        Nothing ->
          resolveSimpleCandidate h m name localLocator Nothing $ DD.new SGL.llvmGlobalLocator localLocator

resolveSimpleCandidate ::
  H.Handle ->
  Hint ->
  T.Text ->
  LL.LocalLocator ->
  Maybe Hint ->
  DD.DefiniteDescription ->
  App (Either T.Text (DD.DefiniteDescription, (Hint, GN.GlobalName)))
resolveSimpleCandidate h m name localLocator mImportAlias dd = do
  lookupResult <- lookupNameMap h m dd
  case lookupResult of
    Found globalVar ->
      finalizeResolution h m localLocator mImportAlias globalVar
    Hidden ->
      return $ Left $ hiddenNameMessage name
    Missing ->
      return $ Left $ "Undefined symbol: " <> name

finalizeResolution ::
  H.Handle ->
  Hint ->
  LL.LocalLocator ->
  Maybe Hint ->
  (DD.DefiniteDescription, (Hint, GN.GlobalName)) ->
  App (Either T.Text (DD.DefiniteDescription, (Hint, GN.GlobalName)))
finalizeResolution h m localLocator mImportAlias globalVar@(dd, (mDef, gn)) = do
  let len = T.length $ LL.reify localLocator
  let mDef' = maybe mDef id mImportAlias
  liftIO $ Tag.insertLocator (H.tagHandle h) m dd (GN.getIsConstLike gn) len mDef'
  liftIO $ Unused.deleteLocalLocator (H.unusedHandle h) localLocator
  return $ Right globalVar

lookupOwnName :: H.Handle -> Hint -> LL.LocalLocator -> App (Maybe (DD.DefiniteDescription, (Hint, GN.GlobalName)))
lookupOwnName h m ll = do
  let cgl = Locator.getCurrentGlobalLocator (H.locatorHandle h)
  let prefixes = reverse $ List.inits (H.nsPath h)
  lookupOwnName' h m cgl prefixes ll

lookupOwnName' ::
  H.Handle ->
  Hint ->
  SGL.StrictGlobalLocator ->
  [[BN.BaseName]] ->
  LL.LocalLocator ->
  App (Maybe (DD.DefiniteDescription, (Hint, GN.GlobalName)))
lookupOwnName' h m cgl prefixes ll =
  case prefixes of
    [] ->
      return Nothing
    prefix : rest -> do
      let dd = DD.new cgl $ LL.prepend prefix ll
      lookupResult <- lookupNameMap h m dd
      case lookupResult of
        Found globalVar ->
          return $ Just globalVar
        _ ->
          lookupOwnName' h m cgl rest ll

lookupNameMap :: H.Handle -> Hint -> DD.DefiniteDescription -> App (LookupResult (DD.DefiniteDescription, (Hint, GN.GlobalName)))
lookupNameMap h m dd = do
  let currentLocator = Locator.getCurrentGlobalLocator (H.locatorHandle h)
  lookupResult <- NameMap.lookup (H.nameMapHandle h) m currentLocator (H.nsPath h) dd
  return $ (dd,) <$> lookupResult

hiddenNameMessage :: T.Text -> T.Text
hiddenNameMessage name =
  "`" <> name <> "` is defined but not visible from here"

resolveDottedNameOrErr :: H.Handle -> Hint -> T.Text -> [BN.BaseName] -> App (Either T.Text (DD.DefiniteDescription, (Hint, GN.GlobalName)))
resolveDottedNameOrErr h m headText rest = do
  headBaseName <- liftEither $ BN.reflect m headText
  ownHeadOrNone <- lookupOwnName h m (LL.new headBaseName)
  case ownHeadOrNone of
    Just (nsDD, (_, GN.Namespace)) -> do
      resolveNamespaceMember h m headText rest Nothing $ DD.extendBody nsDD rest
    Just _ ->
      return $ Left $ "`" <> headText <> "` is not a namespace"
    Nothing -> do
      resolveImportedDottedName h m headText headBaseName rest

resolveImportedDottedName ::
  H.Handle ->
  Hint ->
  T.Text ->
  BN.BaseName ->
  [BN.BaseName] ->
  App (Either T.Text (DD.DefiniteDescription, (Hint, GN.GlobalName)))
resolveImportedDottedName h m headText headBaseName rest = do
  bindingOrNone <- liftIO $ Locator.lookupImportBinding (H.locatorHandle h) headBaseName
  case bindingOrNone of
    Just (Locator.NamespaceView sgl mImportAlias) -> do
      liftIO $ Unused.deleteLocalLocator (H.unusedHandle h) $ LL.new headBaseName
      case rest of
        [] ->
          return $ Left $ "`" <> headText <> "` is a namespace and must be followed by a member"
        b : bs -> do
          liftIO $ Tag.insertNamespaceView (H.tagHandle h) m headText mImportAlias mImportAlias
          resolveNamespaceMember h m headText rest (Just $ toMemberDD sgl) $ DD.new sgl (LL.fromBaseNameList b bs)
    Just (Locator.ImportedName dd mImportAlias) -> do
      let headLL = LL.new headBaseName
      lookupResult <- lookupNameMap h m dd
      case lookupResult of
        Found (dd', (mDef, gn)) ->
          case gn of
            GN.Namespace -> do
              liftIO $ Unused.deleteLocalLocator (H.unusedHandle h) headLL
              case mImportAlias of
                Just importAliasHint ->
                  liftIO $ Tag.insertNamespaceView (H.tagHandle h) m headText importAliasHint importAliasHint
                Nothing ->
                  liftIO $ Tag.insertLocator (H.tagHandle h) m dd' (GN.getIsConstLike gn) (T.length headText) mDef
              resolveNamespaceMember h m headText rest (Just $ DD.extendBody dd) $ DD.extendBody dd rest
            _ ->
              return $ Left $ "`" <> headText <> "` is not a namespace"
        Hidden ->
          return $ Left $ hiddenNameMessage headText
        Missing ->
          return $ Left $ "Undefined symbol: " <> headText
    Nothing ->
      return $ Left $ "Undefined symbol: " <> showDottedName headText rest

resolveNamespaceMember ::
  H.Handle ->
  Hint ->
  T.Text ->
  [BN.BaseName] ->
  Maybe ([BN.BaseName] -> DD.DefiniteDescription) ->
  DD.DefiniteDescription ->
  App (Either T.Text (DD.DefiniteDescription, (Hint, GN.GlobalName)))
resolveNamespaceMember h m headText rest resolverOrNone dd = do
  lookupResult <- lookupNameMap h m dd
  case lookupResult of
    Found globalVar@(dd', (mDef, gn)) -> do
      case resolverOrNone of
        Nothing -> do
          let nameLen = T.length $ showDottedName headText rest
          liftIO $ Tag.insertLocator (H.tagHandle h) m dd' (GN.getIsConstLike gn) nameLen mDef
        Just resolveMemberDD ->
          insertMemberTags h m (T.length headText + T.length C.nsSep) [] rest resolveMemberDD (dd', mDef, gn)
      return $ Right globalVar
    Hidden ->
      return $ Left $ hiddenNameMessage $ showDottedName headText rest
    Missing ->
      return $ Left $ "Undefined symbol: " <> showDottedName headText rest

showDottedName :: T.Text -> [BN.BaseName] -> T.Text
showDottedName headText rest =
  T.intercalate C.nsSep $ headText : map BN.reify rest

toMemberDD :: SGL.StrictGlobalLocator -> [BN.BaseName] -> DD.DefiniteDescription
toMemberDD sgl names = do
  case names of
    [] ->
      error "toMemberDD: empty member path"
    b : bs ->
      DD.new sgl $ LL.fromBaseNameList b bs

insertMemberTags ::
  H.Handle ->
  Hint ->
  Int ->
  [BN.BaseName] ->
  [BN.BaseName] ->
  ([BN.BaseName] -> DD.DefiniteDescription) ->
  (DD.DefiniteDescription, Hint, GN.GlobalName) ->
  App ()
insertMemberTags h m columnOffset prefix rest resolveMemberDD finalInfo =
  case rest of
    [] ->
      return ()
    [b] -> do
      let (dd, mDef, gn) = finalInfo
      insertMemberTag h m columnOffset b dd mDef gn
    b : bs -> do
      let prefix' = prefix ++ [b]
      lookupResult <- lookupNameMap h m $ resolveMemberDD prefix'
      case lookupResult of
        Found (dd, (mDef, gn)) ->
          insertMemberTag h m columnOffset b dd mDef gn
        _ ->
          return ()
      let nextOffset = columnOffset + T.length (BN.reify b) + T.length C.nsSep
      insertMemberTags h m nextOffset prefix' bs resolveMemberDD finalInfo

insertMemberTag ::
  H.Handle ->
  Hint ->
  Int ->
  BN.BaseName ->
  DD.DefiniteDescription ->
  Hint ->
  GN.GlobalName ->
  App ()
insertMemberTag h m columnOffset name dd mDef gn = do
  let (line, column) = metaLocation m
  let memberHint = m {metaLocation = (line, column + columnOffset)}
  let memberLen = T.length $ BN.reify name
  liftIO $ Tag.insertLocator (H.tagHandle h) memberHint dd (GN.getIsConstLike gn) memberLen mDef

resolveLocator ::
  H.Handle ->
  Hint ->
  L.Locator ->
  Bool ->
  App (DD.DefiniteDescription, (Hint, GN.GlobalName))
resolveLocator h m locator shouldInsertTag = do
  let gl = L.globalLocator locator
  let ll = L.localLocator locator
  let modulePathText = L.modulePathText locator
  let sourceText = L.sourceText locator
  sgl <- Alias.resolveAlias (H.aliasHandle h) m gl
  let cand = DD.new sgl ll
  lookupResult <- lookupNameMap h m cand
  case lookupResult of
    Missing ->
      raiseError m $ "Undefined constant: " <> L.reify locator
    Hidden ->
      raiseError m $ hiddenNameMessage $ L.reify locator
    Found globalVar@(dd, (mDef, gn)) -> do
      when shouldInsertTag $ do
        let sepLen = T.length C.doubleColon
        when (metaShouldSaveLocation m) $ do
          moduleLocation <- Alias.getModuleLocation (H.aliasHandle h) m $ SGL.moduleID sgl
          liftIO $ Tag.insertModuleFile (H.tagHandle h) m modulePathText moduleLocation
        let (line, column) = metaLocation m
        let sourceHint = m {metaLocation = (line, column + T.length modulePathText + sepLen)}
        let resolvedSource = SGL.reify $ DD.strictGlobalLocator dd
        let sourceDefHint = mDef {metaLocation = (1, 1)}
        liftIO $ Tag.insertResolvedSourceFile (H.tagHandle h) sourceHint sourceText resolvedSource sourceDefHint
        let bodyOffset = T.length modulePathText + sepLen + T.length sourceText + sepLen
        let bodyHint = m {metaLocation = (line, column + bodyOffset)}
        let bodyPath = LL.baseNameList ll
        let bodySGL = DD.strictGlobalLocator dd
        let resolveMemberDD = toMemberDD bodySGL
        insertMemberTags h bodyHint 0 [] bodyPath resolveMemberDD (dd, mDef, gn)
      return globalVar

resolveDefiniteDescription ::
  H.Handle ->
  Hint ->
  DD.DefiniteDescription ->
  App GN.GlobalName
resolveDefiniteDescription h m dd = do
  lookupResult <- lookupNameMap h m dd
  case lookupResult of
    Found (_, (_, gn)) ->
      return gn
    _ ->
      raiseCritical m $ "Undefined definite description: " <> DD.reify dd

resolveConstructor ::
  H.Handle ->
  Hint ->
  Name ->
  App (DD.DefiniteDescription, AN.ArgNum, AN.ArgNum, D.Discriminant, IsConstLike, Maybe GN.GlobalName)
resolveConstructor h m s = do
  (dd, (_, gn)) <- resolveName h m s
  case resolveConstructorMaybe dd gn of
    Just v ->
      return v
    Nothing ->
      raiseError m $ "`" <> renderDD (H.modulePathMap h) dd <> "` is not a constructor"

resolveConstructorMaybe ::
  DD.DefiniteDescription ->
  GN.GlobalName ->
  Maybe (DD.DefiniteDescription, AN.ArgNum, AN.ArgNum, D.Discriminant, IsConstLike, Maybe GN.GlobalName)
resolveConstructorMaybe dd gn = do
  case gn of
    GN.DataIntro dataArgNum consArgNum disc isConstLike ->
      Just (dd, dataArgNum, consArgNum, disc, isConstLike, Nothing)
    _ ->
      Nothing

interpretGlobalName :: H.Handle -> Hint -> DD.DefiniteDescription -> GN.GlobalName -> App WT.WeakTerm
interpretGlobalName h m dd gn = do
  let dd' = renderDD (H.modulePathMap h) dd
  case gn of
    GN.TopLevelFuncTerm argNum isConstLike isDestPassing -> do
      ensureRuntimeTermStage m h dd
      return $ interpretTopLevelFuncTerm m dd argNum isConstLike isDestPassing
    GN.TopLevelMetaTerm _ True ->
      interpretMetaConstant h m dd gn ImpArgs.Unspecified
    GN.TopLevelMetaTerm argNum isConstLike -> do
      ensureMetaTermStage m h dd
      let isDestPassing = False
      return $ interpretTopLevelFuncTerm m dd argNum isConstLike isDestPassing
    GN.TopLevelFuncType {} -> do
      raiseError m $ "`" <> dd' <> "` is a type name and cannot appear in term position"
    GN.Trope ->
      raiseError m $ "`" <> dd' <> "` is a trope and can only be used by `invoke`"
    GN.Namespace ->
      raiseError m $ "`" <> dd' <> "` is a namespace and cannot appear in term position"
    GN.Data {} ->
      raiseError m $ "`" <> dd' <> "` is a type name and cannot appear in term position"
    GN.DataIntro dataArgNum consArgNum _ isConstLike -> do
      let argNum = AN.add dataArgNum consArgNum
      let isDestPassing = False
      let attr = AttrVG.Attr {..}
      return $ m :< WT.PiElim PEK.Normal (m :< WT.VarGlobal attr dd) ImpArgs.Unspecified [] (DefaultArgs.ByKey [])
    GN.PrimType _ ->
      raiseError m $ "`" <> dd' <> "` is a type name and cannot appear in term position"
    GN.PrimOp primOp ->
      case primOp of
        PO.PrimCmpOp {} ->
          castFromIntToBool h $ m :< WT.Prim (WPV.Op primOp) -- i1 to bool
        _ ->
          return $ m :< WT.Prim (WPV.Op primOp)
    GN.Rule _ ->
      raiseError m $ "`" <> dd' <> "` must be used with arguments"

interpretMetaConstant ::
  H.Handle ->
  Hint ->
  DD.DefiniteDescription ->
  GN.GlobalName ->
  ImpArgs.ImpArgs WT.WeakType ->
  App WT.WeakTerm
interpretMetaConstant h m dd gn impArgs = do
  let h' = h {H.currentStage = H.currentStage h + 1}
  callee <- interpretGlobalName h' m dd $ GN.toMetaFunction gn
  let defaultArgs = DefaultArgs.ByKey []
  let call = m :< WT.PiElim PEK.Normal callee impArgs [] defaultArgs
  return $ m :< WT.CodeElim call

interpretGlobalTypeName :: H.Handle -> Hint -> DD.DefiniteDescription -> GN.GlobalName -> App WT.WeakType
interpretGlobalTypeName h m dd gn = do
  let dd' = renderDD (H.modulePathMap h) dd
  case gn of
    GN.TopLevelFuncTerm {} -> do
      raiseError m $ "`" <> dd' <> "` is a term name and cannot appear in type position"
    GN.TopLevelMetaTerm {} -> do
      raiseError m $ "`" <> dd' <> "` is a term name and cannot appear in type position"
    GN.TopLevelFuncType argNum isConstLike _ -> do
      return $ interpretTopLevelFuncType m dd argNum isConstLike
    GN.Data argNum _ isConstLike ->
      return $ interpretTopLevelFuncType m dd argNum isConstLike
    GN.DataIntro {} ->
      raiseError m $ "`" <> dd' <> "` is a constructor and cannot appear in type position"
    GN.PrimType primNum ->
      return $ m :< WT.PrimType primNum
    GN.PrimOp {} ->
      raiseError m $ "`" <> dd' <> "` is not a type"
    GN.Rule {} ->
      raiseError m $ "`" <> dd' <> "` is not a type"
    GN.Trope ->
      raiseError m $ "`" <> dd' <> "` is a trope and cannot appear in type position"
    GN.Namespace ->
      raiseError m $ "`" <> dd' <> "` is a namespace and cannot appear in type position"

interpretTopLevelFuncTerm ::
  Hint ->
  DD.DefiniteDescription ->
  AN.ArgNum ->
  IsConstLike ->
  IsDestPassing ->
  WT.WeakTerm
interpretTopLevelFuncTerm m dd argNum isConstLike isDestPassing = do
  let attr = AttrVG.Attr {..}
  if isConstLike
    then m :< WT.PiElim PEK.Normal (m :< WT.VarGlobal attr dd) ImpArgs.Unspecified [] (DefaultArgs.ByKey [])
    else m :< WT.VarGlobal attr dd

interpretTopLevelFuncType ::
  Hint ->
  DD.DefiniteDescription ->
  AN.ArgNum ->
  IsConstLike ->
  WT.WeakType
interpretTopLevelFuncType m dd argNum isConstLike = do
  let isDestPassing = False
  let attr = AttrVG.Attr {..}
  if isConstLike
    then m :< WT.TyApp (m :< WT.TVarGlobal attr dd) []
    else m :< WT.TVarGlobal attr dd

ensureRuntimeTermStage :: Hint -> H.Handle -> DD.DefiniteDescription -> App ()
ensureRuntimeTermStage m h dd = do
  let stage = H.currentStage h
  let dd' = renderDD (H.modulePathMap h) dd
  when (stage > 0) $ do
    raiseError m $
      "`"
        <> dd'
        <> "` is a runtime definition and can only be used at stage <= 0 (current stage: "
        <> T.pack (show stage)
        <> ")"

ensureMetaTermStage :: Hint -> H.Handle -> DD.DefiniteDescription -> App ()
ensureMetaTermStage m h dd = do
  let stage = H.currentStage h
  let dd' = renderDD (H.modulePathMap h) dd
  when (stage < 1) $ do
    raiseError m $
      "`"
        <> dd'
        <> "` is a meta definition and can only be used at stage >= 1 (current stage: "
        <> T.pack (show stage)
        <> ")"

castFromIntToBool :: H.Handle -> WT.WeakTerm -> App WT.WeakTerm
castFromIntToBool h e@(m :< _) = do
  let i1 = m :< WT.PrimType (PT.Int PNS.IntSize1)
  l <- liftEither $ L.reflect m C.coreBool
  (dd, (_, gn)) <- resolveLocator h m l False
  bool <- interpretGlobalTypeName h m dd gn
  t <- liftIO $ WT.createTypeHole (H.gensymHandle h) m []
  x1 <- liftIO $ Gensym.newIdentFromText (H.gensymHandle h) "arg"
  x2 <- liftIO $ Gensym.newIdentFromText (H.gensymHandle h) "arg"
  let cmpOpType cod = m :< WT.Pi PK.normal [] [(m, VK.Normal, x1, t), (m, VK.Normal, x2, t)] [] cod
  return $ m :< WT.Magic (M.WeakMagic $ M.LowMagic $ LM.Cast (cmpOpType i1) (cmpOpType bool) e)
