module Kernel.Parse.Internal.Discern.Name
  ( resolveName,
    resolveConstructor,
    resolveLocator,
    interpretGlobalName,
    interpretGlobalTypeName,
    interpretRuleName,
    resolveDefiniteDescription,
  )
where

import App.App (App)
import App.Run (raiseCritical, raiseError)
import Control.Comonad.Cofree hiding (section)
import Control.Monad
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Kernel.Common.Const qualified as C
import Kernel.Common.GlobalName qualified as GN
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Local.Locator qualified as Locator
import Kernel.Common.Handle.Local.Tag qualified as Tag
import Kernel.Common.ReadableDD
import Kernel.Parse.Internal.Discern.Handle qualified as H
import Kernel.Parse.Internal.Handle.Alias qualified as Alias
import Kernel.Parse.Internal.Handle.NameMap qualified as NameMap
import Kernel.Parse.Internal.Handle.Unused qualified as Unused
import Language.Common.ArgNum qualified as AN
import Language.Common.Attr.VarGlobal qualified as AttrVG
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.DefaultArgs qualified as DefaultArgs
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.GlobalLocator qualified as GL
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
import Language.Common.RuleKind (RuleKind)
import Language.Common.VarKind qualified as VK
import Language.RawTerm.Locator qualified as L
import Language.RawTerm.Name
import Language.WeakTerm.CreateHole qualified as WT
import Language.WeakTerm.WeakPrimValue qualified as WPV
import Language.WeakTerm.WeakTerm qualified as WT
import Logger.Hint

{-# INLINE resolveName #-}
resolveName :: H.Handle -> Hint -> Name -> App (DD.DefiniteDescription, (Hint, GN.GlobalName))
resolveName h m name = do
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
    Var var -> do
      resolveVarOrErr h m var
    Locator l -> do
      Right <$> resolveLocator h m l True

resolveVarOrErr :: H.Handle -> Hint -> T.Text -> App (Either T.Text (DD.DefiniteDescription, (Hint, GN.GlobalName)))
resolveVarOrErr h m name = do
  localLocator <- liftEither $ LL.reflect m name
  candList <- liftIO $ Locator.getPossibleReferents (H.locatorHandle h) localLocator
  candList' <- mapM (NameMap.lookup (H.nameMapHandle h) m) candList
  let foundNameList = Maybe.mapMaybe candFilter $ zip candList candList'
  case foundNameList of
    [] ->
      return $ Left $ "Undefined symbol: " <> name
    [globalVar@(dd, (mDef, gn))] -> do
      liftIO $ Tag.insertGlobalVar (H.tagHandle h) m dd (GN.getIsConstLike gn) mDef
      liftIO $ Unused.deleteLocalLocator (H.unusedHandle h) localLocator
      return $ Right globalVar
    _ -> do
      let mainModule = Env.getMainModule (H.envHandle h)
      let foundNameList' = map (readableDD mainModule . fst) foundNameList
      let candInfo = T.concat $ map ("\n- " <>) foundNameList'
      return $ Left $ "This `" <> name <> "` is ambiguous since it could refer to:" <> candInfo

resolveLocator ::
  H.Handle ->
  Hint ->
  L.Locator ->
  Bool ->
  App (DD.DefiniteDescription, (Hint, GN.GlobalName))
resolveLocator h m (gl, ll) shouldInsertTag = do
  sgl <- Alias.resolveAlias (H.aliasHandle h) m gl
  let cand = DD.new sgl ll
  cand' <- NameMap.lookup (H.nameMapHandle h) m cand
  let foundName = candFilter (cand, cand')
  case foundName of
    Nothing ->
      raiseError m $ "Undefined constant: " <> L.reify (gl, ll)
    Just globalVar@(dd, (mDef, gn)) -> do
      when shouldInsertTag $ do
        let glLen = T.length $ GL.reify gl
        let llLen = T.length $ LL.reify ll
        let sepLen = T.length C.nsSep
        liftIO $ Tag.insertLocator (H.tagHandle h) m dd (GN.getIsConstLike gn) (glLen + llLen + sepLen) mDef
      return globalVar

resolveDefiniteDescription ::
  H.Handle ->
  Hint ->
  DD.DefiniteDescription ->
  App GN.GlobalName
resolveDefiniteDescription h m dd = do
  cand' <- NameMap.lookup (H.nameMapHandle h) m dd
  let foundName = candFilter (dd, cand')
  case foundName of
    Nothing ->
      raiseCritical m $ "Undefined definite description: " <> DD.reify dd
    Just (_, (_, gn)) -> do
      return gn

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
      raiseError m $ "`" <> DD.reify dd <> "` is not a constructor"

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
  let dd' = readableDD' (H.currentModule h) dd
  case gn of
    GN.TopLevelFuncTerm argNum isConstLike isDestPassing isMacro -> do
      ensureTopLevelStage m h dd isMacro
      return $ interpretTopLevelFuncTerm m dd argNum isConstLike isDestPassing
    GN.TopLevelFuncType {} -> do
      raiseError m $ "`" <> dd' <> "` is a type name and cannot appear in term position"
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

interpretGlobalTypeName :: Hint -> DD.DefiniteDescription -> GN.GlobalName -> App WT.WeakType
interpretGlobalTypeName m dd gn = do
  case gn of
    GN.TopLevelFuncTerm {} -> do
      raiseError m $ "`" <> DD.reify dd <> "` is a term name and cannot appear in type position"
    GN.TopLevelFuncType argNum isConstLike _ -> do
      return $ interpretTopLevelFuncType m dd argNum isConstLike
    GN.Data argNum _ isConstLike ->
      return $ interpretTopLevelFuncType m dd argNum isConstLike
    GN.DataIntro {} ->
      raiseError m $ "`" <> DD.reify dd <> "` is a constructor and cannot appear in type position"
    GN.PrimType primNum ->
      return $ m :< WT.PrimType primNum
    GN.PrimOp {} ->
      raiseError m $ "`" <> DD.reify dd <> "` is not a type"
    GN.Rule {} ->
      raiseError m $ "`" <> DD.reify dd <> "` is not a type"

interpretRuleName :: Hint -> DD.DefiniteDescription -> GN.GlobalName -> App RuleKind
interpretRuleName m dd gn = do
  case gn of
    GN.Rule kind ->
      return kind
    _ -> do
      raiseError m $ "`" <> DD.reify dd <> "` is not a macro"

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

ensureTopLevelStage :: Hint -> H.Handle -> DD.DefiniteDescription -> Bool -> App ()
ensureTopLevelStage m h dd isMacro = do
  let stage = H.currentStage h
  let dd' = readableDD' (H.currentModule h) dd
  if isMacro
    then do
      when (stage < 1) $ do
        raiseError m $
          "`"
            <> dd'
            <> "` is a meta definition and can only be used at stage >= 1 (current stage: "
            <> T.pack (show stage)
            <> ")"
    else do
      when (stage > 0) $ do
        raiseError m $
          "`"
            <> dd'
            <> "` is a runtime definition and can only be used at stage <= 0 (current stage: "
            <> T.pack (show stage)
            <> ")"

castFromIntToBool :: H.Handle -> WT.WeakTerm -> App WT.WeakTerm
castFromIntToBool h e@(m :< _) = do
  let i1 = m :< WT.PrimType (PT.Int PNS.IntSize1)
  l <- liftEither $ DD.getLocatorPair m C.coreBool
  (dd, (_, gn)) <- resolveLocator h m l False
  bool <- interpretGlobalTypeName m dd gn
  t <- liftIO $ WT.createTypeHole (H.gensymHandle h) m []
  x1 <- liftIO $ Gensym.newIdentFromText (H.gensymHandle h) "arg"
  x2 <- liftIO $ Gensym.newIdentFromText (H.gensymHandle h) "arg"
  let cmpOpType cod = m :< WT.Pi PK.normal [] [(m, VK.Normal, x1, t), (m, VK.Normal, x2, t)] [] cod
  return $ m :< WT.Magic (M.WeakMagic $ M.LowMagic $ LM.Cast (cmpOpType i1) (cmpOpType bool) e)

candFilter :: (a, Maybe b) -> Maybe (a, b)
candFilter (from, mTo) =
  fmap (from,) mTo
