module Main.Move.Scene.Parse.Discern.Name
  ( resolveName,
    resolveConstructor,
    resolveLocator,
    interpretGlobalName,
  )
where

import Control.Comonad.Cofree hiding (section)
import Control.Monad
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Main.Move.Context.EIO (EIO, raiseError)
import Main.Move.Context.Env qualified as Env
import Main.Move.Context.Locator qualified as Locator
import Main.Move.Context.Tag qualified as Tag
import Main.Move.Context.Gensym qualified as Gensym
import Main.Move.Scene.Parse.Discern.Handle qualified as H
import Main.Move.Scene.Parse.Handle.Alias qualified as Alias
import Main.Move.Scene.Parse.Handle.Global qualified as Global
import Main.Move.Scene.Parse.Handle.Unused qualified as Unused
import Main.Rule.ArgNum qualified as AN
import Main.Rule.Attr.VarGlobal qualified as AttrVG
import Main.Rule.Const qualified as C
import Main.Rule.DefiniteDescription qualified as DD
import Main.Rule.Discriminant qualified as D
import Main.Rule.GlobalLocator qualified as GL
import Main.Rule.GlobalName qualified as GN
import Main.Rule.Hint
import Main.Rule.IsConstLike
import Main.Rule.LocalLocator qualified as LL
import Main.Rule.Locator qualified as L
import Main.Rule.Magic qualified as M
import Main.Rule.Name
import Main.Rule.PrimNumSize qualified as PNS
import Main.Rule.PrimOp qualified as PO
import Main.Rule.PrimType qualified as PT
import Main.Rule.WeakPrim qualified as WP
import Main.Rule.WeakPrimValue qualified as WPV
import Main.Rule.WeakTerm qualified as WT

{-# INLINE resolveName #-}
resolveName :: H.Handle -> Hint -> Name -> EIO (DD.DefiniteDescription, (Hint, GN.GlobalName))
resolveName h m name = do
  nameOrErr <- resolveNameOrError h m name
  case nameOrErr of
    Left err ->
      raiseError m err
    Right pair ->
      return pair

{-# INLINE resolveNameOrError #-}
resolveNameOrError :: H.Handle -> Hint -> Name -> EIO (Either T.Text (DD.DefiniteDescription, (Hint, GN.GlobalName)))
resolveNameOrError h m name =
  case name of
    Var var -> do
      resolveVarOrErr h m var
    Locator l -> do
      Right <$> resolveLocator h m l True

resolveVarOrErr :: H.Handle -> Hint -> T.Text -> EIO (Either T.Text (DD.DefiniteDescription, (Hint, GN.GlobalName)))
resolveVarOrErr h m name = do
  localLocator <- liftEither $ LL.reflect m name
  candList <- liftIO $ Locator.getPossibleReferents (H.locatorHandle h) localLocator
  candList' <- mapM (Global.lookup (H.globalHandle h) m) candList
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
      let foundNameList' = map (Locator.getReadableDD mainModule . fst) foundNameList
      let candInfo = T.concat $ map ("\n- " <>) foundNameList'
      return $ Left $ "This `" <> name <> "` is ambiguous since it could refer to:" <> candInfo

resolveLocator ::
  H.Handle ->
  Hint ->
  L.Locator ->
  Bool ->
  EIO (DD.DefiniteDescription, (Hint, GN.GlobalName))
resolveLocator h m (gl, ll) shouldInsertTag = do
  sgl <- Alias.resolveAlias (H.aliasHandle h) m gl
  let cand = DD.new sgl ll
  cand' <- Global.lookup (H.globalHandle h) m cand
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

resolveConstructor ::
  H.Handle ->
  Hint ->
  Name ->
  EIO (DD.DefiniteDescription, AN.ArgNum, AN.ArgNum, D.Discriminant, IsConstLike, Maybe GN.GlobalName)
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

interpretGlobalName :: H.Handle -> Hint -> DD.DefiniteDescription -> GN.GlobalName -> EIO WT.WeakTerm
interpretGlobalName h m dd gn = do
  case gn of
    GN.TopLevelFunc argNum isConstLike ->
      return $ interpretTopLevelFunc m dd argNum isConstLike
    GN.Data argNum _ isConstLike ->
      return $ interpretTopLevelFunc m dd argNum isConstLike
    GN.DataIntro dataArgNum consArgNum _ isConstLike -> do
      let argNum = AN.add dataArgNum consArgNum
      let attr = AttrVG.Attr {..}
      let e = m :< WT.VarGlobal attr dd
      if isConstLike
        then return $ m :< WT.piElim e []
        else return e
    GN.PrimType primNum ->
      return $ m :< WT.Prim (WP.Type primNum)
    GN.PrimOp primOp ->
      case primOp of
        PO.PrimCmpOp {} ->
          castFromIntToBool h $ m :< WT.Prim (WP.Value (WPV.Op primOp)) -- i1 to bool
        _ ->
          return $ m :< WT.Prim (WP.Value (WPV.Op primOp))

interpretTopLevelFunc ::
  Hint ->
  DD.DefiniteDescription ->
  AN.ArgNum ->
  IsConstLike ->
  WT.WeakTerm
interpretTopLevelFunc m dd argNum isConstLike = do
  let attr = AttrVG.Attr {..}
  if isConstLike
    then m :< WT.piElim (m :< WT.VarGlobal attr dd) []
    else m :< WT.VarGlobal attr dd

castFromIntToBool :: H.Handle -> WT.WeakTerm -> EIO WT.WeakTerm
castFromIntToBool h e@(m :< _) = do
  let i1 = m :< WT.Prim (WP.Type (PT.Int (PNS.IntSize 1)))
  l <- liftEither $ DD.getLocatorPair m C.coreBool
  (dd, (_, gn)) <- resolveLocator h m l False
  bool <- interpretGlobalName h m dd gn
  t <- liftIO $ Gensym.newHole (H.gensymHandle h) m []
  x1 <- liftIO $ Gensym.newIdentFromText (H.gensymHandle h) "arg"
  x2 <- liftIO $ Gensym.newIdentFromText (H.gensymHandle h) "arg"
  let cmpOpType cod = m :< WT.Pi [] [(m, x1, t), (m, x2, t)] cod
  return $ m :< WT.Magic (M.WeakMagic $ M.Cast (cmpOpType i1) (cmpOpType bool) e)

candFilter :: (a, Maybe b) -> Maybe (a, b)
candFilter (from, mTo) =
  fmap (from,) mTo
