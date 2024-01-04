module Scene.Parse.Discern.Name
  ( resolveName,
    resolveConstructor,
    resolveLocator,
    interpretGlobalName,
  )
where

import Context.Alias qualified as Alias
import Context.App
import Context.Gensym qualified as Gensym
import Context.Global qualified as Global
import Context.Locator qualified as Locator
import Context.Tag qualified as Tag
import Context.Throw qualified as Throw
import Context.UnusedLocalLocator qualified as UnusedLocalLocator
import Control.Comonad.Cofree hiding (section)
import Control.Monad
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Entity.ArgNum qualified as AN
import Entity.Attr.VarGlobal qualified as AttrVG
import Entity.Const qualified as C
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.GlobalLocator qualified as GL
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.IsConstLike
import Entity.LocalLocator qualified as LL
import Entity.LocationTree qualified as LT
import Entity.Locator qualified as L
import Entity.Magic qualified as M
import Entity.Name
import Entity.PrimNumSize qualified as PNS
import Entity.PrimOp qualified as PO
import Entity.PrimType qualified as PT
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Entity.WeakTerm qualified as WT

{-# INLINE resolveName #-}
resolveName :: Hint -> Name -> App (DD.DefiniteDescription, (Hint, GN.GlobalName))
resolveName m name = do
  nameOrErr <- resolveNameOrError m name
  case nameOrErr of
    Left err ->
      Throw.raiseError m err
    Right pair ->
      return pair

{-# INLINE resolveNameOrError #-}
resolveNameOrError :: Hint -> Name -> App (Either T.Text (DD.DefiniteDescription, (Hint, GN.GlobalName)))
resolveNameOrError m name =
  case name of
    Var var -> do
      resolveVarOrErr m var
    Locator l -> do
      Right <$> resolveLocator m l True

resolveVarOrErr :: Hint -> T.Text -> App (Either T.Text (DD.DefiniteDescription, (Hint, GN.GlobalName)))
resolveVarOrErr m name = do
  localLocator <- Throw.liftEither $ LL.reflect m name
  candList <- Locator.getPossibleReferents localLocator
  candList' <- mapM (Global.lookup m) candList
  let foundNameList = Maybe.mapMaybe candFilter $ zip candList candList'
  case foundNameList of
    [] ->
      return $ Left $ "undefined symbol: " <> name
    [globalVar@(dd, (mDef, _))] -> do
      Tag.insertGlobalVar m dd mDef
      UnusedLocalLocator.delete localLocator
      return $ Right globalVar
    _ -> do
      let candInfo = T.concat $ map (("\n- " <>) . DD.reify . fst) foundNameList
      return $ Left $ "this `" <> name <> "` is ambiguous since it could refer to:" <> candInfo

resolveLocator ::
  Hint ->
  L.Locator ->
  Bool ->
  App (DD.DefiniteDescription, (Hint, GN.GlobalName))
resolveLocator m (gl, ll) shouldInsertTag = do
  sgl <- Alias.resolveAlias m gl
  let cand = DD.new sgl ll
  cand' <- Global.lookup m cand
  let foundName = candFilter (cand, cand')
  case foundName of
    Nothing ->
      Throw.raiseError m $ "undefined constant: " <> L.reify (gl, ll)
    Just globalVar@(dd, (mDef, _)) -> do
      when shouldInsertTag $ do
        let glLen = T.length $ GL.reify gl
        let llLen = T.length $ LL.reify ll
        let sepLen = T.length C.nsSep
        Tag.insert m (LT.SymbolLoc (LT.Global dd)) (glLen + sepLen + llLen) mDef
      return globalVar

resolveConstructor ::
  Hint ->
  Name ->
  App (DD.DefiniteDescription, AN.ArgNum, AN.ArgNum, D.Discriminant, IsConstLike, Maybe GN.GlobalName)
resolveConstructor m s = do
  (dd, (_, gn)) <- resolveName m s
  mCons <- resolveConstructorMaybe dd gn
  case mCons of
    Just v ->
      return v
    Nothing ->
      Throw.raiseError m $ DD.reify dd <> " is not a constructor"

resolveConstructorMaybe ::
  DD.DefiniteDescription ->
  GN.GlobalName ->
  App (Maybe (DD.DefiniteDescription, AN.ArgNum, AN.ArgNum, D.Discriminant, IsConstLike, Maybe GN.GlobalName))
resolveConstructorMaybe dd gn = do
  case gn of
    GN.DataIntro dataArgNum consArgNum disc isConstLike ->
      return $ Just (dd, dataArgNum, consArgNum, disc, isConstLike, Nothing)
    _ ->
      return Nothing

interpretGlobalName :: Hint -> DD.DefiniteDescription -> GN.GlobalName -> App WT.WeakTerm
interpretGlobalName m dd gn = do
  case gn of
    GN.TopLevelFunc argNum isConstLike ->
      interpretTopLevelFunc m dd argNum isConstLike
    GN.Data argNum _ isConstLike ->
      interpretTopLevelFunc m dd argNum isConstLike
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
          castFromIntToBool $ m :< WT.Prim (WP.Value (WPV.Op primOp)) -- i1 to bool
        _ ->
          return $ m :< WT.Prim (WP.Value (WPV.Op primOp))

interpretTopLevelFunc ::
  Hint ->
  DD.DefiniteDescription ->
  AN.ArgNum ->
  IsConstLike ->
  App WT.WeakTerm
interpretTopLevelFunc m dd argNum isConstLike = do
  let attr = AttrVG.Attr {..}
  if isConstLike
    then return $ m :< WT.piElim (m :< WT.VarGlobal attr dd) []
    else return $ m :< WT.VarGlobal attr dd

castFromIntToBool :: WT.WeakTerm -> App WT.WeakTerm
castFromIntToBool e@(m :< _) = do
  let i1 = m :< WT.Prim (WP.Type (PT.Int (PNS.IntSize 1)))
  l <- Throw.liftEither $ DD.getLocatorPair m C.coreBool
  (dd, (_, gn)) <- resolveLocator m l False
  bool <- interpretGlobalName m dd gn
  t <- Gensym.newHole m []
  x1 <- Gensym.newIdentFromText "arg"
  x2 <- Gensym.newIdentFromText "arg"
  let cmpOpType cod = m :< WT.Pi [] [(m, x1, t), (m, x2, t)] cod
  return $ m :< WT.Magic (M.Cast (cmpOpType i1) (cmpOpType bool) e)

candFilter :: (a, Maybe b) -> Maybe (a, b)
candFilter (from, mTo) =
  fmap (from,) mTo
