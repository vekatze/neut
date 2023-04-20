module Scene.Parse.Discern.Symbol
  ( resolveName,
    resolveNameOrErr,
    resolveVarOrLocator,
    resolveLocator,
    resolveConstructor,
    resolveConstructorMaybe,
    resolveAlias,
    interpretDefiniteDescription,
    interpretGlobalName,
  )
where

import Context.Alias qualified as Alias
import Context.App
import Context.Gensym qualified as Gensym
import Context.Global qualified as Global
import Context.Locator qualified as Locator
import Context.Throw qualified as Throw
import Control.Comonad.Cofree hiding (section)
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Entity.Arity qualified as A
import Entity.Const qualified as C
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.GlobalLocator qualified as GL
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.IsConstLike
import Entity.LocalLocator qualified as LL
import Entity.Magic qualified as M
import Entity.PrimNumSize qualified as PNS
import Entity.PrimOp qualified as PO
import Entity.PrimType qualified as PT
import Entity.RawPattern qualified as RP
import Entity.UnresolvedName qualified as UN
import Entity.VarOrLocator
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Entity.WeakTerm qualified as WT

resolveName :: Hint -> T.Text -> App (DD.DefiniteDescription, GN.GlobalName)
resolveName m name = do
  nameOrErr <- resolveNameOrErr m name
  case nameOrErr of
    Left err ->
      Throw.raiseError m err
    Right pair ->
      return pair

resolveNameOrErr :: Hint -> T.Text -> App (Either T.Text (DD.DefiniteDescription, GN.GlobalName))
resolveNameOrErr m name = do
  localLocator <- Throw.liftEither $ LL.reflect m name
  candList <- Locator.getPossibleReferents localLocator
  candList' <- mapM (Global.lookup m) candList
  let foundNameList = Maybe.mapMaybe candFilter $ zip candList candList'
  case foundNameList of
    [] ->
      return $ Left $ "undefined variable: " <> name
    [pair] ->
      return $ Right pair
    _ -> do
      let candInfo = T.concat $ map (("\n- " <>) . DD.reify . fst) foundNameList
      return $ Left $ "this `" <> name <> "` is ambiguous since it could refer to:" <> candInfo

candFilter :: (a, Maybe b) -> Maybe (a, b)
candFilter (from, mTo) =
  fmap (from,) mTo

resolveVarOrLocator :: Hint -> VarOrLocator -> App (Hint, DD.DefiniteDescription, GN.GlobalName)
resolveVarOrLocator mOrig varOrLocator = do
  (dd, gn) <-
    case varOrLocator of
      Var var -> do
        resolveName mOrig var
      Locator gl ll -> do
        resolveLocator mOrig gl ll
  return (mOrig, dd, gn)

resolveLocator ::
  Hint ->
  GL.GlobalLocator ->
  LL.LocalLocator ->
  App (DD.DefiniteDescription, GN.GlobalName)
resolveLocator m gl ll = do
  sgl <- Alias.resolveAlias m gl
  let dd = DD.new sgl ll
  gn <- interpretDefiniteDescription m dd
  return (dd, gn)

interpretDefiniteDescription :: Hint -> DD.DefiniteDescription -> App GN.GlobalName
interpretDefiniteDescription m dd = do
  mgn <- Global.lookup m dd
  case mgn of
    Just gn ->
      return gn
    Nothing ->
      Throw.raiseError m $ "undefined constant: " <> DD.reify dd

resolveConstructor ::
  Hint ->
  RP.RawConsName ->
  App (DD.DefiniteDescription, A.Arity, A.Arity, D.Discriminant, IsConstLike)
resolveConstructor m cons = do
  case cons of
    RP.UnresolvedName (UN.UnresolvedName consName') -> do
      (dd, gn) <- resolveName m consName'
      resolveConstructor' m dd gn
    RP.LocatorPair globalLocator localLocator -> do
      sgl <- Alias.resolveAlias m globalLocator
      let dd = DD.new sgl localLocator
      gn <- interpretDefiniteDescription m dd
      resolveConstructor' m dd gn
    RP.DefiniteDescription dd -> do
      gn <- interpretDefiniteDescription m dd
      resolveConstructor' m dd gn

resolveConstructorMaybe ::
  Hint ->
  DD.DefiniteDescription ->
  GN.GlobalName ->
  App (Maybe (DD.DefiniteDescription, A.Arity, A.Arity, D.Discriminant, IsConstLike))
resolveConstructorMaybe m dd gn = do
  case gn of
    GN.DataIntro dataArity consArity disc isConstLike ->
      return $ Just (dd, dataArity, consArity, disc, isConstLike)
    GN.Alias from gn' ->
      resolveConstructorMaybe m from gn'
    _ ->
      return Nothing

resolveConstructor' ::
  Hint ->
  DD.DefiniteDescription ->
  GN.GlobalName ->
  App (DD.DefiniteDescription, A.Arity, A.Arity, D.Discriminant, IsConstLike)
resolveConstructor' m dd gn = do
  mCons <- resolveConstructorMaybe m dd gn
  case mCons of
    Just v ->
      return v
    Nothing ->
      Throw.raiseError m $ DD.reify dd <> " is not a constructor"

interpretGlobalName :: Hint -> DD.DefiniteDescription -> GN.GlobalName -> App WT.WeakTerm
interpretGlobalName m dd gn = do
  case gn of
    GN.TopLevelFunc arity isConstLike ->
      interpretTopLevelFunc m dd arity isConstLike
    GN.Data arity _ isConstLike ->
      interpretTopLevelFunc m dd arity isConstLike
    GN.DataIntro dataArity consArity _ isConstLike -> do
      let e = m :< WT.VarGlobal dd (A.fromInt $ fromInteger (A.reify dataArity + A.reify consArity))
      if isConstLike
        then return $ m :< WT.PiElim e []
        else return e
    GN.PrimType primNum ->
      return $ m :< WT.Prim (WP.Type primNum)
    GN.PrimOp primOp ->
      case primOp of
        PO.PrimCmpOp {} ->
          castFromIntToBool $ m :< WT.Prim (WP.Value (WPV.Op primOp)) -- i1 to bool
        _ ->
          return $ m :< WT.Prim (WP.Value (WPV.Op primOp))
    GN.Resource ->
      return $ m :< WT.ResourceType dd
    GN.Alias dd' gn' ->
      interpretGlobalName m dd' gn'
    GN.AliasData dd' _ gn' ->
      interpretGlobalName m dd' gn'
    GN.Projection arity isConstLike ->
      interpretTopLevelFunc m dd arity isConstLike

interpretTopLevelFunc ::
  Hint ->
  DD.DefiniteDescription ->
  A.Arity ->
  Bool ->
  App WT.WeakTerm
interpretTopLevelFunc m dd arity isConstLike = do
  if isConstLike
    then return $ m :< WT.PiElim (m :< WT.VarGlobal dd arity) []
    else return $ m :< WT.VarGlobal dd arity

castFromIntToBool :: WT.WeakTerm -> App WT.WeakTerm
castFromIntToBool e@(m :< _) = do
  let i1 = m :< WT.Prim (WP.Type (PT.Int (PNS.IntSize 1)))
  (gl, ll) <- Throw.liftEither $ DD.getLocatorPair m C.coreBool
  bool <- resolveLocator m gl ll >>= uncurry (interpretGlobalName m)
  t <- Gensym.newHole m []
  x1 <- Gensym.newIdentFromText "arg"
  x2 <- Gensym.newIdentFromText "arg"
  let cmpOpType cod = m :< WT.Pi [(m, x1, t), (m, x2, t)] cod
  return $ m :< WT.Magic (M.Cast (cmpOpType i1) (cmpOpType bool) e)

resolveAlias ::
  Hint ->
  DD.DefiniteDescription ->
  GN.GlobalName ->
  App (Hint, DD.DefiniteDescription, GN.GlobalName)
resolveAlias m dd gn =
  case gn of
    GN.Alias dd' gn' ->
      resolveAlias m dd' gn'
    GN.AliasData dd' _ gn' ->
      resolveAlias m dd' gn'
    _ ->
      return (m, dd, gn)
