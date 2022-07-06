module Entity.WeakTerm.Discern
  ( discern,
    discernBinder,
    Context (..),
    specialize,
  )
where

import qualified Context.Alias as Alias
import qualified Context.App as App
import qualified Context.Gensym as Gensym
import qualified Context.Global as Global
import qualified Context.Locator as Locator
import qualified Context.Throw as Throw
import Control.Comonad.Cofree
import Control.Monad
import Data.Function
import qualified Data.HashMap.Lazy as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import Entity.Binder
import Entity.EnumCase
import qualified Entity.GlobalName as GN
import Entity.Hint
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import Entity.LamKind
import Entity.WeakTerm

data Context = Context
  { throw :: Throw.Context,
    gensym :: Gensym.Context,
    global :: Global.Context,
    locator :: Locator.Context,
    alias :: Alias.Context
  }

type NameEnv = Map.HashMap T.Text Ident

type IsDefinite = Bool

specialize :: App.Context -> Context
specialize ctx =
  Context
    { throw = ctx & App.throw,
      gensym = ctx & App.gensym,
      global = ctx & App.global,
      locator = ctx & App.locator,
      alias = ctx & App.alias
    }

-- Alpha-convert all the variables so that different variables have different names.
discern :: Context -> NameEnv -> WeakTerm -> IO WeakTerm
discern ctx nenv term =
  case term of
    _ :< WeakTermTau ->
      return term
    m :< WeakTermVar (I (s, _)) -> do
      case Map.lookup s nenv of
        Just name ->
          return $ m :< WeakTermVar name
        Nothing -> do
          resolveName ctx m s "variable" False
    m :< WeakTermVarGlobal x -> do
      resolveName ctx m x "constant" True
    m :< WeakTermPi xts t -> do
      (xts', t') <- discernBinderWithBody ctx nenv xts t
      return $ m :< WeakTermPi xts' t'
    m :< WeakTermPiIntro kind xts e -> do
      case kind of
        LamKindFix xt -> do
          (xt' : xts', e') <- discernBinderWithBody ctx nenv (xt : xts) e
          return $ m :< WeakTermPiIntro (LamKindFix xt') xts' e'
        LamKindCons dataName consName consNumber dataType -> do
          dataType' <- discern ctx nenv dataType
          (xts', e') <- discernBinderWithBody ctx nenv xts e
          return $ m :< WeakTermPiIntro (LamKindCons dataName consName consNumber dataType') xts' e'
        _ -> do
          (xts', e') <- discernBinderWithBody ctx nenv xts e
          return $ m :< WeakTermPiIntro kind xts' e'
    m :< WeakTermPiElim e es -> do
      es' <- mapM (discern ctx nenv) es
      e' <- discern ctx nenv e
      return $ m :< WeakTermPiElim e' es'
    m :< WeakTermSigma xts -> do
      (xts', _) <- discernBinderWithBody ctx nenv xts (m :< WeakTermTau)
      return $ m :< WeakTermSigma xts'
    m :< WeakTermSigmaIntro es -> do
      es' <- mapM (discern ctx nenv) es
      return $ m :< WeakTermSigmaIntro es'
    m :< WeakTermSigmaElim xts e1 e2 -> do
      e1' <- discern ctx nenv e1
      (xts', e2') <- discernBinderWithBody ctx nenv xts e2
      return $ m :< WeakTermSigmaElim xts' e1' e2'
    m :< WeakTermLet mxt e1 e2 -> do
      e1' <- discern ctx nenv e1
      ([mxt'], e2') <- discernBinderWithBody ctx nenv [mxt] e2
      return $ m :< WeakTermLet mxt' e1' e2'
    _ :< WeakTermConst _ ->
      return term
    _ :< WeakTermAster _ ->
      return term
    m :< WeakTermInt t x -> do
      t' <- discern ctx nenv t
      return $ m :< WeakTermInt t' x
    m :< WeakTermFloat t x -> do
      t' <- discern ctx nenv t
      return $ m :< WeakTermFloat t' x
    _ :< WeakTermEnum {} ->
      return term
    _ :< WeakTermEnumIntro _ _ ->
      return term
    m :< WeakTermEnumElim (e, t) caseList -> do
      e' <- discern ctx nenv e
      t' <- discern ctx nenv t
      caseList' <-
        forM caseList $ \(enumCase, body) -> do
          enumCase' <- discernEnumCase ctx enumCase
          body' <- discern ctx nenv body
          return (enumCase', body')
      return $ m :< WeakTermEnumElim (e', t') caseList'
    m :< WeakTermQuestion e t -> do
      e' <- discern ctx nenv e
      t' <- discern ctx nenv t
      return $ m :< WeakTermQuestion e' t'
    m :< WeakTermMagic der -> do
      der' <- traverse (discern ctx nenv) der
      return $ m :< WeakTermMagic der'
    m :< WeakTermMatch mSubject (e, t) clauseList -> do
      mSubject' <- mapM (discern ctx nenv) mSubject
      e' <- discern ctx nenv e
      t' <- discern ctx nenv t
      clauseList' <- forM clauseList $ \((mCons, constructorName, xts), body) -> do
        term' <- resolveName ctx m constructorName "variable" ("::" `T.isInfixOf` constructorName)
        case term' of
          _ :< WeakTermVarGlobal newName -> do
            (xts', body') <- discernBinderWithBody ctx nenv xts body
            return ((mCons, newName, xts'), body')
          _ ->
            (ctx & throw & Throw.raiseError) m $ "no such constructor is defined: " <> constructorName
      return $ m :< WeakTermMatch mSubject' (e', t') clauseList'
    m :< WeakTermNoema s e -> do
      s' <- discern ctx nenv s
      e' <- discern ctx nenv e
      return $ m :< WeakTermNoema s' e'
    m :< WeakTermNoemaIntro (I (x, _)) e ->
      case Map.lookup x nenv of
        Just name -> do
          e' <- discern ctx nenv e
          return $ m :< WeakTermNoemaIntro name e'
        Nothing ->
          (ctx & throw & Throw.raiseError) m $ "undefined subject variable: " <> x
    m :< WeakTermNoemaElim s e -> do
      s' <- Gensym.newIdentFromIdent (ctx & gensym) s
      e' <- discern ctx (Map.insert (Ident.toText s) s' nenv) e
      return $ m :< WeakTermNoemaElim s' e'
    m :< WeakTermArray elemType -> do
      elemType' <- discern ctx nenv elemType
      return $ m :< WeakTermArray elemType'
    m :< WeakTermArrayIntro elemType elems -> do
      elemType' <- discern ctx nenv elemType
      elems' <- mapM (discern ctx nenv) elems
      return $ m :< WeakTermArrayIntro elemType' elems'
    m :< WeakTermArrayAccess subject elemType array index -> do
      subject' <- discern ctx nenv subject
      elemType' <- discern ctx nenv elemType
      array' <- discern ctx nenv array
      index' <- discern ctx nenv index
      return $ m :< WeakTermArrayAccess subject' elemType' array' index'
    _ :< WeakTermText ->
      return term
    _ :< WeakTermTextIntro _ ->
      return term
    m :< WeakTermCell contentType -> do
      contentType' <- discern ctx nenv contentType
      return $ m :< WeakTermCell contentType'
    m :< WeakTermCellIntro contentType content -> do
      contentType' <- discern ctx nenv contentType
      content' <- discern ctx nenv content
      return $ m :< WeakTermCellIntro contentType' content'
    m :< WeakTermCellRead cell -> do
      cell' <- discern ctx nenv cell
      return $ m :< WeakTermCellRead cell'
    m :< WeakTermCellWrite cell newValue -> do
      cell' <- discern ctx nenv cell
      newValue' <- discern ctx nenv newValue
      return $ m :< WeakTermCellWrite cell' newValue'

discernBinder ::
  Context ->
  NameEnv ->
  [BinderF WeakTerm] ->
  IO ([BinderF WeakTerm], NameEnv)
discernBinder ctx nenv binder =
  case binder of
    [] -> do
      return ([], nenv)
    (mx, x, t) : xts -> do
      t' <- discern ctx nenv t
      x' <- Gensym.newIdentFromIdent (ctx & gensym) x
      (xts', nenv') <- discernBinder ctx (Map.insert (Ident.toText x) x' nenv) xts
      return ((mx, x', t') : xts', nenv')

discernBinderWithBody ::
  Context ->
  NameEnv ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  IO ([BinderF WeakTerm], WeakTerm)
discernBinderWithBody ctx nenv binder e = do
  (binder', nenv') <- discernBinder ctx nenv binder
  e' <- discern ctx nenv' e
  return (binder', e')

discernEnumCase :: Context -> EnumCase -> IO EnumCase
discernEnumCase ctx enumCase =
  case enumCase of
    m :< EnumCaseLabel _ l -> do
      term <- resolveName ctx m l "variable" False
      case term of
        _ :< WeakTermEnumIntro labelInfo label ->
          return $ m :< EnumCaseLabel labelInfo label
        _ ->
          Throw.raiseError (throw ctx) m $ "no such enum-value is defined: " <> l
    _ ->
      return enumCase

resolveName :: Context -> Hint -> T.Text -> T.Text -> IsDefinite -> IO WeakTerm
resolveName ctx m name termKind isDefinite = do
  candList <- Alias.getCandList (alias ctx) name isDefinite
  candList' <- mapM (Global.lookup (global ctx)) candList
  let foundNameList = Maybe.mapMaybe candFilter $ zip candList candList'
  case foundNameList of
    [] ->
      Throw.raiseError (throw ctx) m $ "undefined " <> termKind <> ": " <> name
    [(name', GN.TopLevelFunc)] ->
      return $ m :< WeakTermVarGlobal name'
    [(name', GN.Enum _)] ->
      return $ m :< WeakTermEnum name'
    [(name', GN.EnumIntro enumTypeName discriminant)] ->
      return $ m :< WeakTermEnumIntro (enumTypeName, discriminant) name'
    [(name', GN.Constant)] ->
      return $ m :< WeakTermConst name'
    _ -> do
      let candInfo = T.concat $ map (("\n- " <>) . fst) foundNameList
      Throw.raiseError (throw ctx) m $
        "this `" <> name <> "` is ambiguous since it could refer to:" <> candInfo

candFilter :: (a, Maybe b) -> Maybe (a, b)
candFilter (from, mTo) =
  fmap (from,) mTo
