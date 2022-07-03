module Entity.WeakTerm.Discern
  ( discern,
    discernBinder,
    Axis (..),
  )
where

import qualified Context.Enum as Enum
import qualified Context.Gensym as Gensym
import qualified Context.Global as Global
import qualified Context.Locator as Locator
import qualified Context.Throw as Throw
import Control.Comonad.Cofree
import Control.Monad
import Data.Function
import qualified Data.HashMap.Lazy as Map
import qualified Data.Text as T
import Entity.Binder
import Entity.EnumCase
import Entity.Hint
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import Entity.LamKind
import Entity.Namespace
import Entity.WeakTerm

data Axis = Axis
  { throw :: Throw.Context,
    gensym :: Gensym.Axis,
    enum :: Enum.Axis,
    global :: Global.Axis,
    locator :: Locator.Axis
  }

type NameEnv = Map.HashMap T.Text Ident

type IsDefinite = Bool

-- Alpha-convert all the variables so that different variables have different names.
discern :: Axis -> NameEnv -> WeakTerm -> IO WeakTerm
discern axis nenv term =
  case term of
    _ :< WeakTermTau ->
      return term
    m :< WeakTermVar (I (s, _)) -> do
      case Map.lookup s nenv of
        Just name ->
          return $ m :< WeakTermVar name
        Nothing -> do
          resolveVar axis m s "variable" False
    m :< WeakTermVarGlobal x -> do
      resolveVar axis m x "constant" True
    m :< WeakTermPi xts t -> do
      (xts', t') <- discernBinderWithBody axis nenv xts t
      return $ m :< WeakTermPi xts' t'
    m :< WeakTermPiIntro kind xts e -> do
      case kind of
        LamKindFix xt -> do
          (xt' : xts', e') <- discernBinderWithBody axis nenv (xt : xts) e
          return $ m :< WeakTermPiIntro (LamKindFix xt') xts' e'
        LamKindCons dataName consName consNumber dataType -> do
          dataType' <- discern axis nenv dataType
          (xts', e') <- discernBinderWithBody axis nenv xts e
          return $ m :< WeakTermPiIntro (LamKindCons dataName consName consNumber dataType') xts' e'
        _ -> do
          (xts', e') <- discernBinderWithBody axis nenv xts e
          return $ m :< WeakTermPiIntro kind xts' e'
    m :< WeakTermPiElim e es -> do
      es' <- mapM (discern axis nenv) es
      e' <- discern axis nenv e
      return $ m :< WeakTermPiElim e' es'
    m :< WeakTermSigma xts -> do
      (xts', _) <- discernBinderWithBody axis nenv xts (m :< WeakTermTau)
      return $ m :< WeakTermSigma xts'
    m :< WeakTermSigmaIntro es -> do
      es' <- mapM (discern axis nenv) es
      return $ m :< WeakTermSigmaIntro es'
    m :< WeakTermSigmaElim xts e1 e2 -> do
      e1' <- discern axis nenv e1
      (xts', e2') <- discernBinderWithBody axis nenv xts e2
      return $ m :< WeakTermSigmaElim xts' e1' e2'
    m :< WeakTermLet mxt e1 e2 -> do
      e1' <- discern axis nenv e1
      ([mxt'], e2') <- discernBinderWithBody axis nenv [mxt] e2
      return $ m :< WeakTermLet mxt' e1' e2'
    _ :< WeakTermConst _ ->
      return term
    _ :< WeakTermAster _ ->
      return term
    m :< WeakTermInt t x -> do
      t' <- discern axis nenv t
      return $ m :< WeakTermInt t' x
    m :< WeakTermFloat t x -> do
      t' <- discern axis nenv t
      return $ m :< WeakTermFloat t' x
    _ :< WeakTermEnum {} ->
      return term
    _ :< WeakTermEnumIntro _ _ ->
      return term
    m :< WeakTermEnumElim (e, t) caseList -> do
      e' <- discern axis nenv e
      t' <- discern axis nenv t
      caseList' <-
        forM caseList $ \(enumCase, body) -> do
          enumCase' <- discernEnumCase axis enumCase
          body' <- discern axis nenv body
          return (enumCase', body')
      return $ m :< WeakTermEnumElim (e', t') caseList'
    m :< WeakTermQuestion e t -> do
      e' <- discern axis nenv e
      t' <- discern axis nenv t
      return $ m :< WeakTermQuestion e' t'
    m :< WeakTermMagic der -> do
      der' <- traverse (discern axis nenv) der
      return $ m :< WeakTermMagic der'
    m :< WeakTermMatch mSubject (e, t) clauseList -> do
      mSubject' <- mapM (discern axis nenv) mSubject
      e' <- discern axis nenv e
      t' <- discern axis nenv t
      clauseList' <- forM clauseList $ \((mCons, constructorName, xts), body) -> do
        candList <- getCandidates axis constructorName ("::" `T.isInfixOf` constructorName)
        constructorName' <- resolveSymbol (axis & throw) m (asConstructor (axis & global) m) constructorName candList
        case constructorName' of
          Just (_, newName) -> do
            (xts', body') <- discernBinderWithBody axis nenv xts body
            return ((mCons, newName, xts'), body')
          Nothing ->
            (axis & throw & Throw.raiseError) m $ "no such constructor is defined: " <> constructorName
      return $ m :< WeakTermMatch mSubject' (e', t') clauseList'
    m :< WeakTermNoema s e -> do
      s' <- discern axis nenv s
      e' <- discern axis nenv e
      return $ m :< WeakTermNoema s' e'
    m :< WeakTermNoemaIntro (I (x, _)) e ->
      case Map.lookup x nenv of
        Just name -> do
          e' <- discern axis nenv e
          return $ m :< WeakTermNoemaIntro name e'
        Nothing ->
          (axis & throw & Throw.raiseError) m $ "undefined subject variable: " <> x
    m :< WeakTermNoemaElim s e -> do
      s' <- Gensym.newIdentFromIdent (axis & gensym) s
      e' <- discern axis (Map.insert (Ident.toText s) s' nenv) e
      return $ m :< WeakTermNoemaElim s' e'
    m :< WeakTermArray elemType -> do
      elemType' <- discern axis nenv elemType
      return $ m :< WeakTermArray elemType'
    m :< WeakTermArrayIntro elemType elems -> do
      elemType' <- discern axis nenv elemType
      elems' <- mapM (discern axis nenv) elems
      return $ m :< WeakTermArrayIntro elemType' elems'
    m :< WeakTermArrayAccess subject elemType array index -> do
      subject' <- discern axis nenv subject
      elemType' <- discern axis nenv elemType
      array' <- discern axis nenv array
      index' <- discern axis nenv index
      return $ m :< WeakTermArrayAccess subject' elemType' array' index'
    _ :< WeakTermText ->
      return term
    _ :< WeakTermTextIntro _ ->
      return term
    m :< WeakTermCell contentType -> do
      contentType' <- discern axis nenv contentType
      return $ m :< WeakTermCell contentType'
    m :< WeakTermCellIntro contentType content -> do
      contentType' <- discern axis nenv contentType
      content' <- discern axis nenv content
      return $ m :< WeakTermCellIntro contentType' content'
    m :< WeakTermCellRead cell -> do
      cell' <- discern axis nenv cell
      return $ m :< WeakTermCellRead cell'
    m :< WeakTermCellWrite cell newValue -> do
      cell' <- discern axis nenv cell
      newValue' <- discern axis nenv newValue
      return $ m :< WeakTermCellWrite cell' newValue'

discernBinder ::
  Axis ->
  NameEnv ->
  [BinderF WeakTerm] ->
  IO ([BinderF WeakTerm], NameEnv)
discernBinder axis nenv binder =
  case binder of
    [] -> do
      return ([], nenv)
    (mx, x, t) : xts -> do
      t' <- discern axis nenv t
      x' <- Gensym.newIdentFromIdent (axis & gensym) x
      (xts', nenv') <- discernBinder axis (Map.insert (Ident.toText x) x' nenv) xts
      return ((mx, x', t') : xts', nenv')

discernBinderWithBody ::
  Axis ->
  NameEnv ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  IO ([BinderF WeakTerm], WeakTerm)
discernBinderWithBody axis nenv binder e = do
  (binder', nenv') <- discernBinder axis nenv binder
  e' <- discern axis nenv' e
  return (binder', e')

discernEnumCase :: Axis -> EnumCase -> IO EnumCase
discernEnumCase axis enumCase =
  case enumCase of
    m :< EnumCaseLabel _ l -> do
      candList <- getCandidates axis l False
      ml <- resolveSymbol (axis & throw) m (asEnumCase (axis & enum) m) l $ l : candList
      case ml of
        Just l' ->
          return l'
        Nothing -> do
          (axis & throw & Throw.raiseError) m $ "no such enum-value is defined: " <> l
    _ ->
      return enumCase

resolveVar :: Axis -> Hint -> T.Text -> T.Text -> IsDefinite -> IO WeakTerm
resolveVar axis m x termKind isDefinite = do
  candList <- getCandidates axis x isDefinite
  tryCand (resolveSymbol (axis & throw) m (asGlobalVar (axis & global) m) x candList) $ do
    let candList' = x : candList
    tryCand (resolveSymbol (axis & throw) m (asEnumIntro (axis & enum) m) x candList') $ do
      tryCand (resolveSymbol (axis & throw) m (asEnum (axis & enum) m) x candList') $
        tryCand (resolveSymbol (axis & throw) m (return . asWeakConstant m) x candList') $ do
          (axis & throw & Throw.raiseError) m $ "undefined " <> termKind <> ": " <> x

getCandidates :: Axis -> T.Text -> Bool -> IO [T.Text]
getCandidates axis name isDefinite = do
  constructCandList (axis & locator) name isDefinite
