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
import qualified Data.HashMap.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import Entity.Binder
import qualified Entity.DefiniteDescription as DD
import Entity.EnumCase
import qualified Entity.EnumTypeName as ET
import qualified Entity.EnumValueName as EV
import qualified Entity.GlobalName as GN
import Entity.Hint
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import Entity.LamKind
import qualified Entity.LocalLocator as LL
import qualified Entity.PreTerm as PT
import qualified Entity.Prim as Prim
import qualified Entity.UnresolvedName as UN
import Entity.WeakTerm

data Context = Context
  { throw :: Throw.Context,
    gensym :: Gensym.Context,
    global :: Global.Context,
    locator :: Locator.Context,
    alias :: Alias.Context
  }

type NameEnv = Map.HashMap T.Text Ident

specialize :: App.Context -> Context
specialize ctx =
  Context
    { throw = App.throw ctx,
      gensym = App.gensym ctx,
      global = App.global ctx,
      locator = App.locator ctx,
      alias = App.alias ctx
    }

-- Alpha-convert all the variables so that different variables have different names.
discern :: Context -> NameEnv -> PT.PreTerm -> IO WeakTerm
discern ctx nenv term =
  case term of
    m :< PT.Tau ->
      return $ m :< WeakTermTau
    m :< PT.Var (I (s, _)) -> do
      case Map.lookup s nenv of
        Just name ->
          return $ m :< WeakTermVar name
        Nothing -> do
          resolveName ctx m s
    m :< PT.VarGlobal globalLocator localLocator -> do
      sgl <- Alias.resolveAlias (alias ctx) m globalLocator
      resolveDefiniteDescription ctx m $ DD.new sgl localLocator
    m :< PT.VarGlobalStrict dd -> do
      resolveDefiniteDescription ctx m dd
    m :< PT.Pi xts t -> do
      (xts', t') <- discernBinderWithBody ctx nenv xts t
      return $ m :< WeakTermPi xts' t'
    m :< PT.PiIntro kind xts e -> do
      case kind of
        LamKindFix xt -> do
          (xt' : xts', e') <- discernBinderWithBody ctx nenv (xt : xts) e
          return $ m :< WeakTermPiIntro (LamKindFix xt') xts' e'
        LamKindCons dataName consName consNumber dataType -> do
          dataType' <- discern ctx nenv dataType
          (xts', e') <- discernBinderWithBody ctx nenv xts e
          return $ m :< WeakTermPiIntro (LamKindCons dataName consName consNumber dataType') xts' e'
        LamKindNormal -> do
          (xts', e') <- discernBinderWithBody ctx nenv xts e
          return $ m :< WeakTermPiIntro LamKindNormal xts' e'
    m :< PT.PiElim e es -> do
      es' <- mapM (discern ctx nenv) es
      e' <- discern ctx nenv e
      return $ m :< WeakTermPiElim e' es'
    m :< PT.Sigma xts -> do
      (xts', _) <- discernBinderWithBody ctx nenv xts (m :< PT.Tau)
      return $ m :< WeakTermSigma xts'
    m :< PT.SigmaIntro es -> do
      es' <- mapM (discern ctx nenv) es
      return $ m :< WeakTermSigmaIntro es'
    m :< PT.SigmaElim xts e1 e2 -> do
      e1' <- discern ctx nenv e1
      (xts', e2') <- discernBinderWithBody ctx nenv xts e2
      return $ m :< WeakTermSigmaElim xts' e1' e2'
    m :< PT.Let mxt e1 e2 -> do
      e1' <- discern ctx nenv e1
      ([mxt'], e2') <- discernBinderWithBody ctx nenv [mxt] e2
      return $ m :< WeakTermLet mxt' e1' e2'
    m :< PT.Prim prim ->
      return $ m :< WeakTermPrim prim
    m :< PT.Aster k ->
      return $ m :< WeakTermAster k
    m :< PT.Int t x -> do
      t' <- discern ctx nenv t
      return $ m :< WeakTermInt t' x
    m :< PT.Float t x -> do
      t' <- discern ctx nenv t
      return $ m :< WeakTermFloat t' x
    m :< PT.Enum t ->
      return $ m :< WeakTermEnum t
    m :< PT.EnumIntro label -> do
      label' <- discernEnumLabel ctx m label
      return $ m :< WeakTermEnumIntro label'
    m :< PT.EnumElim (e, t) caseList -> do
      e' <- discern ctx nenv e
      t' <- discern ctx nenv t
      caseList' <-
        forM caseList $ \(enumCase, body) -> do
          enumCase' <- discernEnumCase ctx enumCase
          body' <- discern ctx nenv body
          return (enumCase', body')
      return $ m :< WeakTermEnumElim (e', t') caseList'
    m :< PT.Question e t -> do
      e' <- discern ctx nenv e
      t' <- discern ctx nenv t
      return $ m :< WeakTermQuestion e' t'
    m :< PT.Magic der -> do
      der' <- traverse (discern ctx nenv) der
      return $ m :< WeakTermMagic der'
    m :< PT.Match mSubject (e, t) clauseList -> do
      mSubject' <- mapM (discern ctx nenv) mSubject
      e' <- discern ctx nenv e
      t' <- discern ctx nenv t
      clauseList' <- forM clauseList $ \((mCons, cons, xts), body) -> do
        (cons', consName) <- resolveConstructor ctx mCons cons
        case cons' of
          _ :< WeakTermVarGlobal newName -> do
            (xts', body') <- discernBinderWithBody ctx nenv xts body
            return ((mCons, newName, xts'), body')
          _ ->
            Throw.raiseError (throw ctx) m $ "no such constructor is defined: " <> consName
      return $ m :< WeakTermMatch mSubject' (e', t') clauseList'
    m :< PT.Noema s e -> do
      s' <- discern ctx nenv s
      e' <- discern ctx nenv e
      return $ m :< WeakTermNoema s' e'
    m :< PT.NoemaIntro (I (x, _)) e ->
      case Map.lookup x nenv of
        Just name -> do
          e' <- discern ctx nenv e
          return $ m :< WeakTermNoemaIntro name e'
        Nothing ->
          Throw.raiseError (throw ctx) m $ "undefined subject variable: " <> x
    m :< PT.NoemaElim s e -> do
      s' <- Gensym.newIdentFromIdent (gensym ctx) s
      e' <- discern ctx (Map.insert (Ident.toText s) s' nenv) e
      return $ m :< WeakTermNoemaElim s' e'
    m :< PT.Array elemType -> do
      elemType' <- discern ctx nenv elemType
      return $ m :< WeakTermArray elemType'
    m :< PT.ArrayIntro elemType elems -> do
      elemType' <- discern ctx nenv elemType
      elems' <- mapM (discern ctx nenv) elems
      return $ m :< WeakTermArrayIntro elemType' elems'
    m :< PT.ArrayAccess subject elemType array index -> do
      subject' <- discern ctx nenv subject
      elemType' <- discern ctx nenv elemType
      array' <- discern ctx nenv array
      index' <- discern ctx nenv index
      return $ m :< WeakTermArrayAccess subject' elemType' array' index'
    m :< PT.Text ->
      return $ m :< WeakTermText
    m :< PT.TextIntro txt ->
      return $ m :< WeakTermTextIntro txt
    m :< PT.Cell contentType -> do
      contentType' <- discern ctx nenv contentType
      return $ m :< WeakTermCell contentType'
    m :< PT.CellIntro contentType content -> do
      contentType' <- discern ctx nenv contentType
      content' <- discern ctx nenv content
      return $ m :< WeakTermCellIntro contentType' content'
    m :< PT.CellRead cell -> do
      cell' <- discern ctx nenv cell
      return $ m :< WeakTermCellRead cell'
    m :< PT.CellWrite cell newValue -> do
      cell' <- discern ctx nenv cell
      newValue' <- discern ctx nenv newValue
      return $ m :< WeakTermCellWrite cell' newValue'

discernBinder ::
  Context ->
  NameEnv ->
  [BinderF PT.PreTerm] ->
  IO ([BinderF WeakTerm], NameEnv)
discernBinder ctx nenv binder =
  case binder of
    [] -> do
      return ([], nenv)
    (mx, x, t) : xts -> do
      t' <- discern ctx nenv t
      x' <- Gensym.newIdentFromIdent (gensym ctx) x
      (xts', nenv') <- discernBinder ctx (Map.insert (Ident.toText x) x' nenv) xts
      return ((mx, x', t') : xts', nenv')

discernBinderWithBody ::
  Context ->
  NameEnv ->
  [BinderF PT.PreTerm] ->
  PT.PreTerm ->
  IO ([BinderF WeakTerm], WeakTerm)
discernBinderWithBody ctx nenv binder e = do
  (binder', nenv') <- discernBinder ctx nenv binder
  e' <- discern ctx nenv' e
  return (binder', e')

discernEnumLabel :: Context -> Hint -> PreEnumLabel -> IO EnumLabel
discernEnumLabel ctx m (PreEnumLabel _ _ (UN.UnresolvedName name)) = do
  term <- resolveName ctx m name
  case term of
    _ :< WeakTermEnumIntro label ->
      return label
    _ ->
      Throw.raiseError (throw ctx) m $
        "no such enum-value is defined: " <> name

discernEnumCase :: Context -> PreEnumCase -> IO EnumCase
discernEnumCase ctx enumCase =
  case enumCase of
    m :< EnumCaseLabel l -> do
      l' <- discernEnumLabel ctx m l
      return $ m :< EnumCaseLabel l'
    m :< EnumCaseInt i -> do
      return $ m :< EnumCaseInt i
    m :< EnumCaseDefault -> do
      return $ m :< EnumCaseDefault

resolveName :: Context -> Hint -> T.Text -> IO WeakTerm
resolveName ctx m name = do
  let localLocator = LL.reflect name
  candList <- Locator.getPossibleReferents (locator ctx) localLocator
  candList' <- mapM (Global.lookup (global ctx)) candList
  let foundNameList = Maybe.mapMaybe candFilter $ zip candList candList'
  case foundNameList of
    [] -> do
      print name
      print candList
      print candList'
      Throw.raiseError (throw ctx) m $ "undefined variable: " <> name
    [(dd, GN.TopLevelFunc)] ->
      return $ m :< WeakTermVarGlobal dd
    [(name', GN.EnumType _)] ->
      return $ m :< WeakTermEnum (ET.EnumTypeName name')
    [(name', GN.EnumIntro enumTypeName discriminant)] ->
      return $ m :< WeakTermEnumIntro (EnumLabel enumTypeName discriminant (EV.EnumValueName name'))
    [(_, GN.PrimType primNum)] ->
      return $ m :< WeakTermPrim (Prim.Type primNum)
    [(_, GN.PrimOp primOp)] ->
      return $ m :< WeakTermPrim (Prim.Op primOp)
    [(dd, GN.Resource)] ->
      return $ m :< WeakTermResourceType dd
    _ -> do
      let candInfo = T.concat $ map (("\n- " <>) . DD.reify . fst) foundNameList
      Throw.raiseError (throw ctx) m $
        "this `" <> name <> "` is ambiguous since it could refer to:" <> candInfo

candFilter :: (a, Maybe b) -> Maybe (a, b)
candFilter (from, mTo) =
  fmap (from,) mTo

resolveDefiniteDescription :: Context -> Hint -> DD.DefiniteDescription -> IO WeakTerm
resolveDefiniteDescription ctx m dd = do
  kind <- Global.lookup (global ctx) dd
  case kind of
    Just GN.TopLevelFunc ->
      return $ m :< WeakTermVarGlobal dd
    Just (GN.EnumType _) ->
      return $ m :< WeakTermEnum (ET.EnumTypeName dd)
    Just (GN.EnumIntro enumTypeName discriminant) ->
      return $ m :< WeakTermEnumIntro (EnumLabel enumTypeName discriminant (EV.EnumValueName dd))
    Just (GN.PrimType primNum) ->
      return $ m :< WeakTermPrim (Prim.Type primNum)
    Just (GN.PrimOp primOp) ->
      return $ m :< WeakTermPrim (Prim.Op primOp)
    Just GN.Resource ->
      return $ m :< WeakTermResourceType dd
    Nothing ->
      Throw.raiseError (throw ctx) m $ "undefined definite description: " <> DD.reify dd

resolveConstructor ::
  Context ->
  Hint ->
  Either UN.UnresolvedName DD.DefiniteDescription ->
  IO (WeakTerm, T.Text)
resolveConstructor ctx m cons = do
  case cons of
    Left (UN.UnresolvedName consName') -> do
      term <- resolveName ctx m consName'
      return (term, consName')
    Right dd -> do
      term <- resolveDefiniteDescription ctx m dd
      return (term, DD.reify dd)
