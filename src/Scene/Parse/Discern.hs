module Scene.Parse.Discern
  ( discernStmtList,
    Context,
  )
where

import qualified Context.Alias as Alias
import qualified Context.Gensym as Gensym
import qualified Context.Global as Global
import qualified Context.Locator as Locator
import qualified Context.Throw as Throw
import Control.Comonad.Cofree hiding (section)
import Control.Monad
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
import Entity.Stmt
import qualified Entity.UnresolvedName as UN
import Entity.WeakTerm

class
  ( Throw.Context m,
    Gensym.Context m,
    Global.Context m,
    Locator.Context m,
    Alias.Context m
  ) =>
  Context m

type NameEnv = [(T.Text, (Hint, Ident))]

empty :: NameEnv
empty = []

discernStmtList :: Context m => [PreStmt] -> m [WeakStmt]
discernStmtList stmtList =
  case stmtList of
    [] ->
      return []
    PreStmtDefine isReducible m functionName impArgNum xts codType e : rest -> do
      (xts', nenv) <- discernBinder empty xts
      codType' <- discern nenv codType
      e' <- discern nenv e
      rest' <- discernStmtList rest
      return $ WeakStmtDefine isReducible m functionName impArgNum xts' codType' e' : rest'
    PreStmtDefineResource m name discarder copier : rest -> do
      discarder' <- discern empty discarder
      copier' <- discern empty copier
      rest' <- discernStmtList rest
      return $ WeakStmtDefineResource m name discarder' copier' : rest'
    PreStmtSection section innerStmtList : rest -> do
      Locator.withSection section $ do
        innerStmtList' <- discernStmtList innerStmtList
        rest' <- discernStmtList rest
        return $ innerStmtList' ++ rest'

-- Alpha-convert all the variables so that different variables have different names.
discern :: Context m => NameEnv -> PT.PreTerm -> m WeakTerm
discern nenv term =
  case term of
    m :< PT.Tau ->
      return $ m :< WeakTermTau
    m :< PT.Var (I (s, _)) -> do
      case lookup s nenv of
        Just (_, name) ->
          return $ m :< WeakTermVar name
        Nothing -> do
          resolveName m s
    m :< PT.VarGlobal globalLocator localLocator -> do
      sgl <- Alias.resolveAlias m globalLocator
      resolveDefiniteDescription m $ DD.new sgl localLocator
    m :< PT.VarGlobalStrict dd -> do
      resolveDefiniteDescription m dd
    m :< PT.Pi xts t -> do
      (xts', t') <- discernBinderWithBody nenv xts t
      return $ m :< WeakTermPi xts' t'
    m :< PT.PiIntro kind xts e -> do
      case kind of
        LamKindFix xt -> do
          (xt', xts', e') <- discernBinderWithBody' nenv xt xts e
          return $ m :< WeakTermPiIntro (LamKindFix xt') xts' e'
        LamKindCons dataName consName consNumber dataType -> do
          dataType' <- discern nenv dataType
          (xts', e') <- discernBinderWithBody nenv xts e
          return $ m :< WeakTermPiIntro (LamKindCons dataName consName consNumber dataType') xts' e'
        LamKindNormal -> do
          (xts', e') <- discernBinderWithBody nenv xts e
          return $ m :< WeakTermPiIntro LamKindNormal xts' e'
    m :< PT.PiElim e es -> do
      es' <- mapM (discern nenv) es
      e' <- discern nenv e
      return $ m :< WeakTermPiElim e' es'
    m :< PT.Sigma xts -> do
      (xts', _) <- discernBinderWithBody nenv xts (m :< PT.Tau)
      return $ m :< WeakTermSigma xts'
    m :< PT.SigmaIntro es -> do
      es' <- mapM (discern nenv) es
      return $ m :< WeakTermSigmaIntro es'
    m :< PT.SigmaElim xts e1 e2 -> do
      e1' <- discern nenv e1
      (xts', e2') <- discernBinderWithBody nenv xts e2
      return $ m :< WeakTermSigmaElim xts' e1' e2'
    m :< PT.Let mxt e1 e2 -> do
      e1' <- discern nenv e1
      (mxt', _, e2') <- discernBinderWithBody' nenv mxt [] e2
      return $ m :< WeakTermLet mxt' e1' e2'
    m :< PT.Prim prim ->
      return $ m :< WeakTermPrim prim
    m :< PT.Aster k ->
      return $ m :< WeakTermAster k (map (\(_, (mx, x)) -> mx :< WeakTermVar x) nenv)
    m :< PT.Int t x -> do
      t' <- discern nenv t
      return $ m :< WeakTermInt t' x
    m :< PT.Float t x -> do
      t' <- discern nenv t
      return $ m :< WeakTermFloat t' x
    m :< PT.Enum t ->
      return $ m :< WeakTermEnum t
    m :< PT.EnumIntro label -> do
      label' <- discernEnumLabel m label
      return $ m :< WeakTermEnumIntro label'
    m :< PT.EnumElim (e, t) caseList -> do
      e' <- discern nenv e
      t' <- discern nenv t
      caseList' <-
        forM caseList $ \(enumCase, body) -> do
          enumCase' <- discernEnumCase enumCase
          body' <- discern nenv body
          return (enumCase', body')
      return $ m :< WeakTermEnumElim (e', t') caseList'
    m :< PT.Question e t -> do
      e' <- discern nenv e
      t' <- discern nenv t
      return $ m :< WeakTermQuestion e' t'
    m :< PT.Magic der -> do
      der' <- traverse (discern nenv) der
      return $ m :< WeakTermMagic der'
    m :< PT.Match mSubject (e, t) clauseList -> do
      mSubject' <- mapM (discern nenv) mSubject
      e' <- discern nenv e
      t' <- discern nenv t
      clauseList' <- forM clauseList $ \((mCons, cons, xts), body) -> do
        (cons', unresolvedConsName) <- resolveConstructor mCons cons
        case cons' of
          _ :< WeakTermVarGlobal consName arity -> do
            (xts', body') <- discernBinderWithBody nenv xts body
            return ((mCons, consName, arity, xts'), body')
          _ ->
            Throw.raiseError m $ "no such constructor is defined: " <> unresolvedConsName
      return $ m :< WeakTermMatch mSubject' (e', t') clauseList'
    m :< PT.Noema s e -> do
      s' <- discern nenv s
      e' <- discern nenv e
      return $ m :< WeakTermNoema s' e'
    m :< PT.NoemaIntro (I (x, _)) e ->
      case lookup x nenv of
        Just (_, name) -> do
          e' <- discern nenv e
          return $ m :< WeakTermNoemaIntro name e'
        Nothing ->
          Throw.raiseError m $ "undefined subject variable: " <> x
    m :< PT.NoemaElim s e -> do
      s' <- Gensym.newIdentFromIdent s
      e' <- discern ((Ident.toText s, (m, s')) : nenv) e
      return $ m :< WeakTermNoemaElim s' e'
    m :< PT.Array elemType -> do
      elemType' <- discern nenv elemType
      return $ m :< WeakTermArray elemType'
    m :< PT.ArrayIntro elemType elems -> do
      elemType' <- discern nenv elemType
      elems' <- mapM (discern nenv) elems
      return $ m :< WeakTermArrayIntro elemType' elems'
    m :< PT.ArrayAccess subject elemType array index -> do
      subject' <- discern nenv subject
      elemType' <- discern nenv elemType
      array' <- discern nenv array
      index' <- discern nenv index
      return $ m :< WeakTermArrayAccess subject' elemType' array' index'
    m :< PT.Text ->
      return $ m :< WeakTermText
    m :< PT.TextIntro txt ->
      return $ m :< WeakTermTextIntro txt
    m :< PT.Cell contentType -> do
      contentType' <- discern nenv contentType
      return $ m :< WeakTermCell contentType'
    m :< PT.CellIntro contentType content -> do
      contentType' <- discern nenv contentType
      content' <- discern nenv content
      return $ m :< WeakTermCellIntro contentType' content'
    m :< PT.CellRead cell -> do
      cell' <- discern nenv cell
      return $ m :< WeakTermCellRead cell'
    m :< PT.CellWrite cell newValue -> do
      cell' <- discern nenv cell
      newValue' <- discern nenv newValue
      return $ m :< WeakTermCellWrite cell' newValue'

discernBinder ::
  Context m =>
  NameEnv ->
  [BinderF PT.PreTerm] ->
  m ([BinderF WeakTerm], NameEnv)
discernBinder nenv binder =
  case binder of
    [] -> do
      return ([], nenv)
    (mx, x, t) : xts -> do
      t' <- discern nenv t
      x' <- Gensym.newIdentFromIdent x
      (xts', nenv') <- discernBinder ((Ident.toText x, (mx, x')) : nenv) xts
      return ((mx, x', t') : xts', nenv')

discernBinderWithBody ::
  Context m =>
  NameEnv ->
  [BinderF PT.PreTerm] ->
  PT.PreTerm ->
  m ([BinderF WeakTerm], WeakTerm)
discernBinderWithBody nenv binder e = do
  (binder', nenv') <- discernBinder nenv binder
  e' <- discern nenv' e
  return (binder', e')

discernBinderWithBody' ::
  Context m =>
  NameEnv ->
  BinderF PT.PreTerm ->
  [BinderF PT.PreTerm] ->
  PT.PreTerm ->
  m (BinderF WeakTerm, [BinderF WeakTerm], WeakTerm)
discernBinderWithBody' nenv (mx, x, t) binder e = do
  t' <- discern nenv t
  x' <- Gensym.newIdentFromIdent x
  (binder', e') <- discernBinderWithBody ((Ident.toText x, (mx, x')) : nenv) binder e
  return ((mx, x', t'), binder', e')

discernEnumLabel :: Context m => Hint -> PreEnumLabel -> m EnumLabel
discernEnumLabel m (PreEnumLabel _ _ (UN.UnresolvedName name)) = do
  term <- resolveName m name
  case term of
    _ :< WeakTermEnumIntro label ->
      return label
    _ ->
      Throw.raiseError m $
        "no such enum-value is defined: " <> name

discernEnumCase :: Context m => PreEnumCase -> m EnumCase
discernEnumCase enumCase =
  case enumCase of
    m :< EnumCaseLabel l -> do
      l' <- discernEnumLabel m l
      return $ m :< EnumCaseLabel l'
    m :< EnumCaseInt i -> do
      return $ m :< EnumCaseInt i
    m :< EnumCaseDefault -> do
      return $ m :< EnumCaseDefault

resolveName :: Context m => Hint -> T.Text -> m WeakTerm
resolveName m name = do
  localLocator <- LL.reflect m name
  candList <- Locator.getPossibleReferents localLocator
  candList' <- mapM Global.lookup candList
  let foundNameList = Maybe.mapMaybe candFilter $ zip candList candList'
  case foundNameList of
    [] ->
      Throw.raiseError m $ "undefined variable: " <> name
    [(dd, GN.TopLevelFunc arity)] ->
      return $ m :< WeakTermVarGlobal dd arity
    [(dd, GN.Data arity _)] ->
      return $ m :< WeakTermVarGlobal dd arity
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
      Throw.raiseError m $
        "this `" <> name <> "` is ambiguous since it could refer to:" <> candInfo

candFilter :: (a, Maybe b) -> Maybe (a, b)
candFilter (from, mTo) =
  fmap (from,) mTo

resolveDefiniteDescription :: Context m => Hint -> DD.DefiniteDescription -> m WeakTerm
resolveDefiniteDescription m dd = do
  kind <- Global.lookup dd
  case kind of
    Just (GN.TopLevelFunc arity) ->
      return $ m :< WeakTermVarGlobal dd arity
    Just (GN.Data arity _) ->
      return $ m :< WeakTermVarGlobal dd arity
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
      Throw.raiseError m $ "undefined definite description: " <> DD.reify dd

resolveConstructor ::
  Context m =>
  Hint ->
  Either UN.UnresolvedName DD.DefiniteDescription ->
  m (WeakTerm, T.Text)
resolveConstructor m cons = do
  case cons of
    Left (UN.UnresolvedName consName') -> do
      term <- resolveName m consName'
      return (term, consName')
    Right dd -> do
      term <- resolveDefiniteDescription m dd
      return (term, DD.reify dd)
