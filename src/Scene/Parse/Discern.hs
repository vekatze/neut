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
import qualified Entity.EnumCase as EC
import qualified Entity.EnumTypeName as ET
import qualified Entity.EnumValueName as EV
import qualified Entity.GlobalName as GN
import Entity.Hint
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import qualified Entity.LamKind as LK
import qualified Entity.LocalLocator as LL
import qualified Entity.PreTerm as PT
import qualified Entity.Prim as Prim
import Entity.Stmt
import qualified Entity.UnresolvedName as UN
import qualified Entity.WeakTerm as WT

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
    PreStmtSection section innerStmtList : rest -> do
      Locator.withSection section $ do
        innerStmtList' <- discernStmtList innerStmtList
        rest' <- discernStmtList rest
        return $ innerStmtList' ++ rest'

-- Alpha-convert all the variables so that different variables have different names.
discern :: Context m => NameEnv -> PT.PreTerm -> m WT.WeakTerm
discern nenv term =
  case term of
    m :< PT.Tau ->
      return $ m :< WT.Tau
    m :< PT.Var (I (s, _)) -> do
      case lookup s nenv of
        Just (_, name) ->
          return $ m :< WT.Var name
        Nothing -> do
          resolveName m s
    m :< PT.VarGlobal globalLocator localLocator -> do
      sgl <- Alias.resolveAlias m globalLocator
      resolveDefiniteDescription m $ DD.new sgl localLocator
    m :< PT.VarGlobalStrict dd -> do
      resolveDefiniteDescription m dd
    m :< PT.Pi xts t -> do
      (xts', t') <- discernBinderWithBody nenv xts t
      return $ m :< WT.Pi xts' t'
    m :< PT.PiIntro kind xts e -> do
      case kind of
        LK.Fix xt -> do
          (xt', xts', e') <- discernBinderWithBody' nenv xt xts e
          return $ m :< WT.PiIntro (LK.Fix xt') xts' e'
        LK.Cons dataName consName consNumber dataType -> do
          dataType' <- discern nenv dataType
          (xts', e') <- discernBinderWithBody nenv xts e
          return $ m :< WT.PiIntro (LK.Cons dataName consName consNumber dataType') xts' e'
        LK.Normal -> do
          (xts', e') <- discernBinderWithBody nenv xts e
          return $ m :< WT.PiIntro LK.Normal xts' e'
    m :< PT.PiElim e es -> do
      es' <- mapM (discern nenv) es
      e' <- discern nenv e
      return $ m :< WT.PiElim e' es'
    m :< PT.Sigma xts -> do
      (xts', _) <- discernBinderWithBody nenv xts (m :< PT.Tau)
      return $ m :< WT.Sigma xts'
    m :< PT.SigmaIntro es -> do
      es' <- mapM (discern nenv) es
      return $ m :< WT.SigmaIntro es'
    m :< PT.SigmaElim xts e1 e2 -> do
      e1' <- discern nenv e1
      (xts', e2') <- discernBinderWithBody nenv xts e2
      return $ m :< WT.SigmaElim xts' e1' e2'
    m :< PT.Let mxt e1 e2 -> do
      e1' <- discern nenv e1
      (mxt', _, e2') <- discernBinderWithBody' nenv mxt [] e2
      return $ m :< WT.Let mxt' e1' e2'
    m :< PT.Prim prim ->
      return $ m :< WT.Prim prim
    m :< PT.Aster k ->
      return $ m :< WT.Aster k (map (\(_, (mx, x)) -> mx :< WT.Var x) nenv)
    m :< PT.Int t x -> do
      t' <- discern nenv t
      return $ m :< WT.Int t' x
    m :< PT.Float t x -> do
      t' <- discern nenv t
      return $ m :< WT.Float t' x
    m :< PT.Enum t ->
      return $ m :< WT.Enum t
    m :< PT.EnumIntro label -> do
      label' <- discernEnumLabel m label
      return $ m :< WT.EnumIntro label'
    m :< PT.EnumElim (e, t) caseList -> do
      e' <- discern nenv e
      t' <- discern nenv t
      caseList' <-
        forM caseList $ \(enumCase, body) -> do
          enumCase' <- discernEnumCase enumCase
          body' <- discern nenv body
          return (enumCase', body')
      return $ m :< WT.EnumElim (e', t') caseList'
    m :< PT.Question e t -> do
      e' <- discern nenv e
      t' <- discern nenv t
      return $ m :< WT.Question e' t'
    m :< PT.Magic der -> do
      der' <- traverse (discern nenv) der
      return $ m :< WT.Magic der'
    m :< PT.Match (e, t) clauseList -> do
      e' <- discern nenv e
      t' <- discern nenv t
      clauseList' <- forM clauseList $ \((mCons, cons, xts), body) -> do
        (cons', unresolvedConsName) <- resolveConstructor mCons cons
        case cons' of
          _ :< WT.VarGlobal consName arity -> do
            (xts', body') <- discernBinderWithBody nenv xts body
            return ((mCons, consName, arity, xts'), body')
          _ ->
            Throw.raiseError m $ "no such constructor is defined: " <> unresolvedConsName
      return $ m :< WT.Match (e', t') clauseList'

discernBinder ::
  Context m =>
  NameEnv ->
  [BinderF PT.PreTerm] ->
  m ([BinderF WT.WeakTerm], NameEnv)
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
  m ([BinderF WT.WeakTerm], WT.WeakTerm)
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
  m (BinderF WT.WeakTerm, [BinderF WT.WeakTerm], WT.WeakTerm)
discernBinderWithBody' nenv (mx, x, t) binder e = do
  t' <- discern nenv t
  x' <- Gensym.newIdentFromIdent x
  (binder', e') <- discernBinderWithBody ((Ident.toText x, (mx, x')) : nenv) binder e
  return ((mx, x', t'), binder', e')

discernEnumLabel :: Context m => Hint -> EC.PreEnumLabel -> m EC.EnumLabel
discernEnumLabel m (EC.PreEnumLabel _ _ (UN.UnresolvedName name)) = do
  term <- resolveName m name
  case term of
    _ :< WT.EnumIntro label ->
      return label
    _ ->
      Throw.raiseError m $
        "no such enum-value is defined: " <> name

discernEnumCase :: Context m => EC.PreEnumCase -> m EC.EnumCase
discernEnumCase enumCase =
  case enumCase of
    m :< EC.Label l -> do
      l' <- discernEnumLabel m l
      return $ m :< EC.Label l'
    m :< EC.Int i -> do
      return $ m :< EC.Int i
    m :< EC.Default -> do
      return $ m :< EC.Default

resolveName :: Context m => Hint -> T.Text -> m WT.WeakTerm
resolveName m name = do
  localLocator <- LL.reflect m name
  candList <- Locator.getPossibleReferents localLocator
  candList' <- mapM Global.lookup candList
  let foundNameList = Maybe.mapMaybe candFilter $ zip candList candList'
  case foundNameList of
    [] ->
      Throw.raiseError m $ "undefined variable: " <> name
    [(dd, GN.TopLevelFunc arity)] ->
      return $ m :< WT.VarGlobal dd arity
    [(dd, GN.Data arity _)] ->
      return $ m :< WT.VarGlobal dd arity
    [(name', GN.EnumType _)] ->
      return $ m :< WT.Enum (ET.EnumTypeName name')
    [(name', GN.EnumIntro enumTypeName discriminant)] ->
      return $ m :< WT.EnumIntro (EC.EnumLabel enumTypeName discriminant (EV.EnumValueName name'))
    [(_, GN.PrimType primNum)] ->
      return $ m :< WT.Prim (Prim.Type primNum)
    [(_, GN.PrimOp primOp)] ->
      return $ m :< WT.Prim (Prim.Op primOp)
    _ -> do
      let candInfo = T.concat $ map (("\n- " <>) . DD.reify . fst) foundNameList
      Throw.raiseError m $
        "this `" <> name <> "` is ambiguous since it could refer to:" <> candInfo

candFilter :: (a, Maybe b) -> Maybe (a, b)
candFilter (from, mTo) =
  fmap (from,) mTo

resolveDefiniteDescription :: Context m => Hint -> DD.DefiniteDescription -> m WT.WeakTerm
resolveDefiniteDescription m dd = do
  kind <- Global.lookup dd
  case kind of
    Just (GN.TopLevelFunc arity) ->
      return $ m :< WT.VarGlobal dd arity
    Just (GN.Data arity _) ->
      return $ m :< WT.VarGlobal dd arity
    Just (GN.EnumType _) ->
      return $ m :< WT.Enum (ET.EnumTypeName dd)
    Just (GN.EnumIntro enumTypeName discriminant) ->
      return $ m :< WT.EnumIntro (EC.EnumLabel enumTypeName discriminant (EV.EnumValueName dd))
    Just (GN.PrimType primNum) ->
      return $ m :< WT.Prim (Prim.Type primNum)
    Just (GN.PrimOp primOp) ->
      return $ m :< WT.Prim (Prim.Op primOp)
    Nothing ->
      Throw.raiseError m $ "undefined definite description: " <> DD.reify dd

resolveConstructor ::
  Context m =>
  Hint ->
  Either UN.UnresolvedName DD.DefiniteDescription ->
  m (WT.WeakTerm, T.Text)
resolveConstructor m cons = do
  case cons of
    Left (UN.UnresolvedName consName') -> do
      term <- resolveName m consName'
      return (term, consName')
    Right dd -> do
      term <- resolveDefiniteDescription m dd
      return (term, DD.reify dd)
