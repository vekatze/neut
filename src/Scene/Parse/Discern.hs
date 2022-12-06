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
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Entity.Arity as A
import Entity.Binder
import qualified Entity.DecisionTree as DT
import qualified Entity.DefiniteDescription as DD
import qualified Entity.Discriminant as D
import qualified Entity.EnumCase as EC
import qualified Entity.EnumTypeName as ET
import qualified Entity.EnumValueName as EV
import qualified Entity.GlobalLocator as GL
import qualified Entity.GlobalName as GN
import Entity.Hint
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import qualified Entity.LamKind as LK
import qualified Entity.LocalLocator as LL
import Entity.NominalEnv
import qualified Entity.Pattern as PAT
import qualified Entity.Pattern.Fallback as PATF
import qualified Entity.Pattern.Specialize as PATS
import qualified Entity.RawPattern as RP
import qualified Entity.RawTerm as RT
import Entity.Stmt
import qualified Entity.UnresolvedName as UN
import qualified Entity.Vector as V
import qualified Entity.WeakPrim as WP
import qualified Entity.WeakPrimValue as WPV
import qualified Entity.WeakTerm as WT

class
  ( Throw.Context m,
    Gensym.Context m,
    Global.Context m,
    Locator.Context m,
    Alias.Context m,
    PATF.Context m,
    PATS.Context m
  ) =>
  Context m

discernStmtList :: Context m => [RawStmt] -> m [WeakStmt]
discernStmtList stmtList =
  case stmtList of
    [] ->
      return []
    RawStmtDefine stmtKind m functionName impArgNum xts codType e : rest -> do
      stmtKind' <- discernStmtKind stmtKind
      (xts', nenv) <- discernBinder empty xts
      codType' <- discern nenv codType
      e' <- discern nenv e
      rest' <- discernStmtList rest
      return $ WeakStmtDefine stmtKind' m functionName impArgNum xts' codType' e' : rest'
    RawStmtSection section innerStmtList : rest -> do
      Locator.withSection section $ do
        innerStmtList' <- discernStmtList innerStmtList
        rest' <- discernStmtList rest
        return $ innerStmtList' ++ rest'

discernStmtKind :: Context m => StmtKindF RT.RawTerm -> m (StmtKindF WT.WeakTerm)
discernStmtKind stmtKind =
  case stmtKind of
    Normal opacity ->
      return $ Normal opacity
    Data arity dataName consNameList ->
      return $ Data arity dataName consNameList
    DataIntro dataName dataArgs consArgs discriminant -> do
      (dataArgs', nenv) <- discernBinder empty dataArgs
      (consArgs', _) <- discernBinder nenv consArgs
      return $ DataIntro dataName dataArgs' consArgs' discriminant

-- Alpha-convert all the variables so that different variables have different names.
discern :: Context m => NominalEnv -> RT.RawTerm -> m WT.WeakTerm
discern nenv term =
  case term of
    m :< RT.Tau ->
      return $ m :< WT.Tau
    m :< RT.Var (I (s, _)) -> do
      case lookup s nenv of
        Just (_, name) ->
          return $ m :< WT.Var name
        Nothing -> do
          (dd, gn) <- resolveName m s
          interpretGlobalName m dd gn
    m :< RT.VarGlobal globalLocator localLocator -> do
      sgl <- Alias.resolveAlias m globalLocator
      let dd = DD.new sgl localLocator
      gn <- interpretDefiniteDescription m dd
      interpretGlobalName m dd gn
    m :< RT.Pi xts t -> do
      (xts', t') <- discernBinderWithBody nenv xts t
      return $ m :< WT.Pi xts' t'
    m :< RT.PiIntro kind xts e -> do
      case kind of
        LK.Fix xt -> do
          (xt', xts', e') <- discernBinderWithBody' nenv xt xts e
          return $ m :< WT.PiIntro (LK.Fix xt') xts' e'
        LK.Normal -> do
          (xts', e') <- discernBinderWithBody nenv xts e
          return $ m :< WT.PiIntro LK.Normal xts' e'
    m :< RT.Data name es -> do
      es' <- mapM (discern nenv) es
      return $ m :< WT.Data name es'
    m :< RT.DataIntro dataName consName disc dataArgs consArgs -> do
      dataArgs' <- mapM (discern nenv) dataArgs
      consArgs' <- mapM (discern nenv) consArgs
      return $ m :< WT.DataIntro dataName consName disc dataArgs' consArgs'
    m :< RT.DataElim es patternMatrix -> do
      os <- mapM (const $ Gensym.newIdentFromText "match") es -- os: occurrences
      es' <- mapM (discern nenv) es
      ts <- mapM (const $ Gensym.newAster m (asHoleArgs nenv)) es'
      decisionTree <- discernPatternMatrix nenv m patternMatrix >>= compilePatternMatrix nenv m (V.fromList os)
      return $ m :< WT.DataElim (zip3 os es' ts) decisionTree
    m :< RT.PiElim e es -> do
      es' <- mapM (discern nenv) es
      e' <- discern nenv e
      return $ m :< WT.PiElim e' es'
    m :< RT.Sigma xts -> do
      (xts', _) <- discernBinderWithBody nenv xts (m :< RT.Tau)
      return $ m :< WT.Sigma xts'
    m :< RT.SigmaIntro es -> do
      es' <- mapM (discern nenv) es
      return $ m :< WT.SigmaIntro es'
    m :< RT.SigmaElim xts e1 e2 -> do
      e1' <- discern nenv e1
      (xts', e2') <- discernBinderWithBody nenv xts e2
      return $ m :< WT.SigmaElim xts' e1' e2'
    m :< RT.Let mxt e1 e2 -> do
      e1' <- discern nenv e1
      (mxt', _, e2') <- discernBinderWithBody' nenv mxt [] e2
      return $ m :< WT.Let mxt' e1' e2'
    m :< RT.Prim prim -> do
      prim' <- mapM (discern nenv) prim
      return $ m :< WT.Prim prim'
    m :< RT.Aster k ->
      return $ m :< WT.Aster k (asHoleArgs nenv)
    m :< RT.Enum t ->
      return $ m :< WT.Enum t
    m :< RT.EnumIntro label -> do
      label' <- discernEnumLabel m label
      return $ m :< WT.EnumIntro label'
    m :< RT.EnumElim (e, t) caseList -> do
      e' <- discern nenv e
      t' <- discern nenv t
      caseList' <-
        forM caseList $ \(enumCase, body) -> do
          enumCase' <- discernEnumCase enumCase
          body' <- discern nenv body
          return (enumCase', body')
      return $ m :< WT.EnumElim (e', t') caseList'
    m :< RT.Question e t -> do
      e' <- discern nenv e
      t' <- discern nenv t
      return $ m :< WT.Question e' t'
    m :< RT.Magic der -> do
      der' <- traverse (discern nenv) der
      return $ m :< WT.Magic der'

discernBinder ::
  Context m =>
  NominalEnv ->
  [BinderF RT.RawTerm] ->
  m ([BinderF WT.WeakTerm], NominalEnv)
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
  NominalEnv ->
  [BinderF RT.RawTerm] ->
  RT.RawTerm ->
  m ([BinderF WT.WeakTerm], WT.WeakTerm)
discernBinderWithBody nenv binder e = do
  (binder', nenv') <- discernBinder nenv binder
  e' <- discern nenv' e
  return (binder', e')

discernBinderWithBody' ::
  Context m =>
  NominalEnv ->
  BinderF RT.RawTerm ->
  [BinderF RT.RawTerm] ->
  RT.RawTerm ->
  m (BinderF WT.WeakTerm, [BinderF WT.WeakTerm], WT.WeakTerm)
discernBinderWithBody' nenv (mx, x, t) binder e = do
  t' <- discern nenv t
  x' <- Gensym.newIdentFromIdent x
  (binder', e') <- discernBinderWithBody ((Ident.toText x, (mx, x')) : nenv) binder e
  return ((mx, x', t'), binder', e')

discernEnumLabel :: Context m => Hint -> EC.PreEnumLabel -> m EC.EnumLabel
discernEnumLabel m (EC.PreEnumLabel _ _ (UN.UnresolvedName name)) = do
  (dd, gn) <- resolveName m name
  term <- interpretGlobalName m dd gn
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

resolveName :: Context m => Hint -> T.Text -> m (DD.DefiniteDescription, GN.GlobalName)
resolveName m name = do
  localLocator <- LL.reflect m name
  candList <- Locator.getPossibleReferents localLocator
  candList' <- mapM Global.lookup candList
  let foundNameList = Maybe.mapMaybe candFilter $ zip candList candList'
  case foundNameList of
    [] ->
      Throw.raiseError m $ "undefined variable: " <> name
    [pair] ->
      return pair
    _ -> do
      let candInfo = T.concat $ map (("\n- " <>) . DD.reify . fst) foundNameList
      Throw.raiseError m $
        "this `" <> name <> "` is ambiguous since it could refer to:" <> candInfo

interpretGlobalName :: Context m => Hint -> DD.DefiniteDescription -> GN.GlobalName -> m WT.WeakTerm
interpretGlobalName m dd gn =
  case gn of
    GN.TopLevelFunc arity ->
      return $ m :< WT.VarGlobal dd arity
    GN.Data arity _ ->
      return $ m :< WT.VarGlobal dd arity
    GN.DataIntro dataArity consArity _ ->
      return $ m :< WT.VarGlobal dd (A.fromInt $ fromInteger (A.reify dataArity + A.reify consArity))
    GN.EnumType _ ->
      return $ m :< WT.Enum (ET.EnumTypeName dd)
    GN.EnumIntro enumTypeName discriminant ->
      return $ m :< WT.EnumIntro (EC.EnumLabel enumTypeName discriminant (EV.EnumValueName dd))
    GN.PrimType primNum ->
      return $ m :< WT.Prim (WP.Type primNum)
    GN.PrimOp primOp ->
      return $ m :< WT.Prim (WP.Value (WPV.Op primOp))

candFilter :: (a, Maybe b) -> Maybe (a, b)
candFilter (from, mTo) =
  fmap (from,) mTo

interpretDefiniteDescription :: Context m => Hint -> DD.DefiniteDescription -> m GN.GlobalName
interpretDefiniteDescription m dd = do
  mgn <- Global.lookup dd
  case mgn of
    Just gn ->
      return gn
    Nothing ->
      Throw.raiseError m $ "undefined definite description: " <> DD.reify dd

resolveConstructor ::
  Context m =>
  Hint ->
  Either UN.UnresolvedName (GL.GlobalLocator, LL.LocalLocator) ->
  m (DD.DefiniteDescription, A.Arity, A.Arity, D.Discriminant)
resolveConstructor m cons = do
  case cons of
    Left (UN.UnresolvedName consName') -> do
      (dd, gn) <- resolveName m consName'
      resolveConstructor' m dd gn
    Right (globalLocator, localLocator) -> do
      sgl <- Alias.resolveAlias m globalLocator
      let dd = DD.new sgl localLocator
      gn <- interpretDefiniteDescription m dd
      resolveConstructor' m dd gn

resolveConstructor' ::
  Context m =>
  Hint ->
  DD.DefiniteDescription ->
  GN.GlobalName ->
  m (DD.DefiniteDescription, A.Arity, A.Arity, D.Discriminant)
resolveConstructor' m dd gn =
  case gn of
    GN.DataIntro dataArity consArity disc ->
      return (dd, dataArity, consArity, disc)
    _ ->
      Throw.raiseError m $ DD.reify dd <> " is not a constructor"

discernPatternMatrix ::
  Context m =>
  NominalEnv ->
  Hint ->
  RP.RawPatternMatrix RT.RawTerm ->
  m (PAT.PatternMatrix ([Ident], WT.WeakTerm))
discernPatternMatrix nenv m patternMatrix =
  case RP.unconsRow patternMatrix of
    Nothing ->
      return $ PAT.new []
    Just (row, rows) -> do
      row' <- discernPatternRow nenv m row
      rows' <- discernPatternMatrix nenv m rows
      return $ PAT.consRow row' rows'

discernPatternRow ::
  Context m =>
  NominalEnv ->
  Hint ->
  RP.RawPatternRow RT.RawTerm ->
  m (PAT.PatternRow ([Ident], WT.WeakTerm))
discernPatternRow nenv m (patList, body) = do
  let vars = RP.rowVars patList
  vars' <- mapM (Gensym.newIdentFromIdent . snd) vars
  when (S.size (S.fromList vars') /= length vars') $ do
    Throw.raiseError m "found a non-linear pattern"
  let nenv' = zipWith (\(mv, var) var' -> (Ident.toText var, (mv, var'))) vars vars'
  patList' <- mapM (discernPattern nenv') patList
  body' <- discern (nenv' ++ nenv) body
  return (patList', ([], body'))

discernPattern ::
  Context m =>
  NominalEnv ->
  (Hint, RP.RawPattern) ->
  m (Hint, PAT.Pattern)
discernPattern nenv (m, pat) =
  case pat of
    RP.Var x ->
      case lookup (Ident.toText x) nenv of
        Nothing ->
          return (m, PAT.Var x)
        Just (_, x') ->
          return (m, PAT.Var x')
    RP.Cons cons args -> do
      (consName, dataArity, consArity, disc) <- resolveConstructor m cons
      args' <- mapM (discernPattern nenv) args
      return (m, PAT.Cons consName disc dataArity consArity args')

-- This translation is based on:
--   https://dl.acm.org/doi/10.1145/1411304.1411311
compilePatternMatrix ::
  Context m =>
  NominalEnv ->
  Hint ->
  V.Vector Ident ->
  PAT.PatternMatrix ([Ident], WT.WeakTerm) ->
  m (DT.DecisionTree WT.WeakTerm)
compilePatternMatrix nenv m occurrences mat =
  case PAT.unconsRow mat of
    Nothing ->
      return DT.Unreachable
    Just (row, _) ->
      case PAT.getClauseBody row of
        Right (usedVars, (freedVars, body)) -> do
          DT.Leaf freedVars <$> bindLet nenv m (zip usedVars (V.toList occurrences)) body
        Left i ->
          if i > 0
            then do
              occurrences' <- V.swap m i occurrences
              mat' <- PAT.swapColumn m i mat
              compilePatternMatrix nenv m occurrences' mat'
            else do
              let headConstructors = PAT.getHeadConstructors mat
              let cursor = V.head occurrences
              clauseList <- forM headConstructors $ \(cons, disc, dataArity, _, args) -> do
                let consArity = length args
                dataHoles <- mapM (const $ Gensym.newAster m (asHoleArgs nenv)) [1 .. A.reify dataArity]
                dataTypeHoles <- mapM (const $ Gensym.newAster m (asHoleArgs nenv)) [1 .. A.reify dataArity]
                consVars <- mapM (const $ Gensym.newIdentFromText "cvar") [1 .. consArity]
                (consArgs', nenv') <- alignConsArgs nenv $ map (m,) consVars
                let occurrences' = V.fromList consVars <> V.tail occurrences
                specialMatrix <- PATS.specialize nenv cursor (cons, A.fromInt consArity) mat
                specialDecisionTree <- compilePatternMatrix nenv' m occurrences' specialMatrix
                return (DT.Cons cons disc (zip dataHoles dataTypeHoles) consArgs' specialDecisionTree)
              fallbackMatrix <- PATF.getFallbackMatrix nenv cursor mat
              fallbackClause <- compilePatternMatrix nenv m (V.tail occurrences) fallbackMatrix
              t <- Gensym.newAster m (asHoleArgs nenv)
              return $ DT.Switch (cursor, t) (fallbackClause, clauseList)

alignConsArgs ::
  Context m =>
  NominalEnv ->
  [(Hint, Ident)] ->
  m ([BinderF WT.WeakTerm], NominalEnv)
alignConsArgs nenv binder =
  case binder of
    [] -> do
      return ([], nenv)
    (mx, x) : xts -> do
      t <- Gensym.newPreAster mx
      t' <- discern nenv t
      (xts', nenv') <- alignConsArgs ((Ident.toText x, (mx, x)) : nenv) xts
      return ((mx, x, t') : xts', nenv')

bindLet ::
  Context m =>
  NominalEnv ->
  Hint ->
  [(Maybe Ident, Ident)] ->
  WT.WeakTerm ->
  m WT.WeakTerm
bindLet nenv m binder cont =
  case binder of
    [] ->
      return cont
    (Nothing, _) : xes -> do
      bindLet nenv m xes cont
    (Just from, to) : xes -> do
      h <- Gensym.newAster m (asHoleArgs nenv)
      cont' <- bindLet nenv m xes cont
      return $ m :< WT.Let (m, from, h) (m :< WT.Var to) cont'
