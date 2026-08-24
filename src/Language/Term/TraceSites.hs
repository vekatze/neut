module Language.Term.TraceSites
  ( annotate,
    annotateRoot,
    traceIDOf,
    setTraceID,
    traceIDSet,
    Blocker (..),
    findBlocker,
    describeBlocker,
    describeTraceBlocker,
    sourceLevelName,
  )
where

import Control.Applicative ((<|>))
import Control.Comonad.Cofree
import Control.Monad (forM)
import Data.Bitraversable (bimapM)
import Data.Foldable (asum, foldl', toList)
import Data.IORef
import Data.IntSet qualified as IntSet
import Data.Text qualified as T
import Language.Common.BaseLowType qualified as BLT
import Language.Common.DecisionTree qualified as DT
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.ExternalName qualified as EN
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.Term.Term qualified as TM
import Language.Term.Trace qualified as Trace
import Language.Term.TraceID
import Logger.Hint

annotate :: Trace.Handle -> TM.Term -> IO (TM.Term, IntSet.IntSet)
annotate traceHandle term = do
  traceIDsRef <- newIORef IntSet.empty
  term' <- forTermM term $ \m node -> do
    node' <- annotateNode traceHandle m node
    let TraceID rawID = traceIDOf node'
    if rawID == 0
      then return ()
      else modifyIORef' traceIDsRef $ IntSet.insert rawID
    return node'
  traceIDs <- readIORef traceIDsRef
  return (term', traceIDs)

annotateRoot :: Trace.Handle -> TM.Term -> IO TM.Term
annotateRoot traceHandle term@(m :< node) = do
  if isTraceRoot node
    then do
      node' <- annotateNode traceHandle m node
      return $ m :< node'
    else return term

annotateNode :: Trace.Handle -> Hint -> TM.TermF TM.Term -> IO (TM.TermF TM.Term)
annotateNode traceHandle m node = do
  if traceIDOf node /= noTrace
    then return node
    else do
      if null $ metaFileName m
        then return node
        else do
          traceID <- Trace.registerSourceSite traceHandle m
          return $ setTraceID traceID node

traceIDOf :: TM.TermF a -> TraceID
traceIDOf node = do
  case node of
    TM.PiElim traceID _ _ _ _ _ ->
      traceID
    TM.DataElim traceID _ _ _ ->
      traceID
    TM.BoxIntro traceID _ _ ->
      traceID
    TM.BoxElim traceID _ _ _ _ _ ->
      traceID
    TM.CodeElim traceID _ ->
      traceID
    TM.TauElim traceID _ _ _ ->
      traceID
    TM.Magic traceID _ ->
      traceID
    _ ->
      noTrace

setTraceID :: TraceID -> TM.TermF a -> TM.TermF a
setTraceID traceID node = do
  case node of
    TM.PiElim _ kind callee impArgs expArgs defaultArgs ->
      TM.PiElim traceID kind callee impArgs expArgs defaultArgs
    TM.DataElim _ isNoetic oets tree ->
      TM.DataElim traceID isNoetic oets tree
    TM.BoxIntro _ letSeq body ->
      TM.BoxIntro traceID letSeq body
    TM.BoxElim _ castSeq binder value uncastSeq body ->
      TM.BoxElim traceID castSeq binder value uncastSeq body
    TM.CodeElim _ body ->
      TM.CodeElim traceID body
    TM.TauElim _ binder value body ->
      TM.TauElim traceID binder value body
    TM.Magic _ magic ->
      TM.Magic traceID magic
    _ ->
      node

traceIDSet :: TM.Term -> IntSet.IntSet
traceIDSet =
  collectTraceIDs IntSet.empty

collectTraceIDs :: IntSet.IntSet -> TM.Term -> IntSet.IntSet
collectTraceIDs traceIDs (_ :< node) = do
  let TraceID rawID = traceIDOf node
  let traceIDs' =
        if rawID == 0
          then traceIDs
          else IntSet.insert rawID traceIDs
  foldl' collectTraceIDs traceIDs' $ termChildren node

data Blocker = Blocker
  { blockerTraceID :: TraceID,
    blockerKind :: BlockerKind
  }

data BlockerKind
  = OpaqueApplication DD.DefiniteDescription [TM.Term]
  | StuckApplication
  | StuckMatch
  | BoxComputation
  | BoxElimination
  | StageElimination
  | TypeElimination
  | MagicOperation T.Text T.Text

findBlocker :: TM.Term -> Maybe Blocker
findBlocker (_ :< node) =
  case node of
    TM.Let _ rhs body ->
      findBlocker rhs <|> findBlocker body
    TM.DataIntro _ _ _ args ->
      firstBlocker args
    TM.BoxIntroLift _ child ->
      findBlocker child
    TM.PiElim traceID _ callee _ args defaults ->
      firstBlocker (callee : args ++ foldMap toList defaults)
        <|> Just (Blocker traceID (applicationKind callee args))
    TM.DataElim traceID _ oets _ ->
      firstBlocker (map (\(_, e, _) -> e) oets)
        <|> Just (Blocker traceID StuckMatch)
    TM.CodeElim traceID child ->
      findBlocker child <|> Just (Blocker traceID StageElimination)
    TM.TauElim traceID _ child _ ->
      findBlocker child <|> Just (Blocker traceID TypeElimination)
    TM.Magic traceID magic ->
      if isValueMagic magic
        then Nothing
        else
          firstBlocker (toList magic)
            <|> Just
              ( Blocker
                  traceID
                  (MagicOperation (describeMagicCall magic) (describeMagicBlocker magic))
              )
    TM.BoxIntro traceID _ _ ->
      Just (Blocker traceID BoxComputation)
    TM.BoxElim traceID _ _ _ _ _ ->
      Just (Blocker traceID BoxElimination)
    TM.Var {} ->
      Nothing
    TM.VarGlobal {} ->
      Nothing
    TM.PiIntro {} ->
      Nothing
    TM.CodeIntro {} ->
      Nothing
    TM.TauIntro {} ->
      Nothing
    TM.Prim {} ->
      Nothing
    TM.Invoke {} ->
      Nothing

firstBlocker :: [TM.Term] -> Maybe Blocker
firstBlocker =
  asum . map findBlocker

applicationKind :: TM.Term -> [TM.Term] -> BlockerKind
applicationKind callee args =
  case callee of
    _ :< TM.VarGlobal _ dd ->
      OpaqueApplication dd args
    _ ->
      StuckApplication

describeBlocker :: Blocker -> T.Text
describeBlocker blocker =
  case blockerKind blocker of
    OpaqueApplication callee args ->
      "an opaque function application `" <> sourceLevelName callee <> renderArgList args <> "`"
    StuckApplication ->
      "an opaque application"
    StuckMatch ->
      "a pattern matching"
    BoxComputation ->
      "a layer introduction"
    BoxElimination ->
      "a layer elimination"
    StageElimination ->
      "a stage elimination"
    TypeElimination ->
      "a type elimination"
    MagicOperation description _ ->
      "`" <> description <> "`"

describeTraceBlocker :: Blocker -> T.Text
describeTraceBlocker blocker =
  case blockerKind blocker of
    OpaqueApplication _ _ ->
      "Found an opaque function application"
    StuckApplication ->
      "Found an application"
    StuckMatch ->
      "Found a pattern matching"
    BoxComputation ->
      "Found a layer introduction"
    BoxElimination ->
      "Found a layer elimination"
    StageElimination ->
      "Found a stage elimination"
    TypeElimination ->
      "Found a type elimination"
    MagicOperation _ description ->
      "Found " <> description

renderArgList :: [TM.Term] -> T.Text
renderArgList args =
  if null args
    then "()"
    else "(..)"

describeMagicCall :: M.Magic BLT.BaseLowType TM.Type TM.Term -> T.Text
describeMagicCall magic =
  case magic of
    M.LowMagic (LM.External _ _ name args _) ->
      "magic external " <> EN.reify name <> renderArgList args
    _ ->
      T.pack (describeMagic magic)

describeMagicBlocker :: M.Magic BLT.BaseLowType TM.Type TM.Term -> T.Text
describeMagicBlocker magic =
  case magic of
    M.LowMagic (LM.External {}) ->
      "a runtime-only external function"
    _ ->
      "a runtime-only operation"

sourceLevelName :: DD.DefiniteDescription -> T.Text
sourceLevelName callee = do
  let (name, _) = T.breakOn "#" $ DD.localLocator callee
  name

describeMagic :: M.Magic lt ty a -> String
describeMagic magic = do
  case magic of
    M.LowMagic lowMagic ->
      case lowMagic of
        LM.Cast {} ->
          "magic cast"
        LM.Store {} ->
          "magic store"
        LM.Load {} ->
          "magic load"
        LM.Alloca {} ->
          "magic alloca"
        LM.External _ _ name _ _ ->
          "magic external " <> T.unpack (EN.reify name)
        LM.Global name _ ->
          "magic global " <> T.unpack (EN.reify name)
        LM.OpaqueValue {} ->
          "magic opaque-value"
        LM.CallType {} ->
          "magic call-type"
    M.Calloc {} ->
      "magic calloc"
    M.Malloc {} ->
      "magic malloc"
    M.Realloc {} ->
      "magic realloc"
    M.Free {} ->
      "magic free"
    M.InspectType {} ->
      "magic inspect-type"
    M.EqType {} ->
      "magic eq-type"
    M.ShowType {} ->
      "magic show-type"
    M.TextCons {} ->
      "magic text-cons"
    M.TextUncons {} ->
      "magic text-uncons"
    M.MakeSwitch {} ->
      "magic make-switch"
    M.CompileError {} ->
      "magic compile-error"
    M.GetOriginFileName ->
      "magic get-origin-file-name"
    M.GetOriginLine ->
      "magic get-origin-line"
    M.GetOriginColumn ->
      "magic get-origin-column"

isTraceRoot :: TM.TermF a -> Bool
isTraceRoot node = do
  case node of
    TM.PiElim {} ->
      True
    TM.DataElim {} ->
      True
    TM.BoxIntro {} ->
      True
    TM.BoxElim {} ->
      True
    TM.CodeElim {} ->
      True
    TM.TauElim {} ->
      True
    TM.Magic _ magic ->
      not (isValueMagic magic)
    TM.Var {} ->
      False
    TM.VarGlobal {} ->
      False
    TM.PiIntro {} ->
      False
    TM.DataIntro {} ->
      False
    TM.BoxIntroLift {} ->
      False
    TM.CodeIntro {} ->
      False
    TM.TauIntro {} ->
      False
    TM.Let {} ->
      False
    TM.Invoke {} ->
      False
    TM.Prim {} ->
      False

isValueMagic :: M.Magic lt ty a -> Bool
isValueMagic magic =
  case magic of
    M.LowMagic (LM.OpaqueValue _) ->
      True
    _ ->
      False

forTermM :: TM.Term -> (Hint -> TM.TermF TM.Term -> IO (TM.TermF TM.Term)) -> IO TM.Term
forTermM (m :< node) transform = do
  node' <- forNodeM node $ \child -> do
    forTermM child transform
  node'' <- transform m node'
  return $ m :< node''

forNodeM :: TM.TermF TM.Term -> (TM.Term -> IO TM.Term) -> IO (TM.TermF TM.Term)
forNodeM node recur = do
  case node of
    TM.Var x ->
      return $ TM.Var x
    TM.VarGlobal attr dd ->
      return $ TM.VarGlobal attr dd
    TM.PiIntro attr impArgs expArgs defaultArgs body -> do
      defaultArgs' <- forM defaultArgs (bimapM return recur)
      body' <- recur body
      return $ TM.PiIntro attr impArgs expArgs defaultArgs' body'
    TM.PiElim traceID kind callee impArgs expArgs defaultArgs -> do
      callee' <- recur callee
      expArgs' <- forM expArgs recur
      defaultArgs' <- forM defaultArgs (traverse recur)
      return $ TM.PiElim traceID kind callee' impArgs expArgs' defaultArgs'
    TM.DataIntro attr dd dataArgs consArgs -> do
      consArgs' <- forM consArgs recur
      return $ TM.DataIntro attr dd dataArgs consArgs'
    TM.DataElim traceID isNoetic oets tree -> do
      oets' <- forM oets $ \(x, e, t) -> do
        e' <- recur e
        return (x, e', t)
      tree' <- forDecisionTreeM tree recur
      return $ TM.DataElim traceID isNoetic oets' tree'
    TM.BoxIntro traceID letSeq body -> do
      letSeq' <- forM letSeq (bimapM return recur)
      body' <- recur body
      return $ TM.BoxIntro traceID letSeq' body'
    TM.BoxIntroLift ty child -> do
      child' <- recur child
      return $ TM.BoxIntroLift ty child'
    TM.BoxElim traceID castSeq binder value uncastSeq body -> do
      castSeq' <- forM castSeq (bimapM return recur)
      value' <- recur value
      uncastSeq' <- forM uncastSeq (bimapM return recur)
      body' <- recur body
      return $ TM.BoxElim traceID castSeq' binder value' uncastSeq' body'
    TM.CodeIntro body ->
      TM.CodeIntro <$> recur body
    TM.CodeElim traceID body ->
      TM.CodeElim traceID <$> recur body
    TM.TauIntro ty ->
      return $ TM.TauIntro ty
    TM.TauElim traceID binder value body ->
      TM.TauElim traceID binder <$> recur value <*> recur body
    TM.Let binder value body ->
      TM.Let binder <$> recur value <*> recur body
    TM.Invoke names body ->
      TM.Invoke names <$> recur body
    TM.Prim prim ->
      return $ TM.Prim prim
    TM.Magic traceID magic ->
      TM.Magic traceID <$> traverse recur magic

forDecisionTreeM :: DT.DecisionTree TM.Type TM.Term -> (TM.Term -> IO TM.Term) -> IO (DT.DecisionTree TM.Type TM.Term)
forDecisionTreeM tree recur = do
  case tree of
    DT.Leaf xs letSeq body -> do
      letSeq' <- forM letSeq (bimapM return recur)
      body' <- recur body
      return $ DT.Leaf xs letSeq' body'
    DT.Unreachable ->
      return DT.Unreachable
    DT.Switch cursor (fallback, cases) -> do
      fallback' <- forDecisionTreeM fallback recur
      cases' <- forM cases $ \decisionCase ->
        forCaseM decisionCase recur
      return $ DT.Switch cursor (fallback', cases')

forCaseM :: DT.Case TM.Type TM.Term -> (TM.Term -> IO TM.Term) -> IO (DT.Case TM.Type TM.Term)
forCaseM decisionCase recur = do
  case decisionCase of
    DT.LiteralCase m literal cont ->
      DT.LiteralCase m literal <$> forDecisionTreeM cont recur
    DT.ConsCase record -> do
      cont' <- forDecisionTreeM (DT.cont record) recur
      return $ DT.ConsCase record {DT.cont = cont'}

termChildren :: TM.TermF TM.Term -> [TM.Term]
termChildren node = do
  case node of
    TM.Var {} ->
      []
    TM.VarGlobal {} ->
      []
    TM.PiIntro _ _ _ defaultArgs body ->
      map snd defaultArgs ++ [body]
    TM.PiElim _ _ callee _ args defaults ->
      callee : args ++ foldMap toList defaults
    TM.DataIntro _ _ _ args ->
      args
    TM.DataElim _ _ oets tree ->
      map (\(_, e, _) -> e) oets ++ decisionTreeTerms tree
    TM.BoxIntro _ letSeq body ->
      map snd letSeq ++ [body]
    TM.BoxIntroLift _ child ->
      [child]
    TM.BoxElim _ castSeq _ value uncastSeq body ->
      map snd castSeq ++ [value] ++ map snd uncastSeq ++ [body]
    TM.CodeIntro body ->
      [body]
    TM.CodeElim _ body ->
      [body]
    TM.TauIntro _ ->
      []
    TM.TauElim _ _ value body ->
      [value, body]
    TM.Let _ value body ->
      [value, body]
    TM.Invoke _ body ->
      [body]
    TM.Prim _ ->
      []
    TM.Magic _ magic ->
      toList magic

decisionTreeTerms :: DT.DecisionTree t TM.Term -> [TM.Term]
decisionTreeTerms tree = do
  case tree of
    DT.Leaf _ letSeq body ->
      map snd letSeq ++ [body]
    DT.Unreachable ->
      []
    DT.Switch _ (fallback, cases) ->
      decisionTreeTerms fallback ++ foldMap caseTerms cases

caseTerms :: DT.Case t TM.Term -> [TM.Term]
caseTerms decisionCase = do
  case decisionCase of
    DT.LiteralCase _ _ cont ->
      decisionTreeTerms cont
    DT.ConsCase record ->
      decisionTreeTerms $ DT.cont record
