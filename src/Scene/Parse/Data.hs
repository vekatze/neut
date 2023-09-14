module Scene.Parse.Data (interpretDataTree) where

import Context.App
import Context.Locator qualified as Locator
import Context.Throw (liftEither)
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Data.HashMap.Strict qualified as Map
import Data.Maybe
import Data.Text qualified as T
import Entity.ArgNum qualified as AN
import Entity.Atom qualified as AT
import Entity.BaseName (fromTextOptional)
import Entity.BaseName qualified as BN
import Entity.Const (holeVarPrefix)
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.Error
import Entity.Hint
import Entity.IsConstLike
import Entity.Name
import Entity.Name qualified as Name
import Entity.RawBinder
import Entity.RawIdent
import Entity.RawTerm qualified as RT
import Entity.Stmt
import Entity.StmtKind qualified as SK
import Entity.Tree
import Scene.Parse.RawTerm

type ClauseInfo =
  (Hint, BN.BaseName, IsConstLike, [(RawBinder RT.RawTerm, Maybe Name)])

interpretDataTree :: Tree -> App [RawStmt]
interpretDataTree t = do
  ax <- newAxis
  case t of
    m :< Node ((_ :< Atom (AT.Symbol "enum")) : name : clauseList) -> do
      (_, name') <- liftEither $ getSymbol name >>= fromTextOptional
      clauseInfoList <- liftEither $ mapM (interpretClause ax) clauseList
      nameLL <- Locator.attachCurrentLocator name'
      defineData m nameLL Nothing clauseInfoList
    m :< Node (stmtName : name : rest) -> do
      liftEither $ chunk "data" stmtName
      (_, name') <- liftEither $ getSymbol name >>= fromTextOptional
      (argList, clauseList) <- liftEither $ reflArrowArgs'' m rest
      argList' <- liftEither $ reflArgList ax argList
      clauseInfoList <- liftEither $ mapM (interpretClause ax) clauseList
      nameLL <- Locator.attachCurrentLocator name'
      defineData m nameLL (Just argList') clauseInfoList
    m :< _ ->
      Throw.raiseError m "data-interp"

interpretClause :: Axis -> Tree -> EE ClauseInfo
interpretClause ax t =
  case t of
    m :< Atom {} -> do
      (_, consName') <- getSymbol t >>= fromTextOptional
      return (m, consName', True, [])
    m :< Node ts -> do
      (consName, consArgs) <- treeUncons m ts
      (_, consName') <- getSymbol consName >>= fromTextOptional
      clauses <- getClauseList m consArgs
      consArgs'' <- mapM (interpretDataClauseArg ax m) clauses
      return (m, consName', False, consArgs'')
    m :< _ ->
      Left $ newError m "interpretClause"

getClauseList :: Hint -> [Tree] -> EE [[Tree]]
getClauseList m ts =
  case ts of
    (_ :< Atom (AT.Symbol "of")) : rest -> do
      clauses <- mapM toNode rest
      return $ map snd clauses
    _ ->
      return $ map (\arg -> [m :< Atom (AT.Symbol holeVarPrefix), arg]) ts

interpretDataClauseArg :: Axis -> Hint -> [Tree] -> Either Error (RawBinder RT.RawTerm, Maybe Name)
interpretDataClauseArg ax m argInfo = do
  let (argInfo', attrs) = splitAttrs argInfo
  case argInfo' of
    [argName, argType] -> do
      (mArg, argName') <- getSymbol argName
      argType' <- reflRawTerm ax argType
      let viaInfo = getViaInfo attrs
      return ((mArg, argName', argType'), viaInfo)
    _ ->
      Left $ newError m "interpretDataClauseArgList"

getViaInfo :: Map.HashMap T.Text Tree -> Maybe Name
getViaInfo attrs = do
  val <- Map.lookup "via" attrs
  case val of
    m :< Atom (AT.Symbol symbol) ->
      return $ Name.fromText m symbol
    _ ->
      Nothing

defineData ::
  Hint ->
  DD.DefiniteDescription ->
  Maybe [RawBinder RT.RawTerm] ->
  [ClauseInfo] ->
  App [RawStmt]
defineData m dataName dataArgsOrNone consInfoList = do
  let dataArgs = fromMaybe [] dataArgsOrNone
  consInfoList' <- mapM modifyConstructorName consInfoList
  let consInfoList'' = modifyConsInfo D.zero consInfoList'
  let stmtKind = SK.Data dataName dataArgs consInfoList''
  let consNameList = map (\(_, consName, _, _, _) -> consName) consInfoList''
  let dataType = constructDataType m dataName consNameList dataArgs
  let isConstLike = isNothing dataArgsOrNone
  let formRule = RawStmtDefine isConstLike stmtKind m dataName (AN.fromInt 0) dataArgs (m :< RT.Tau) dataType
  introRuleList <- parseDefineDataConstructor dataType dataName dataArgs consInfoList' D.zero
  return $ formRule : introRuleList

modifyConsInfo ::
  D.Discriminant ->
  [(Hint, DD.DefiniteDescription, b, [(RawBinder RT.RawTerm, Maybe Name)])] ->
  [(Hint, DD.DefiniteDescription, b, [RawBinder RT.RawTerm], D.Discriminant)]
modifyConsInfo d consInfoList =
  case consInfoList of
    [] ->
      []
    (m, consName, isConstLike, consArgs) : rest ->
      (m, consName, isConstLike, map fst consArgs, d) : modifyConsInfo (D.increment d) rest

modifyConstructorName ::
  (Hint, BN.BaseName, IsConstLike, [(RawBinder RT.RawTerm, Maybe Name)]) ->
  App (Hint, DD.DefiniteDescription, IsConstLike, [(RawBinder RT.RawTerm, Maybe Name)])
modifyConstructorName (mb, consName, isConstLike, yts) = do
  consName' <- Locator.attachCurrentLocator consName
  return (mb, consName', isConstLike, yts)

parseDefineDataConstructor ::
  RT.RawTerm ->
  DD.DefiniteDescription ->
  [RawBinder RT.RawTerm] ->
  [(Hint, DD.DefiniteDescription, IsConstLike, [(RawBinder RT.RawTerm, Maybe Name)])] ->
  D.Discriminant ->
  App [RawStmt]
parseDefineDataConstructor dataType dataName dataArgs consInfoList discriminant = do
  case consInfoList of
    [] ->
      return []
    (m, consName, isConstLike, consArgs) : rest -> do
      let dataArgs' = map identPlusToVar dataArgs
      let consArgs' = map adjustConsArg consArgs
      let consNameList = map (\(_, c, _, _) -> c) consInfoList
      let args = dataArgs ++ map fst consArgs
      let introRule =
            RawStmtDefine
              isConstLike
              (SK.DataIntro consName dataArgs (map fst consArgs) discriminant)
              m
              consName
              (AN.fromInt $ length dataArgs)
              args
              dataType
              $ m :< RT.DataIntro dataName consName consNameList discriminant dataArgs' (map fst consArgs')
      let viaRule = RawStmtVia m consName (map snd consArgs')
      introRuleList <- parseDefineDataConstructor dataType dataName dataArgs rest (D.increment discriminant)
      return $ introRule : viaRule : introRuleList

constructDataType ::
  Hint ->
  DD.DefiniteDescription ->
  [DD.DefiniteDescription] ->
  [RawBinder RT.RawTerm] ->
  RT.RawTerm
constructDataType m dataName consNameList dataArgs = do
  m :< RT.Data dataName consNameList (map identPlusToVar dataArgs)

identPlusToVar :: RawBinder RT.RawTerm -> RT.RawTerm
identPlusToVar (m, x, _) =
  m :< RT.Var (Var x)

adjustConsArg :: (RawBinder RT.RawTerm, Maybe Name) -> (RT.RawTerm, (RawIdent, Maybe Name))
adjustConsArg ((m, x, _), mName) =
  (m :< RT.Var (Var x), (x, mName))
