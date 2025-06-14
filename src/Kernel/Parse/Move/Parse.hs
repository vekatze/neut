module Kernel.Parse.Move.Parse
  ( Handle,
    parse,
  )
where

import CodeParser.Move.Parse (runParser)
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (Bifunctor (first))
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Error.Move.Run (forP, forP_)
import Error.Rule.EIO (EIO)
import Kernel.Common.Move.CreateGlobalHandle qualified as Global
import Kernel.Common.Move.CreateLocalHandle qualified as Local
import Kernel.Common.Move.Handle.Global.KeyArg qualified as KeyArg
import Kernel.Common.Move.Handle.Global.OptimizableData qualified as OptimizableData
import Kernel.Common.Move.Handle.Local.Locator qualified as Locator
import Kernel.Common.Rule.Cache (Cache)
import Kernel.Common.Rule.Cache qualified as Cache
import Kernel.Common.Rule.GlobalName qualified as GN
import Kernel.Common.Rule.Handle.Global.KeyArg qualified as KeyArg
import Kernel.Common.Rule.Handle.Global.OptimizableData qualified as OptimizableData
import Kernel.Common.Rule.Handle.Local.Locator qualified as Locator
import Kernel.Common.Rule.OptimizableData qualified as OD
import Kernel.Common.Rule.Source (Source)
import Kernel.Common.Rule.Source qualified as Source
import Kernel.Parse.Move.Internal.Discern.Data (defineData)
import Kernel.Parse.Move.Internal.Handle.GlobalNameMap qualified as GlobalNameMap
import Kernel.Parse.Move.Internal.Handle.NameMap qualified as NameMap
import Kernel.Parse.Move.Internal.Program qualified as Parse
import Kernel.Parse.Move.Internal.RawTerm qualified as ParseRT
import Language.Common.Rule.ArgNum qualified as AN
import Language.Common.Rule.BaseName qualified as BN
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.Ident.Reify
import Language.Common.Rule.StmtKind qualified as SK
import Language.RawTerm.Rule.RawStmt
import Language.RawTerm.Rule.RawTerm qualified as RT
import Language.Term.Rule.Stmt
import Logger.Rule.Hint
import SyntaxTree.Rule.Series qualified as SE

data Handle = Handle
  { parseHandle :: ParseRT.Handle,
    globalNameMapHandle :: GlobalNameMap.Handle,
    keyArgHandle :: KeyArg.Handle,
    optDataHandle :: OptimizableData.Handle
  }

new ::
  Global.Handle ->
  Handle
new globalHandle = do
  let parseHandle = ParseRT.new (Global.gensymHandle globalHandle)
  let globalNameMapHandle = Global.globalNameMapHandle globalHandle
  let keyArgHandle = Global.keyArgHandle globalHandle
  let optDataHandle = Global.optDataHandle globalHandle
  Handle {..}

parse ::
  Global.Handle ->
  [(Source, Either Cache T.Text)] ->
  EIO [(Local.Handle, (Source, Either Cache PostRawProgram))]
parse h contentSeq = do
  let parseHandle = new h
  cacheOrProgList <- forP contentSeq $ \(source, cacheOrContent) -> do
    prog <- parse' parseHandle source cacheOrContent
    localHandle <- Local.new h source
    let locatorHandle = Local.locatorHandle localHandle
    return (localHandle, (source, fmap (postprocess locatorHandle) prog))
  forP_ cacheOrProgList $ \(_, (source, cacheOrProg)) -> do
    registerTopLevelNames parseHandle source cacheOrProg
  return cacheOrProgList

parse' ::
  Handle ->
  Source.Source ->
  Either Cache.Cache T.Text ->
  EIO (Either Cache.Cache RawProgram)
parse' h source cacheOrContent = do
  parseSource h source cacheOrContent

parseSource ::
  Handle ->
  Source.Source ->
  Either Cache.Cache T.Text ->
  EIO (Either Cache.Cache RawProgram)
parseSource h source cacheOrContent = do
  let filePath = Source.sourceFilePath source
  case cacheOrContent of
    Left cache -> do
      return $ Left cache
    Right fileContent -> do
      prog <- runParser filePath fileContent True (Parse.parseProgram (parseHandle h))
      return $ Right $ snd prog

postprocess :: Locator.Handle -> RawProgram -> PostRawProgram
postprocess h (RawProgram m importList stmtList) = do
  let stmtList' = concatMap (postprocess' h . fst) stmtList
  PostRawProgram m importList stmtList'

postprocess' :: Locator.Handle -> RawStmt -> [PostRawStmt]
postprocess' h stmt = do
  case stmt of
    RawStmtDefine c stmtKind rawDef@(RT.RawDef {geist}) -> do
      let geist' = liftGeist h geist
      let stmtKind' = liftStmtKind h stmtKind
      [PostRawStmtDefine c stmtKind' (rawDef {RT.geist = geist'})]
    RawStmtDefineData _ m (name, _) args consInfo loc -> do
      let name' = Locator.attachCurrentLocator h name
      let consInfo' = fmap (liftRawCons h) consInfo
      defineData m name' args (SE.extract consInfo') loc
    RawStmtDefineResource c m (name, c1) (c2, discarder) (c3, copier) c4 -> do
      let name' = Locator.attachCurrentLocator h name
      [PostRawStmtDefineResource c m (name', c1) (c2, discarder) (c3, copier) c4]
    RawStmtNominal c m geistList -> do
      let geistList' = fmap (first (liftGeist h)) geistList
      [PostRawStmtNominal c m geistList']
    RawStmtForeign m foreignList -> do
      [PostRawStmtForeign m foreignList]

liftGeist :: Locator.Handle -> RT.RawGeist BN.BaseName -> RT.RawGeist DD.DefiniteDescription
liftGeist h geist = do
  fmap (Locator.attachCurrentLocator h) geist

liftStmtKind :: Locator.Handle -> RawStmtKind BN.BaseName -> RawStmtKind DD.DefiniteDescription
liftStmtKind h stmtKind = do
  case stmtKind of
    SK.Normal o ->
      SK.Normal o
    SK.Main o t ->
      SK.Main o t
    SK.Data name dataArgs consInfo -> do
      let name' = Locator.attachCurrentLocator h name
      let f (m, consName, isConstLike, consArgs, loc) = do
            let consName' = Locator.attachCurrentLocator h consName
            (m, consName', isConstLike, consArgs, loc)
      SK.Data name' dataArgs (fmap f consInfo)
    SK.DataIntro name dataArgs consArgs discriminant -> do
      let name' = Locator.attachCurrentLocator h name
      SK.DataIntro name' dataArgs consArgs discriminant

liftRawCons :: Locator.Handle -> RawConsInfo BN.BaseName -> RawConsInfo DD.DefiniteDescription
liftRawCons h (m, name, isConstLike, consArgs, loc) = do
  let name' = Locator.attachCurrentLocator h name
  (m, name', isConstLike, consArgs, loc)

registerTopLevelNames ::
  Handle ->
  Source.Source ->
  Either Cache.Cache PostRawProgram ->
  EIO ()
registerTopLevelNames h source cacheOrContent = do
  case cacheOrContent of
    Left cache -> do
      let stmtList = Cache.stmtList cache
      let nameArrowList = NameMap.getGlobalNames' stmtList
      liftIO $ saveTopLevelNames h source nameArrowList
      forM_ stmtList $ registerKeyArg' h
    Right (PostRawProgram _ _ stmtList) -> do
      let nameArrowList = NameMap.getGlobalNames stmtList
      liftIO $ saveTopLevelNames h source nameArrowList
      forM_ stmtList $ registerKeyArg h

saveTopLevelNames :: Handle -> Source.Source -> [(DD.DefiniteDescription, (Hint, GN.GlobalName))] -> IO ()
saveTopLevelNames h source nameArrowList = do
  let nameMap = Map.fromList nameArrowList
  GlobalNameMap.insert (globalNameMapHandle h) (Source.sourceFilePath source) nameMap
  registerOptDataInfo h nameArrowList

registerKeyArg :: Handle -> PostRawStmt -> EIO ()
registerKeyArg h stmt = do
  case stmt of
    PostRawStmtDefine _ _ (RT.RawDef {geist}) -> do
      let name = fst $ RT.name geist
      let impArgs = RT.extractArgs $ RT.impArgs geist
      let expArgs = RT.extractArgs $ RT.expArgs geist
      let isConstLike = RT.isConstLike geist
      let m = RT.loc geist
      let impKeys = map (\(_, x, _, _, _) -> x) impArgs
      let expKeys = map (\(_, x, _, _, _) -> x) expArgs
      KeyArg.insert (keyArgHandle h) m name isConstLike impKeys expKeys
    PostRawStmtNominal {} -> do
      return ()
    PostRawStmtDefineResource {} -> do
      return ()
    PostRawStmtForeign {} ->
      return ()

registerKeyArg' :: Handle -> Stmt -> EIO ()
registerKeyArg' h stmt = do
  case stmt of
    StmtDefine isConstLike _ (SavedHint m) name impArgs expArgs _ _ -> do
      let impKeys = map (\((_, x, _), _) -> toText x) impArgs
      let expKeys = map (\(_, x, _) -> toText x) expArgs
      KeyArg.insert (keyArgHandle h) m name isConstLike impKeys expKeys
    StmtForeign {} ->
      return ()

registerOptDataInfo :: Handle -> [(DD.DefiniteDescription, (Hint, GN.GlobalName))] -> IO ()
registerOptDataInfo h nameArrowList = do
  forM_ nameArrowList $ \(dd, (_, gn)) -> do
    case gn of
      GN.Data dataArgNum consNameArrowList _ -> do
        registerAsUnaryIfNecessary h dd consNameArrowList
        registerAsEnumIfNecessary h dd dataArgNum consNameArrowList
      _ ->
        return ()

registerAsUnaryIfNecessary ::
  Handle ->
  DD.DefiniteDescription ->
  [(DD.DefiniteDescription, (Hint, GN.GlobalName))] ->
  IO ()
registerAsUnaryIfNecessary h dataName consInfoList = do
  when (isUnary consInfoList) $ do
    OptimizableData.insert (optDataHandle h) dataName OD.Unary
    forM_ consInfoList $ \(consName, _) -> do
      OptimizableData.insert (optDataHandle h) consName OD.Unary

registerAsEnumIfNecessary ::
  Handle ->
  DD.DefiniteDescription ->
  AN.ArgNum ->
  [(DD.DefiniteDescription, (Hint, GN.GlobalName))] ->
  IO ()
registerAsEnumIfNecessary h dataName dataArgNum consInfoList =
  when (hasNoArgs dataArgNum consInfoList) $ do
    OptimizableData.insert (optDataHandle h) dataName OD.Enum
    forM_ consInfoList $ \(consName, _) -> do
      OptimizableData.insert (optDataHandle h) consName OD.Enum

isUnary :: [(DD.DefiniteDescription, (Hint, GN.GlobalName))] -> Bool
isUnary consInfoList =
  case consInfoList of
    [(_, (_, GN.DataIntro _ consArgNum _ _))] ->
      consArgNum == AN.fromInt 1
    _ ->
      False

hasNoArgs :: AN.ArgNum -> [(DD.DefiniteDescription, (Hint, GN.GlobalName))] -> Bool
hasNoArgs dataArgNum consInfoList = do
  let b1 = dataArgNum == AN.fromInt 0
  let b2 = all (\(_, (_, gn)) -> GN.hasNoArgs gn) consInfoList
  b1 && b2
