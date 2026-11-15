module Kernel.Parse.Parse
  ( Handle,
    SourceUnit,
    getSourcePath,
    parse,
  )
where

import App.App (App)
import App.Run (forP, forP_)
import CodeParser.Parser (runParser)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Gensym.CreateHandle qualified as Gensym
import Gensym.Handle qualified as Gensym
import Kernel.Common.Cache (Cache)
import Kernel.Common.Cache qualified as Cache
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.CreateLocalHandle qualified as Local
import Kernel.Common.GlobalName qualified as GN
import Kernel.Common.Handle.Global.OptimizableData qualified as OptimizableData
import Kernel.Common.Handle.Global.ParamKey (Entry (..))
import Kernel.Common.Handle.Global.ParamKey qualified as ParamKey
import Kernel.Common.Handle.Local.Locator qualified as Locator
import Kernel.Common.OptimizableData qualified as OD
import Kernel.Common.Source (Source)
import Kernel.Common.Source qualified as Source
import Kernel.Parse.Internal.Discern.Data (defineData)
import Kernel.Parse.Internal.Discern.Variadic (defineVariadic)
import Kernel.Parse.Internal.Handle.GlobalNameMap qualified as GlobalNameMap
import Kernel.Parse.Internal.Handle.NameMap qualified as NameMap
import Kernel.Parse.Internal.Handle.UnusedTopLevelName qualified as UnusedTopLevelName
import Kernel.Parse.Internal.Program qualified as Parse
import Kernel.Parse.Internal.RawTerm qualified as ParseRT
import Language.Common.Arity qualified as Arity
import Language.Common.BaseName qualified as BN
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Ident.Reify
import Language.Common.NominalTag qualified as NT
import Language.Common.StmtKind qualified as SK
import Language.RawTerm.RawStmt
import Language.RawTerm.RawTerm qualified as RT
import Language.Term.Stmt
import Logger.Debug qualified as Logger
import Logger.Hint
import Path
import SyntaxTree.Series qualified as SE

data Handle = Handle
  { parseHandle :: ParseRT.Handle,
    globalNameMapHandle :: GlobalNameMap.Handle,
    unusedTopLevelNameHandle :: UnusedTopLevelName.Handle,
    paramKeyHandle :: ParamKey.Handle,
    optDataHandle :: OptimizableData.Handle
  }

new ::
  Gensym.Handle ->
  Global.Handle ->
  Handle
new gensymHandle globalHandle = do
  let parseHandle = ParseRT.new gensymHandle
  let globalNameMapHandle = Global.globalNameMapHandle globalHandle
  let unusedTopLevelNameHandle = Global.unusedTopLevelNameHandle globalHandle
  let paramKeyHandle = Global.paramKeyHandle globalHandle
  let optDataHandle = Global.optDataHandle globalHandle
  Handle {..}

type SourceUnit a =
  (Gensym.Handle, Local.Handle, (Source, a))

getSourcePath :: SourceUnit a -> Path Abs File
getSourcePath (_, _, (source, _)) =
  Source.sourceFilePath source

parse ::
  Global.Handle ->
  [(Source, Either Cache T.Text)] ->
  App [SourceUnit (Either Cache PostRawProgram)]
parse h contentSeq = do
  parseGensymHandle <- liftIO Gensym.createHandle
  let parseHandle = new parseGensymHandle h
  liftIO $ UnusedTopLevelName.clear (unusedTopLevelNameHandle parseHandle)
  cacheOrProgList <- forP contentSeq $ \(source, cacheOrContent) -> do
    gensymHandle <- liftIO Gensym.createHandle
    let unitParseHandle = new gensymHandle h
    case cacheOrContent of
      Left _ ->
        return ()
      Right _ ->
        liftIO $
          Logger.report (Global.loggerHandle h) $
            "Parsing: " <> T.pack (show $ Source.sourceFilePath source)
    prog <- parseSource unitParseHandle source cacheOrContent
    localHandle <- Local.new h source
    let locatorHandle = Local.locatorHandle localHandle
    return (gensymHandle, localHandle, (source, fmap (postprocess locatorHandle) prog))
  forP_ cacheOrProgList $ \(_, _, (source, cacheOrProg)) -> do
    registerTopLevelNames parseHandle source cacheOrProg
  return cacheOrProgList

parseSource ::
  Handle ->
  Source.Source ->
  Either Cache.Cache T.Text ->
  App (Either Cache.Cache RawProgram)
parseSource h source cacheOrContent = do
  let filePath = Source.sourceFilePath source
  case cacheOrContent of
    Left cache -> do
      return $ Left cache
    Right fileContent -> do
      prog <- runParser filePath fileContent True (Parse.parseProgram (parseHandle h))
      return $ Right $ snd prog

postprocess :: Locator.Handle -> RawProgram -> PostRawProgram
postprocess h (RawProgram m importList requireList stmtList) = do
  let stmtList' = concatMap (postprocess' h [] . fst) stmtList
  let nominalNameList = concatMap collectNominalDecls stmtList'
  let stmtList'' = map (markNominalData nominalNameList) stmtList'
  PostRawProgram m importList requireList stmtList''

collectNominalDecls :: PostRawStmt -> [DD.DefiniteDescription]
collectNominalDecls stmt =
  case stmt of
    PostRawStmtNominal _ _ geistList ->
      flip map (SE.extract geistList) $ \(_, geist, _) -> fst $ RT.name geist
    PostRawStmtNamespace _ _ children ->
      concatMap collectNominalDecls children
    _ ->
      []

markNominalData :: [DD.DefiniteDescription] -> PostRawStmt -> PostRawStmt
markNominalData nominalNameList stmt =
  case stmt of
    PostRawStmtDefineType c (SK.Data dataName dataParams consInfoList _) def
      | dataName `elem` nominalNameList ->
          PostRawStmtDefineType c (SK.Data dataName dataParams consInfoList True) def
    PostRawStmtNamespace m name children ->
      PostRawStmtNamespace m name (map (markNominalData nominalNameList) children)
    _ ->
      stmt

postprocess' :: Locator.Handle -> [BN.BaseName] -> RawStmt -> [PostRawStmt]
postprocess' h nsPath stmt = do
  case stmt of
    RawStmtDefineTerm c stmtKind rawDef@(RT.RawDef {geist}) -> do
      let geist' = liftGeist h nsPath geist
      let stmtKind' = liftStmtKindTerm h nsPath stmtKind
      [PostRawStmtDefineTerm c stmtKind' (rawDef {RT.geist = geist'})]
    RawStmtDefineType c aliasKind rawDef@(RT.RawTypeDef {typeGeist}) -> do
      let geist' = liftGeist h nsPath typeGeist
      let stmtKind = case aliasKind of
            TransparentAlias -> SK.Alias
            OpaqueAlias -> SK.AliasOpaque
      [PostRawStmtDefineType c stmtKind (rawDef {RT.typeGeist = geist'})]
    RawStmtDefineData _ m (name, _) params consInfo loc -> do
      let name' = Locator.attachCurrentLocatorWithin h nsPath name
      let consInfo' = fmap (liftRawCons h nsPath) consInfo
      defineData m name' params (SE.extract consInfo') loc
    RawStmtDefineResource c m (name, c1) (c2, discarder) (c3, copier) (c4, resourceSize) c5 -> do
      let name' = Locator.attachCurrentLocatorWithin h nsPath name
      [PostRawStmtDefineResource c m (name', c1) (c2, discarder) (c3, copier) (c4, resourceSize) c5]
    RawStmtTrope c m (name, c1) defineMetaList loc -> do
      let name' = Locator.attachCurrentLocatorWithin h nsPath name
      let liftDefineMeta index (RawDefineMeta defineMetaLoc' defineMetaTarget' defineMetaTargetArgs' defineMetaExpParams' defineMetaCod' defineMetaBody' defineMetaEndLoc') = do
            PostRawDefineMeta
              { postDefineMetaLoc = defineMetaLoc',
                postDefineMetaTarget = defineMetaTarget',
                postDefineMetaTargetArgs = defineMetaTargetArgs',
                postDefineMetaExpParams = defineMetaExpParams',
                postDefineMetaCod = defineMetaCod',
                postDefineMetaBody = defineMetaBody',
                postDefineMetaEndLoc = defineMetaEndLoc',
                postDefineMetaHelperName = DD.getKnotDD name' index
              }
      let defineMetaElems =
            zipWith
              (\index (comment, defineMeta) -> (comment, liftDefineMeta index defineMeta))
              [0 ..]
              (SE.elems defineMetaList)
      [PostRawStmtTrope c m (name', c1) (defineMetaList {SE.elems = defineMetaElems}) loc]
    RawStmtVariadic kind _ m (name, _) (_, leaf, leafType) (_, node, nodeType) (_, root, rootType) _ loc -> do
      let name' = Locator.attachCurrentLocatorWithin h nsPath name
      defineVariadic kind m name' (leaf, leafType) (node, nodeType) (root, rootType) loc
    RawStmtNominal c m geistList -> do
      let geistList' = fmap (\(tag, geist, endLoc) -> (tag, liftGeist h nsPath geist, endLoc)) geistList
      [PostRawStmtNominal c m geistList']
    RawStmtForeign m foreignList -> do
      [PostRawStmtForeign m foreignList]
    RawStmtExpose c exportList -> do
      [PostRawStmtExpose c (SE.extract exportList)]
    RawStmtNamespace _ m (name, _) _ children _ -> do
      let name' = Locator.attachCurrentLocatorWithin h nsPath name
      let children' = concatMap (postprocess' h (nsPath ++ [name]) . fst) children
      [PostRawStmtNamespace m name' children']

liftGeist :: Locator.Handle -> [BN.BaseName] -> RT.RawGeist BN.BaseName -> RT.RawGeist DD.DefiniteDescription
liftGeist h nsPath geist = do
  fmap (Locator.attachCurrentLocatorWithin h nsPath) geist

liftStmtKindTerm :: Locator.Handle -> [BN.BaseName] -> RawStmtKindTerm BN.BaseName -> RawStmtKindTerm DD.DefiniteDescription
liftStmtKindTerm _h nsPath stmtKind = do
  case stmtKind of
    SK.Define ->
      SK.Define
    SK.DestPassing ->
      SK.DestPassing
    SK.DestPassingInline ->
      SK.DestPassingInline
    SK.Inline ->
      SK.Inline
    SK.Constant ->
      SK.Constant
    SK.ConstantMeta ->
      SK.ConstantMeta
    SK.Macro ->
      SK.Macro
    SK.MacroInline ->
      SK.MacroInline
    SK.Main t ->
      SK.Main t
    SK.DataIntro name dataParams consImpParams expConsParams discriminant -> do
      let name' = Locator.attachCurrentLocatorWithin _h nsPath name
      SK.DataIntro name' dataParams consImpParams expConsParams discriminant

liftRawCons :: Locator.Handle -> [BN.BaseName] -> RawConsInfo BN.BaseName -> RawConsInfo DD.DefiniteDescription
liftRawCons h nsPath (RawConsInfo {loc, name, impParams, expParams, endLoc}) = do
  let name' = Locator.attachCurrentLocatorWithin h nsPath name
  RawConsInfo {loc, name = name', impParams, expParams, endLoc}

registerTopLevelNames ::
  Handle ->
  Source.Source ->
  Either Cache.Cache PostRawProgram ->
  App ()
registerTopLevelNames h source cacheOrContent = do
  case cacheOrContent of
    Left cache -> do
      let stmtList = Cache.stmtList cache
      let nameArrowList = NameMap.getGlobalNames' stmtList
      liftIO $ saveTopLevelNames h source nameArrowList
      ParamKey.insertMany (paramKeyHandle h) $ concatMap cachedParamKeyEntries stmtList
    Right (PostRawProgram _ _ _ stmtList) -> do
      let nameArrowList = NameMap.getGlobalNames stmtList
      liftIO $ saveTopLevelNames h source nameArrowList
      ParamKey.insertMany (paramKeyHandle h) $ concatMap rawParamKeyEntries stmtList

saveTopLevelNames ::
  Handle ->
  Source.Source ->
  [(DD.DefiniteDescription, (Hint, Maybe NT.NominalTag, GN.GlobalName))] ->
  IO ()
saveTopLevelNames h source nameArrowList = do
  let nameMap = Map.fromList nameArrowList
  GlobalNameMap.insert (globalNameMapHandle h) (Source.sourceFilePath source) nameMap
  UnusedTopLevelName.insertMany (unusedTopLevelNameHandle h) $ concatMap unusedTopLevelNameEntries nameArrowList
  OptimizableData.insertMany (optDataHandle h) $ concatMap optimizableDataEntries nameArrowList

unusedTopLevelNameEntries ::
  (DD.DefiniteDescription, (Hint, Maybe NT.NominalTag, GN.GlobalName)) ->
  [(DD.DefiniteDescription, Hint, GN.GlobalName)]
unusedTopLevelNameEntries (dd, (m, _, gn)) =
  case gn of
    GN.Namespace ->
      []
    _ ->
      [(dd, m, gn)]

rawParamKeyEntries :: PostRawStmt -> [ParamKey.Entry]
rawParamKeyEntries stmt = do
  case stmt of
    PostRawStmtDefineTerm _ stmtKind (RT.RawDef {geist}) -> do
      let name = fst $ RT.name geist
      let isConstLike = RT.isConstLike geist
      let m = RT.loc geist
      case stmtKind of
        SK.DataIntro _ _ _ expConsParams _ -> do
          let expKeys = map (\(_, _, x, _, _, _) -> x) expConsParams
          [ ParamKey.Entry
              { entryHint = m,
                entryName = name,
                entryOrigin = ParamKey.Defined,
                entryIsConstLike = isConstLike,
                entryImpKeys = [],
                entryExpKeys = expKeys,
                entryDefaultKeys = []
              }
            ]
        _ -> do
          let RT.GeistKeys {geistImpKeys = impKeys, geistExpKeys = expKeys, geistDefaultKeys = defaultKeys} = RT.getGeistKeys geist
          [ ParamKey.Entry
              { entryHint = m,
                entryName = name,
                entryOrigin = ParamKey.Defined,
                entryIsConstLike = isConstLike,
                entryImpKeys = impKeys,
                entryExpKeys = expKeys,
                entryDefaultKeys = defaultKeys
              }
            ]
    PostRawStmtDefineType {} ->
      []
    PostRawStmtNominal _ _ geistList ->
      flip concatMap (SE.extract geistList) $ \(tag, geist, _) ->
        if NT.isTermTag tag
          then do
            let name = fst $ RT.name geist
            let isConstLike = RT.isConstLike geist
            let m = RT.loc geist
            let RT.GeistKeys {geistImpKeys = impKeys, geistExpKeys = expKeys} = RT.getGeistKeys geist
            [ ParamKey.Entry
                { entryHint = m,
                  entryName = name,
                  entryOrigin = ParamKey.Declared,
                  entryIsConstLike = isConstLike,
                  entryImpKeys = impKeys,
                  entryExpKeys = expKeys,
                  entryDefaultKeys = []
                }
              ]
          else []
    PostRawStmtDefineResource {} ->
      []
    PostRawStmtTrope {} ->
      []
    PostRawStmtVariadic {} ->
      []
    PostRawStmtForeign {} ->
      []
    PostRawStmtExpose {} ->
      []
    PostRawStmtNamespace _ _ children ->
      concatMap rawParamKeyEntries children

cachedParamKeyEntries :: Stmt -> [ParamKey.Entry]
cachedParamKeyEntries stmt = do
  case stmt of
    StmtDefine isConstLike stmtKind (SavedHint m) name impParams expParams defaultParams _ _ -> do
      case stmtKind of
        SK.DataIntro _ _ _ expConsParams _ -> do
          let expKeys = map (\(_, _, x, _) -> toText x) expConsParams
          [ ParamKey.Entry
              { entryHint = m,
                entryName = name,
                entryOrigin = ParamKey.Defined,
                entryIsConstLike = isConstLike,
                entryImpKeys = [],
                entryExpKeys = expKeys,
                entryDefaultKeys = []
              }
            ]
        _ -> do
          let impKeys = map (\(_, _, x, _) -> toText x) impParams
          let defaultKeys = map ((\(_, _, x, _) -> toText x) . fst) defaultParams
          let expKeys = map (\(_, _, x, _) -> toText x) expParams
          [ ParamKey.Entry
              { entryHint = m,
                entryName = name,
                entryOrigin = ParamKey.Defined,
                entryIsConstLike = isConstLike,
                entryImpKeys = impKeys,
                entryExpKeys = expKeys,
                entryDefaultKeys = defaultKeys
              }
            ]
    StmtDefineType {} ->
      []
    StmtDefineResource {} ->
      []
    StmtTrope {} ->
      []
    StmtVariadic {} ->
      []
    StmtForeign {} ->
      []
    StmtExpose {} ->
      []
    StmtNamespace {} ->
      []

optimizableDataEntries ::
  (DD.DefiniteDescription, (Hint, Maybe NT.NominalTag, GN.GlobalName)) ->
  [(DD.DefiniteDescription, OD.OptimizableData)]
optimizableDataEntries (dd, (_, _, gn)) =
  case gn of
    GN.Data dataArity consNameArrowList _
      | isUnary consNameArrowList ->
          (dd, OD.Unary) : map (\(consName, _) -> (consName, OD.Unary)) consNameArrowList
      | isNullary dataArity consNameArrowList ->
          (dd, OD.Enum) : map (\(consName, _) -> (consName, OD.Enum)) consNameArrowList
    _ ->
      []

isUnary :: [(DD.DefiniteDescription, (Hint, GN.GlobalName))] -> Bool
isUnary consInfoList =
  case consInfoList of
    [(_, (_, GN.DataIntro _ consImpParamNames consArity _ _ _))] ->
      null consImpParamNames && consArity == Arity.fromInt 1
    _ ->
      False

isNullary :: Arity.Arity -> [(DD.DefiniteDescription, (Hint, GN.GlobalName))] -> Bool
isNullary dataArity consInfoList = do
  let b1 = dataArity == Arity.fromInt 0
  let b2 = all (\(_, (_, gn)) -> GN.isNullary gn) consInfoList
  b1 && b2
