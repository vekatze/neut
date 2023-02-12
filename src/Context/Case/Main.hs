module Context.Case.Main
  ( execute,
  )
where

import Act.Build qualified as Build
import Act.Check qualified as Check
import Act.Clean qualified as Clean
import Act.Get qualified as Get
import Act.Init qualified as Init
import Act.Release qualified as Release
import Act.Run qualified as Run
import Act.Tidy qualified as Tidy
import Act.Version qualified as Version
import Context.Alias qualified as Alias
import Context.Case.Main.Alias qualified as MainAlias
import Context.Case.Main.Cache qualified as MainCache
import Context.Case.Main.External qualified as MainExternal
import Context.Case.Main.Global qualified as MainGlobal
import Context.Case.Main.LLVM qualified as MainLLVM
import Context.Case.Main.Locator qualified as MainLocator
import Context.Case.Main.Log qualified as MainLog
import Context.Case.Main.Module qualified as MainModule
import Context.Case.Main.OptParse qualified as MainOptParse
import Context.Case.Main.Path qualified as MainPath
import Context.Case.Main.Throw qualified as MainThrow
import Context.CodataDefinition qualified as CodataDefinition
import Context.CompDefinition qualified as CompDefinition
import Context.DataDefinition qualified as DataDefinition
import Context.Definition qualified as Definition
import Context.Enum qualified as Enum
import Context.Env qualified as Env
import Context.External qualified as External
import Context.Gensym qualified as Gensym
import Context.Global qualified as Global
import Context.Implicit qualified as Implicit
import Context.LLVM qualified as LLVM
import Context.Locator qualified as Locator
import Context.Log qualified as Log
import Context.Module qualified as Module
import Context.OptParse qualified as OptParse
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Context.Type qualified as Type
import Control.Comonad.Cofree
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as L
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.IORef.Unboxed
import Data.IntMap qualified as IntMap
import Data.Maybe
import Data.PQueue.Min qualified as Q
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Entity.AliasInfo
import Entity.Arity qualified as A
import Entity.Binder
import Entity.Command qualified as C
import Entity.Comp
import Entity.Comp.Reduce qualified as Comp (Context)
import Entity.Const
import Entity.Constraint
import Entity.DeclarationName qualified as DN
import Entity.DefiniteDescription qualified as DD
import Entity.DefiniteLocator qualified as DL
import Entity.Discriminant qualified as D
import Entity.GlobalLocatorAlias qualified as GLA
import Entity.GlobalName qualified as GN
import Entity.HoleSubst qualified as HS
import Entity.Ident
import Entity.Ident.Reify qualified as Ident
import Entity.ImpArgNum qualified as I
import Entity.LamKind qualified as LK
import Entity.LowComp.Reduce qualified as LowComp (Context)
import Entity.LowType qualified as LT
import Entity.Module qualified as M
import Entity.Module qualified as Module
import Entity.ModuleAlias qualified as MA
import Entity.ModuleChecksum qualified as MC
import Entity.Opacity qualified as O
import Entity.Pattern.Fallback qualified as PatternFallback
import Entity.Pattern.Specialize qualified as PatternSpecialize
import Entity.Section qualified as Section
import Entity.Source qualified as Source
import Entity.StrictGlobalLocator qualified as SGL
import Entity.TargetPlatform qualified as TP
import Entity.Term qualified as TM
import Entity.Term.Subst qualified as Subst (Context (..))
import Entity.VisitInfo
import Entity.WeakTerm qualified as WT
import Path
import Path.IO
import Scene.Clarify qualified as Clarify (Context)
import Scene.Clarify.Context qualified as ClarifyBase (Context (..))
import Scene.Elaborate qualified as Elaborate (Context (..))
import Scene.Elaborate.Infer qualified as ElaborateInfer (Context (..))
import Scene.Elaborate.Unify qualified as ElaborateUnify (Context (..))
import Scene.Emit qualified as Emit (Context)
import Scene.Fetch qualified as Fetch (Context (..))
import Scene.Initialize qualified as Initialize (Context)
import Scene.Link qualified as Link
import Scene.Lower qualified as Lower (Context (..))
import Scene.Parse qualified as Parse (Context (..))
import Scene.Parse.Core qualified as ParseCore (Context (..))
import Scene.Parse.Discern qualified as ParseDiscern (Context)
import Scene.Parse.Import qualified as ParseImport (Context)
import Scene.Unravel qualified as Unravel (Context)
import System.Environment
import System.Info qualified as SI

type Ref a = IORef (Maybe a)

type FastRef a = IORef a

data Env = Env
  { counter :: IORefU Int,
    endOfEntry :: FastRef T.Text,
    clangOptString :: FastRef String,
    shouldColorize :: FastRef Bool,
    moduleCacheMap :: FastRef (Map.HashMap (Path Abs File) M.Module),
    moduleAliasMap :: FastRef (Map.HashMap MA.ModuleAlias MC.ModuleChecksum),
    locatorAliasMap :: FastRef (Map.HashMap GLA.GlobalLocatorAlias SGL.StrictGlobalLocator),
    nameMap :: FastRef (Map.HashMap DD.DefiniteDescription GN.GlobalName),
    currentSectionStack :: FastRef [Section.Section],
    shouldCancelAlloc :: FastRef Bool,
    nopFreeSet :: FastRef (S.Set Int),
    sourceAliasMap :: FastRef SourceAliasMap,
    constraintEnv :: FastRef [(WT.WeakTerm, WT.WeakTerm)],
    holeSubst :: FastRef HS.HoleSubst,
    sourceChildrenMap :: FastRef (Map.HashMap (Path Abs File) [Source.Source]),
    traceSourceList :: FastRef [Source.Source],
    weakTypeEnv :: FastRef (IntMap.IntMap WT.WeakTerm),
    holeEnv :: FastRef (IntMap.IntMap (WT.WeakTerm, WT.WeakTerm)),
    constraintQueue :: FastRef (Q.MinQueue SuspendedConstraint),
    hasObjectSet :: FastRef (S.Set (Path Abs File)),
    hasCacheSet :: FastRef (S.Set (Path Abs File)),
    hasLLVMSet :: FastRef (S.Set (Path Abs File)),
    visitEnv :: FastRef (Map.HashMap (Path Abs File) VisitInfo),
    defMap :: FastRef (Map.HashMap DD.DefiniteDescription WT.WeakTerm),
    compDefMap :: FastRef (Map.HashMap DD.DefiniteDescription (O.Opacity, [Ident], Comp)),
    dataDefMap :: FastRef (Map.HashMap DD.DefiniteDescription [(D.Discriminant, [BinderF TM.Term], [BinderF TM.Term])]),
    codataDefMap :: FastRef (Map.HashMap DD.DefiniteDescription ((DD.DefiniteDescription, A.Arity), [DD.DefiniteDescription])),
    enumSet :: FastRef (S.Set DD.DefiniteDescription),
    declEnv :: FastRef (Map.HashMap DN.DeclarationName ([LT.LowType], LT.LowType)),
    definedNameSet :: FastRef (S.Set DD.DefiniteDescription),
    impEnv :: FastRef (Map.HashMap DD.DefiniteDescription I.ImpArgNum),
    compEnv :: FastRef (Map.HashMap DD.DefiniteDescription (O.Opacity, [Ident], Comp)),
    typeEnv :: FastRef (Map.HashMap DD.DefiniteDescription WT.WeakTerm),
    activeGlobalLocatorList :: FastRef [SGL.StrictGlobalLocator],
    activeDefiniteLocatorList :: FastRef [DL.DefiniteLocator],
    currentGlobalLocator :: Ref SGL.StrictGlobalLocator,
    currentSource :: Ref Source.Source,
    mainModule :: Ref Module.Module,
    targetPlatform :: Ref TP.TargetPlatform
  }

newtype App a = App
  { reify :: ReaderT Env IO a
  }
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader Env,
      MonadIO,
      MonadThrow,
      MonadFail,
      MonadCatch,
      MonadUnliftIO,
      MonadMask
    )

execute :: IO ()
execute = do
  runApp $ do
    c <- OptParse.parseCommand
    Throw.run $ do
      case c of
        C.Build cfg -> do
          Build.build cfg
        C.Run cfg -> do
          Run.run cfg
        C.Check cfg -> do
          Check.check cfg
        C.Clean cfg ->
          Clean.clean cfg
        C.Release cfg ->
          Release.release cfg
        C.Init cfg ->
          Init.initialize cfg
        C.Get cfg ->
          Get.get cfg
        C.Tidy cfg ->
          Tidy.tidy cfg
        C.ShowVersion cfg ->
          Version.showVersion cfg

runApp :: App a -> IO a
runApp app = do
  newEnv >>= runReaderT (reify app)

newEnv :: IO Env
newEnv = do
  counter <- newIORefU 0
  endOfEntry <- newFastRef
  clangOptString <- newFastRef
  shouldColorize <- newIORef True
  moduleCacheMap <- newFastRef
  moduleAliasMap <- newFastRef
  locatorAliasMap <- newFastRef
  nameMap <- newFastRef
  currentSectionStack <- newFastRef
  shouldCancelAlloc <- newIORef True
  nopFreeSet <- newFastRef
  sourceAliasMap <- newFastRef
  constraintEnv <- newFastRef
  holeSubst <- newFastRef
  sourceChildrenMap <- newFastRef
  weakTypeEnv <- newFastRef
  holeEnv <- newFastRef
  constraintQueue <- newFastRef
  traceSourceList <- newFastRef
  hasObjectSet <- newFastRef
  hasLLVMSet <- newFastRef
  definedNameSet <- newFastRef
  hasCacheSet <- newFastRef
  visitEnv <- newFastRef
  defMap <- newFastRef
  compDefMap <- newFastRef
  dataDefMap <- newFastRef
  codataDefMap <- newFastRef
  enumSet <- newFastRef
  declEnv <- newFastRef
  impEnv <- newFastRef
  compEnv <- newFastRef
  typeEnv <- newFastRef
  activeGlobalLocatorList <- newFastRef
  activeDefiniteLocatorList <- newFastRef
  currentGlobalLocator <- newRef
  currentSource <- newRef
  mainModule <- newRef
  targetPlatform <- newRef
  return Env {..}

instance Initialize.Context App

instance Link.Context App

instance Build.Context App

instance Check.Context App

instance Clean.Context App

instance Get.Context App

instance Init.Context App

instance Release.Context App

instance Run.Context App

instance Tidy.Context App

instance Version.Context App where
  printString = liftIO . putStrLn

instance Unravel.Context App

instance Fetch.Context App where
  withTempFile =
    withSystemTempFile "fetch"
  writeModule targetModule =
    liftIO $ TIO.writeFile (toFilePath $ M.moduleLocation targetModule) $ M.ppModule targetModule
  getHandleContents =
    liftIO . B.hGetContents

instance Parse.Context App where
  loadCache = MainCache.loadCache

instance ParseCore.Context App where
  getTargetPlatform =
    Env.getTargetPlatform
  readSourceFile =
    liftIO . TIO.readFile . toFilePath
  ensureExistence path = do
    fileExists <- doesFileExist path
    unless fileExists $ do
      Throw.raiseError' $ T.pack $ "no such file exists: " <> toFilePath path

instance ParseDiscern.Context App

instance PatternSpecialize.Context App

instance PatternFallback.Context App

instance ParseImport.Context App

instance Elaborate.Context App where
  initialize = do
    Env.setConstraintEnv []
    writeRef' weakTypeEnv IntMap.empty
    writeRef' holeEnv IntMap.empty
  saveCache =
    MainCache.saveCache

instance ElaborateInfer.Context App where
  insWeakTypeEnv k v =
    modifyRef' weakTypeEnv $ IntMap.insert (Ident.toInt k) v
  lookupWeakTypeEnv m k = do
    wtenv <- readRef' weakTypeEnv
    case IntMap.lookup (Ident.toInt k) wtenv of
      Just t ->
        return t
      Nothing ->
        Throw.raiseCritical m $
          Ident.toText' k <> " is not found in the weak type environment."
  lookupHoleEnv i =
    IntMap.lookup i <$> readRef' holeEnv
  insHoleEnv i e1 e2 =
    modifyRef' holeEnv $ IntMap.insert i (e1, e2)

instance ElaborateUnify.Context App where
  insertSubst =
    Env.insertSubst
  setConstraintQueue =
    writeRef' constraintQueue
  insertConstraint sc =
    modifyRef' constraintQueue $ Q.insert sc
  getConstraintQueue =
    readRef' constraintQueue
  getDefMap =
    Definition.read

instance Subst.Context App where
  newIdentFromIdent = Gensym.newIdentFromIdent

instance Clarify.Context App

instance ClarifyBase.Context App where
  initialize = do
    writeRef' compDefMap mempty
  getAuxEnv =
    readRef' compDefMap
  insertToAuxEnv k v =
    modifyRef' compDefMap $ Map.insert k v
  isAlreadyRegistered dd =
    Map.member dd <$> ClarifyBase.getAuxEnv

instance Lower.Context App where
  initialize nameList = do
    writeRef' definedNameSet $ S.fromList nameList
    writeRef' declEnv $
      Map.fromList
        [ (DN.malloc, ([LT.voidPtr], LT.voidPtr)),
          (DN.free, ([LT.voidPtr], LT.voidPtr))
        ]
  getDeclEnv =
    readRef' declEnv
  insDeclEnv k arity =
    modifyRef' declEnv $ Map.insert k (LT.toVoidPtrSeq arity, LT.voidPtr)
  getDefinedNameSet =
    readRef' definedNameSet

instance Emit.Context App

instance Env.Context App where
  setShouldColorize = writeRef' shouldColorize
  getShouldColorize = readRef' shouldColorize
  setEndOfEntry = writeRef' endOfEntry
  getEndOfEntry = readRef' endOfEntry
  insertToModuleCacheMap k v = modifyRef' moduleCacheMap $ Map.insert k v
  getModuleCacheMap = readRef' moduleCacheMap
  insertToLocatorAliasMap k v = modifyRef' locatorAliasMap $ Map.insert k v
  getLocatorAliasMap = readRef' locatorAliasMap
  setModuleAliasMap = writeRef' moduleAliasMap
  insertToModuleAliasMap k v = modifyRef' moduleAliasMap $ Map.insert k v
  getModuleAliasMap = readRef' moduleAliasMap
  setNameMap = writeRef' nameMap
  insertToNameMap k v = modifyRef' nameMap $ Map.insert k v
  getNameMap = readRef' nameMap
  setClangOptString = writeRef' clangOptString
  getClangOptString = readRef' clangOptString
  setCurrentSectionStack = writeRef' currentSectionStack
  getCurrentSectionStack = readRef' currentSectionStack
  setMainModule = writeRef mainModule
  getMainModule = readRef "mainModule" mainModule
  setCurrentSource = writeRef currentSource
  getCurrentSource = readRef "currentSource" currentSource
  setShouldCancelAlloc = writeRef' shouldCancelAlloc
  getShouldCancelAlloc = readRef' shouldCancelAlloc
  setNopFreeSet = writeRef' nopFreeSet
  getNopFreeSet = readRef' nopFreeSet
  insertToNopFreeSet i = modifyRef' nopFreeSet $ S.insert i
  getSourceAliasMap = readRef' sourceAliasMap
  insertToSourceAliasMap k v = modifyRef' sourceAliasMap $ Map.insert k v
  setConstraintEnv = writeRef' constraintEnv
  insConstraintEnv e1 e2 = modifyRef' constraintEnv $ (:) (e1, e2)
  getConstraintEnv = readRef' constraintEnv
  setHoleSubst = writeRef' holeSubst
  insertSubst holeID xs e = modifyRef' holeSubst $ HS.insert holeID xs e
  getHoleSubst = readRef' holeSubst
  setTargetPlatform = do
    mTargetArch <- liftIO $ lookupEnv envVarTargetArch
    mTargetOS <- liftIO $ lookupEnv envVarTargetOS
    let targetOS = fromMaybe SI.os mTargetOS
    let targetArch = fromMaybe SI.arch mTargetArch
    writeRef targetPlatform $ TP.TargetPlatform {os = targetOS, arch = targetArch}
  getTargetPlatform = readRef "targetPlatform" targetPlatform
  insertToSourceChildrenMap k v = modifyRef' sourceChildrenMap $ Map.insert k v
  getSourceChildrenMap = readRef' sourceChildrenMap
  setTraceSourceList = writeRef' traceSourceList
  pushToTraceSourceList source = modifyRef' traceSourceList $ (:) source
  popFromTraceSourceList = do modifyRef' traceSourceList tail
  getTraceSourceList = readRef' traceSourceList
  insertToHasObjectSet v = modifyRef' hasObjectSet $ S.insert v
  getHasObjectSet = readRef' hasObjectSet
  insertToHasCacheSet v = modifyRef' hasCacheSet $ S.insert v
  getHasLLVMSet = readRef' hasLLVMSet
  insertToHasLLVMSet v = modifyRef' hasLLVMSet $ S.insert v
  getHasCacheSet = readRef' hasCacheSet
  insertToVisitEnv k v = modifyRef' visitEnv $ Map.insert k v
  getVisitEnv = readRef' visitEnv

instance MainThrow.Context App

instance Throw.Context App where
  throw =
    MainThrow.throw
  try =
    MainThrow.try
  run =
    MainThrow.run

instance MainLog.Context App

instance Log.Context App where
  printLog =
    MainLog.printLogIO

instance OptParse.Context App where
  parseCommand =
    MainOptParse.parseCommand

instance MainLLVM.Context App

instance LLVM.Context App where
  emit =
    MainLLVM.emit
  link =
    MainLLVM.link

instance MainPath.Context App

instance Path.Context App where
  getLibraryDirPath = MainPath.getLibraryDirPath
  getCurrentDir = getCurrentDir
  ensureNotInLibDir = MainPath.ensureNotInLibDir
  resolveDir = resolveDir
  resolveFile = resolveFile
  doesDirExist = doesDirExist
  doesFileExist = doesFileExist
  getModificationTime = getModificationTime
  ensureDir = ensureDir
  stripPrefix = stripProperPrefix
  writeText path text = liftIO $ TIO.writeFile (toFilePath path) text
  writeByteString path content = liftIO $ L.writeFile (toFilePath path) content
  parseRelFile = parseRelFile
  removeDirRecur = removeDirRecur

instance MainModule.Context App

instance Module.Context App where
  getModuleFilePath = MainModule.getModuleFilePath
  getModule = MainModule.getModule
  getSourcePath = MainModule.getSourcePath

instance MainCache.Context App

instance Source.Context App

instance Comp.Context App

instance LowComp.Context App

instance MainAlias.Context App

instance Alias.Context App where
  initializeAliasMap = MainAlias.initializeAliasMap
  registerGlobalLocatorAlias = MainAlias.registerGlobalLocatorAlias
  resolveAlias = MainAlias.resolveAlias

instance MainGlobal.Context App

instance Global.Context App where
  registerTopLevelFunc = MainGlobal.registerTopLevelFunc
  registerData = MainGlobal.registerData
  registerResource = MainGlobal.registerResource
  lookup = MainGlobal.lookup
  initialize = MainGlobal.initialize

instance Gensym.Context App where
  newCount =
    asks counter >>= \ref -> liftIO $ atomicAddCounter ref 1
  readCount =
    asks counter >>= liftIO . readIORefU
  writeCount v =
    asks counter >>= \ref -> liftIO $ writeIORefU ref v

instance DataDefinition.Context App where
  insert dataName dataArgs consInfoList = do
    asks dataDefMap >>= \ref -> do
      liftIO $ do
        hashMap <- readIORef ref
        let value = map (\(_, consArgs, discriminant) -> (discriminant, dataArgs, consArgs)) consInfoList
        writeIORef ref (Map.insert dataName value hashMap)
  lookup dataName = do
    Map.lookup dataName <$> (asks dataDefMap >>= liftIO . readIORef)

instance CodataDefinition.Context App where
  insert dataName dataNewInfo consNameList =
    asks codataDefMap >>= \ref -> do
      liftIO $ do
        hashMap <- readIORef ref
        writeIORef ref (Map.insert dataName (dataNewInfo, consNameList) hashMap)
  lookup m dataName = do
    mValue <- Map.lookup dataName <$> (asks codataDefMap >>= liftIO . readIORef)
    case mValue of
      Just value ->
        return value
      Nothing ->
        Throw.raiseError m $ "no such codata is defined: " <> DD.reify dataName

instance Definition.Context App where
  read =
    asks defMap >>= liftIO . readIORef
  lookup =
    return Map.lookup
  insert opacity m name xts e =
    when (opacity == O.Transparent) $
      asks defMap >>= \ref ->
        liftIO $
          modifyIORef ref $
            Map.insert name (m :< WT.PiIntro (LK.Normal opacity) xts e)

instance Implicit.Context App where
  insert k v =
    asks impEnv >>= \ref -> liftIO $ modifyIORef' ref $ Map.insert k v
  lookup k =
    Map.lookup k <$> (asks impEnv >>= liftIO . readIORef)

instance Type.Context App where
  insert k v =
    asks typeEnv >>= \ref -> liftIO $ modifyIORef' ref $ Map.insert k v
  lookup m k = do
    mValue <- asks typeEnv >>= \ref -> Map.lookup k <$> liftIO (readIORef ref)
    case mValue of
      Just value ->
        return value
      Nothing ->
        Throw.raiseCritical m $ "`" <> DD.reify k <> "` is not found in the term type environment."

instance MainExternal.Context App

instance External.Context App where
  run = MainExternal.run

instance CompDefinition.Context App where
  insert k v =
    asks compEnv >>= \ref -> liftIO $ modifyIORef' ref $ Map.insert k v
  union otherEnv =
    asks compEnv >>= \ref -> liftIO $ modifyIORef' ref $ Map.union otherEnv
  lookup k = do
    cenv <- asks compEnv >>= liftIO . readIORef
    return $ Map.lookup k cenv

instance Enum.Context App where
  insert dd =
    asks enumSet >>= \ref -> liftIO $ modifyIORef' ref $ S.insert dd
  isMember dd =
    asks enumSet >>= \ref -> liftIO $ do
      enumSet <- readIORef ref
      return $ S.member dd enumSet

instance MainLocator.Context App where
  setActiveGlobalLocatorList v =
    asks activeGlobalLocatorList >>= \ref -> liftIO $ writeIORef ref v
  getActiveGlobalLocatorList =
    asks activeGlobalLocatorList >>= liftIO . readIORef
  setActiveDefiniteLocatorList v =
    asks activeDefiniteLocatorList >>= \ref -> liftIO $ writeIORef ref v
  getActiveDefiniteLocatorList =
    asks activeDefiniteLocatorList >>= liftIO . readIORef
  setCurrentGlobalLocator =
    writeRef currentGlobalLocator
  getCurrentGlobalLocator =
    readRef "currentGlobalLocator" currentGlobalLocator

instance Locator.Context App where
  initialize = MainLocator.initialize
  withLiftedSection = MainLocator.withLiftedSection
  attachCurrentLocator = MainLocator.attachCurrentLocator
  activateGlobalLocator = MainLocator.activateGlobalLocator
  activateDefiniteLocator = MainLocator.activateDefiniteLocator
  clearActiveLocators = MainLocator.clearActiveLocators
  getPossibleReferents = MainLocator.getPossibleReferents
  getMainDefiniteDescription = MainLocator.getMainDefiniteDescription

readRef :: T.Text -> (Env -> Ref a) -> App a
readRef name accessor = do
  mValue <- asks accessor >>= liftIO . readIORef
  case mValue of
    Just a ->
      return a
    Nothing ->
      raiseUninitializedVariable name

writeRef :: (Env -> Ref a) -> a -> App ()
writeRef accessor value = do
  ref <- asks accessor
  liftIO $ writeIORef ref (Just value)

newFastRef :: Monoid a => IO (FastRef a)
newFastRef =
  newIORef mempty

newRef :: IO (Ref a)
newRef =
  newIORef Nothing

readRef' :: (Env -> FastRef a) -> App a
readRef' accessor = do
  asks accessor >>= liftIO . readIORef

writeRef' :: (Env -> FastRef a) -> a -> App ()
writeRef' accessor value = do
  ref <- asks accessor
  liftIO $ writeIORef ref value

modifyRef' :: (Env -> FastRef a) -> (a -> a) -> App ()
modifyRef' accessor modifier = do
  ref <- asks accessor
  value <- liftIO $ readIORef ref
  liftIO $ writeIORef ref $ modifier value

raiseUninitializedVariable :: T.Text -> App a
raiseUninitializedVariable name =
  Throw.raiseCritical' $ "`" <> name <> "` is uninitialized"
