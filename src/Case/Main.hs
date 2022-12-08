module Case.Main
  ( build,
    check,
    clean,
    get,
    initialize,
    release,
    run,
    tidy,
    version,
  )
where

import qualified Act.Build as Build
import qualified Act.Check as Check
import qualified Act.Clean as Clean
import qualified Act.Get as Get
import qualified Act.Init as Init
import qualified Act.Release as Release
import qualified Act.Run as Run
import qualified Act.Tidy as Tidy
import qualified Act.Version as Version
import qualified Case.Main.Alias as MainAlias
import qualified Case.Main.Cache as MainCache
import qualified Case.Main.External as MainExternal
import qualified Case.Main.Global as MainGlobal
import qualified Case.Main.LLVM as MainLLVM
import qualified Case.Main.Locator as MainLocator
import qualified Case.Main.Log as MainLog
import qualified Case.Main.Module as MainModule
import qualified Case.Main.Path as MainPath
import qualified Case.Main.Throw as MainThrow
import qualified Context.Alias as Alias
import qualified Context.CompDefinition as CompDefinition
import qualified Context.DataDefinition as DataDefinition
import qualified Context.Definition as Definition
import qualified Context.Enum as Enum
import qualified Context.Env as Env
import qualified Context.External as External
import qualified Context.Gensym as Gensym
import qualified Context.Global as Global
import qualified Context.Implicit as Implicit
import qualified Context.LLVM as LLVM
import qualified Context.Locator as Locator
import qualified Context.Log as Log
import qualified Context.Module as Module
import qualified Context.Path as Path
import qualified Context.Throw as Throw
import qualified Context.Type as Type
import Control.Comonad.Cofree
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as Map
import Data.IORef
import Data.IORef.Unboxed
import qualified Data.IntMap as IntMap
import qualified Data.PQueue.Min as Q
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Entity.AliasInfo
import Entity.Binder
import Entity.Comp
import qualified Entity.Comp.Reduce as Comp (Context)
import Entity.Constraint
import qualified Entity.DeclarationName as DN
import qualified Entity.DefiniteDescription as DD
import qualified Entity.DefiniteLocator as DL
import qualified Entity.Discriminant as D
import qualified Entity.GlobalLocatorAlias as GLA
import qualified Entity.GlobalName as GN
import qualified Entity.HoleSubst as HS
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import qualified Entity.ImpArgNum as I
import qualified Entity.LamKind as LK
import qualified Entity.LowComp.Reduce as LowComp (Context)
import qualified Entity.LowType as LT
import qualified Entity.Module as M
import qualified Entity.Module as Module
import qualified Entity.ModuleAlias as MA
import qualified Entity.ModuleChecksum as MC
import qualified Entity.Opacity as O
import qualified Entity.Pattern.Fallback as PatternFallback
import qualified Entity.Pattern.Specialize as PatternSpecialize
import qualified Entity.Section as Section
import qualified Entity.Source as Source
import qualified Entity.StrictGlobalLocator as SGL
import qualified Entity.TargetPlatform as TP
import qualified Entity.Term as TM
import qualified Entity.Term.Subst as Subst (Context (..))
import Entity.VisitInfo
import qualified Entity.WeakTerm as WT
import Path
import Path.IO
import qualified Scene.Clarify as Clarify (Context)
import qualified Scene.Clarify.Context as ClarifyBase (Context (..))
import qualified Scene.Elaborate as Elaborate (Context (..))
import qualified Scene.Elaborate.Infer as ElaborateInfer (Context (..))
import qualified Scene.Elaborate.Unify as ElaborateUnify (Context (..))
import qualified Scene.Emit as Emit (Context)
import qualified Scene.Fetch as Fetch (Context (..))
import qualified Scene.Lower as Lower (Context (..))
import qualified Scene.Parse as Parse (Context (..))
import qualified Scene.Parse.Core as ParseCore (Context (..))
import qualified Scene.Parse.Discern as ParseDiscern (Context)
import qualified Scene.Parse.Import as ParseImport (Context)
import qualified Scene.Unravel as Unravel (Context)

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
    visitEnv :: FastRef (Map.HashMap (Path Abs File) VisitInfo),
    defMap :: FastRef (Map.HashMap DD.DefiniteDescription WT.WeakTerm),
    compDefMap :: FastRef (Map.HashMap DD.DefiniteDescription (O.Opacity, [Ident], Comp)),
    dataDefMap :: FastRef (Map.HashMap DD.DefiniteDescription [(D.Discriminant, [BinderF TM.Term], [BinderF TM.Term])]),
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

build :: Build.Config -> IO ()
build cfg = do
  runApp $ Build.build cfg

check :: Check.Config -> IO ()
check cfg = do
  runApp $ Check.check cfg

clean :: Clean.Config -> IO ()
clean cfg = do
  runApp $ Clean.clean cfg

get :: Get.Config -> IO ()
get cfg = do
  runApp $ Get.get cfg

initialize :: Init.Config -> IO ()
initialize cfg = do
  runApp $ Init.initialize cfg

release :: Release.Config -> IO ()
release cfg = do
  runApp $ Release.release cfg

run :: Run.Config -> IO ()
run cfg = do
  runApp $ Run.run cfg

tidy :: Tidy.Config -> IO ()
tidy cfg = do
  runApp $ Tidy.tidy cfg

version :: Version.Config -> IO ()
version cfg = do
  runApp $ Version.showVersion cfg

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
  definedNameSet <- newFastRef
  hasCacheSet <- newFastRef
  visitEnv <- newFastRef
  defMap <- newFastRef
  compDefMap <- newFastRef
  dataDefMap <- newFastRef
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
  setTargetPlatform = writeRef targetPlatform
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
  registerDataIntro = MainGlobal.registerDataIntro
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
  insert dataName discriminant dataArgs consArgs = do
    mDataInfo <- DataDefinition.lookup dataName
    asks dataDefMap >>= \ref -> do
      liftIO $ do
        hashMap <- readIORef ref
        case mDataInfo of
          Nothing ->
            writeIORef ref (Map.insert dataName [(discriminant, dataArgs, consArgs)] hashMap)
          Just dataInfo ->
            writeIORef ref (Map.insert dataName ((discriminant, dataArgs, consArgs) : dataInfo) hashMap)
  lookup dataName = do
    Map.lookup dataName <$> (asks dataDefMap >>= liftIO . readIORef)

-- let yo = dataDefMap env
-- undefined

-- Map.lookup k <$> (asks dataDefMap >>= liftIO . readIORef)

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
            Map.insert name (m :< WT.PiIntro LK.Normal xts e)

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
    case Map.lookup k cenv of
      v@(Just _) ->
        return v
      Nothing -> do
        aenv <- asks compDefMap >>= liftIO . readIORef
        return $ Map.lookup k aenv

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
