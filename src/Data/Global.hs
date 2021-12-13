{-# LANGUAGE TemplateHaskell #-}

module Data.Global where

import Control.Concurrent.Async (Async)
import Control.Monad (when)
import Data.Basic
  ( Hint,
    Ident (..),
    IsReducible,
    PosInfo,
    TopName,
    asText,
    getPosInfo,
    showPosInfo,
  )
import Data.Comp (Comp, Value (ValueVarLocal))
import qualified Data.HashMap.Lazy as Map
import Data.IORef
  ( IORef,
    atomicModifyIORef',
    modifyIORef',
    newIORef,
    readIORef,
  )
import qualified Data.IntMap as IntMap
import Data.Log
  ( Log,
    LogLevel (LogLevelFail, LogLevelPass),
    logLevelToSGR,
    logLevelToText,
    logNote,
    logNote',
    logWarning,
  )
import Data.LowComp (LowComp)
import Data.LowType (LowType, voidPtr)
import qualified Data.PQueue.Min as Q
import qualified Data.Set as S
import Data.Term (TermPlus)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import Data.WeakTerm
  ( Constraint,
    SuspendedConstraintQueue,
    WeakTerm (WeakTermAster),
    WeakTermPlus,
  )
import Path
  ( Abs,
    Dir,
    File,
    Path,
    Rel,
    mkRelDir,
    parent,
    parseAbsDir,
    parseRelDir,
    (</>),
  )
import Path.IO (ensureDir, getHomeDir)
import Paths_neut (version)
import System.Console.ANSI
  ( ConsoleIntensity (BoldIntensity),
    SGR (Reset, SetConsoleIntensity),
    setSGR,
  )
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Info as System

data VisitInfo
  = VisitInfoActive
  | VisitInfoFinish (Map.HashMap T.Text FilePath)

--
-- global variables
--

{-# NOINLINE count #-}
count :: IORef Int
count =
  unsafePerformIO (newIORef 0)

{-# NOINLINE shouldColorize #-}
shouldColorize :: IORef Bool
shouldColorize =
  unsafePerformIO (newIORef True)

{-# NOINLINE shouldDisplayLogLocation #-}
shouldDisplayLogLocation :: IORef Bool
shouldDisplayLogLocation =
  unsafePerformIO (newIORef True)

{-# NOINLINE shouldDisplayLogLevel #-}
shouldDisplayLogLevel :: IORef Bool
shouldDisplayLogLevel =
  unsafePerformIO (newIORef True)

{-# NOINLINE shouldDisplayLogText #-}
shouldDisplayLogText :: IORef Bool
shouldDisplayLogText =
  unsafePerformIO (newIORef True)

{-# NOINLINE shouldDisplayLogFooter #-}
shouldDisplayLogFooter :: IORef Bool
shouldDisplayLogFooter =
  unsafePerformIO (newIORef True)

{-# NOINLINE shouldCancelAlloc #-}
shouldCancelAlloc :: IORef Bool
shouldCancelAlloc =
  unsafePerformIO (newIORef True)

{-# NOINLINE isMain #-}
isMain :: IORef Bool
isMain =
  unsafePerformIO (newIORef False)

{-# NOINLINE endOfEntry #-}
endOfEntry :: IORef String
endOfEntry =
  unsafePerformIO (newIORef "")

{-# NOINLINE topMetaNameEnv #-}
topMetaNameEnv :: IORef (Map.HashMap T.Text Ident)
topMetaNameEnv =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE targetPlatform #-}
targetPlatform :: IORef String
targetPlatform =
  unsafePerformIO (newIORef $ System.arch <> "-" <> System.os)

{-# NOINLINE fileEnv #-}
fileEnv :: IORef (Map.HashMap (Path Abs File) VisitInfo)
fileEnv =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE traceEnv #-}
traceEnv :: IORef [Path Abs File]
traceEnv =
  unsafePerformIO (newIORef [])

pushTrace :: Path Abs File -> IO ()
pushTrace path =
  modifyIORef' traceEnv $ \env -> path : env

popTrace :: IO ()
popTrace =
  modifyIORef' traceEnv $ \env -> tail env

-- [("choice", [("left", 0), ("right", 1)]), ...]
{-# NOINLINE enumEnv #-}
enumEnv :: IORef (Map.HashMap T.Text (FilePath, [(T.Text, Int)]))
enumEnv =
  unsafePerformIO (newIORef Map.empty)

-- [("left", ("choice", 0)), ("right", ("choice", 1)), ...]
{-# NOINLINE revEnumEnv #-}
revEnumEnv :: IORef (Map.HashMap T.Text (FilePath, T.Text, Int))
revEnumEnv =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE dataEnv #-}
dataEnv :: IORef (Map.HashMap T.Text [T.Text])
dataEnv =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE constructorEnv #-}
constructorEnv :: IORef (Map.HashMap T.Text (Int, Int))
constructorEnv =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE defaultPrefixEnv #-}
defaultPrefixEnv :: IORef [T.Text]
defaultPrefixEnv =
  unsafePerformIO (newIORef [])

{-# NOINLINE prefixEnv #-}
prefixEnv :: IORef [T.Text]
prefixEnv =
  unsafePerformIO (newIORef [])

{-# NOINLINE defaultAliasEnv #-}
defaultAliasEnv :: IORef [(T.Text, T.Text)]
defaultAliasEnv =
  unsafePerformIO (newIORef [])

{-# NOINLINE aliasEnv #-}
aliasEnv :: IORef [(T.Text, T.Text)]
aliasEnv =
  unsafePerformIO (newIORef [])

{-# NOINLINE defaultSectionEnv #-}
defaultSectionEnv :: IORef [T.Text]
defaultSectionEnv =
  unsafePerformIO (newIORef [])

{-# NOINLINE sectionEnv #-}
sectionEnv :: IORef [T.Text]
sectionEnv =
  unsafePerformIO (newIORef [])

{-# NOINLINE topNameEnv #-}
topNameEnv :: IORef (Map.HashMap T.Text FilePath)
topNameEnv =
  unsafePerformIO (newIORef Map.empty)

-- included names
{-# NOINLINE topNameEnvExt #-}
topNameEnvExt :: IORef (Map.HashMap T.Text FilePath)
topNameEnvExt =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE weakTypeEnv #-}
weakTypeEnv :: IORef (IntMap.IntMap WeakTermPlus)
weakTypeEnv =
  unsafePerformIO (newIORef IntMap.empty)

{-# NOINLINE topTypeEnv #-}
topTypeEnv :: IORef (Map.HashMap TopName WeakTermPlus)
topTypeEnv =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE constTypeEnv #-}
constTypeEnv :: IORef (Map.HashMap T.Text TermPlus)
constTypeEnv =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE holeEnv #-}
holeEnv :: IORef (IntMap.IntMap (WeakTermPlus, WeakTermPlus))
holeEnv =
  unsafePerformIO (newIORef IntMap.empty)

{-# NOINLINE constraintEnv #-}
constraintEnv :: IORef [Constraint]
constraintEnv =
  unsafePerformIO (newIORef [])

{-# NOINLINE suspendedConstraintEnv #-}
suspendedConstraintEnv :: IORef SuspendedConstraintQueue
suspendedConstraintEnv =
  unsafePerformIO (newIORef Q.empty)

{-# NOINLINE substEnv #-}
substEnv :: IORef (IntMap.IntMap WeakTermPlus)
substEnv =
  unsafePerformIO (newIORef IntMap.empty)

{-# NOINLINE topDefEnv #-}
topDefEnv :: IORef (Map.HashMap TopName WeakTermPlus)
topDefEnv =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE defEnv #-}
defEnv :: IORef (Map.HashMap T.Text (IsReducible, [Ident], Maybe Comp))
defEnv =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE lowDefEnv #-}
lowDefEnv :: IORef (Map.HashMap T.Text ([Ident], LowComp))
lowDefEnv =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE promiseEnv #-}
promiseEnv :: IORef [Async ()]
promiseEnv =
  unsafePerformIO (newIORef [])

{-# NOINLINE declEnv #-}
declEnv :: IORef (Map.HashMap T.Text ([LowType], LowType))
declEnv =
  unsafePerformIO $
    newIORef $
      Map.fromList
        [ ("malloc", ([voidPtr], voidPtr)),
          ("free", ([voidPtr], voidPtr))
        ]

{-# NOINLINE nopFreeSet #-}
nopFreeSet :: IORef (S.Set Int)
nopFreeSet =
  unsafePerformIO (newIORef S.empty)

--
-- generating new symbols using count
--

{-# INLINE newCount #-}
newCount :: IO Int
newCount =
  atomicModifyIORef' count $ \x -> let z = x + 1 in (z, z) -- for now (i64-overflow breaks this)

{-# INLINE newIdentFromText #-}
newIdentFromText :: T.Text -> IO Ident
newIdentFromText s = do
  i <- newCount
  return $ I (s, i)

{-# INLINE newIdentFromIdent #-}
newIdentFromIdent :: Ident -> IO Ident
newIdentFromIdent x =
  newIdentFromText (asText x)

{-# INLINE newText #-}
newText :: IO T.Text
newText = do
  i <- newCount
  return $ ";" <> T.pack (show i)

{-# INLINE newTextFromText #-}
newTextFromText :: T.Text -> IO T.Text
newTextFromText txt = do
  i <- newCount
  return $ ";" <> txt <> T.pack (show i)

{-# INLINE newAster #-}
newAster :: Hint -> IO WeakTermPlus
newAster m = do
  i <- newCount
  return (m, WeakTermAster i)

{-# INLINE newValueVarLocalWith #-}
newValueVarLocalWith :: T.Text -> IO (Ident, Value)
newValueVarLocalWith name = do
  x <- newIdentFromText name
  return (x, ValueVarLocal x)

--
-- obtain information from the environment
--

getCacheDir :: IO (Path Abs Dir)
getCacheDir = do
  mPathString <- lookupEnv "XDG_CACHE_HOME"
  case mPathString of
    Just pathString ->
      parseAbsDir pathString >>= returnDirectory
    Nothing -> do
      homeDirPath <- getHomeDir
      returnDirectory $ homeDirPath </> $(mkRelDir ".cache")

getBasePath :: IO (Path Abs Dir)
getBasePath = do
  dirPath <- getCacheDir
  returnDirectory $ dirPath </> $(mkRelDir "neut")

getCurrentFilePath :: IO (Path Abs File)
getCurrentFilePath = do
  tenv <- readIORef traceEnv
  return $ head tenv

getCurrentDirPath :: IO (Path Abs Dir)
getCurrentDirPath =
  parent <$> getCurrentFilePath

getLibraryDirPath :: IO (Path Abs Dir)
getLibraryDirPath = do
  basePath <- getBasePath
  returnDirectory $ basePath </> $(mkRelDir "library")

getTargetDependentDirPath :: String -> IO (Path Abs Dir)
getTargetDependentDirPath directoryName = do
  basePath <- getBasePath
  target <- readIORef targetPlatform
  relPath <- parseRelDir $ directoryName <> "/" <> target <> "/" <> showVersion version
  returnDirectory $ basePath </> relPath

getObjectDirPath :: IO (Path Abs Dir)
getObjectDirPath = do
  getTargetDependentDirPath "object"

returnDirectory :: Path Abs Dir -> IO (Path Abs Dir)
returnDirectory path =
  ensureDir path >> return path

--
-- log
--

outputLog :: Log -> IO ()
outputLog (mpos, l, t) = do
  whenRef shouldDisplayLogLocation $
    outputLogLocation mpos
  whenRef shouldDisplayLogLevel $
    outputLogLevel l
  whenRef shouldDisplayLogText $
    outputLogText t (logLevelToPad l)
  whenRef
    shouldDisplayLogFooter
    outputFooter

whenRef :: IORef Bool -> IO () -> IO ()
whenRef ref comp = do
  b <- readIORef ref
  when b comp

outputLogLocation :: Maybe PosInfo -> IO ()
outputLogLocation mpos = do
  b <- readIORef shouldDisplayLogLocation
  case mpos of
    Just (path, loc) | b ->
      withSGR [SetConsoleIntensity BoldIntensity] $ do
        TIO.putStr $ T.pack (showPosInfo path loc)
        TIO.putStrLn ":"
    _ ->
      return ()

outputFooter :: IO ()
outputFooter = do
  eoe <- readIORef endOfEntry
  if eoe == ""
    then return ()
    else putStrLn eoe

outputPosInfo :: PosInfo -> IO ()
outputPosInfo (path, loc) =
  withSGR [SetConsoleIntensity BoldIntensity] $ do
    TIO.putStr $ T.pack (showPosInfo path loc)
    TIO.putStrLn ":"

outputLogLevel :: LogLevel -> IO ()
outputLogLevel l =
  withSGR (logLevelToSGR l) $ do
    TIO.putStr $ logLevelToText l
    TIO.putStr ": "

outputLogText :: T.Text -> IO T.Text -> IO ()
outputLogText str padComp = do
  pad <- padComp
  TIO.putStrLn $ stylizeLogText str pad

logLevelToPad :: LogLevel -> IO T.Text
logLevelToPad level = do
  b <- readIORef shouldDisplayLogLevel
  if b
    then return $ T.replicate (T.length (logLevelToText level) + 2) " "
    else return ""

stylizeLogText :: T.Text -> T.Text -> T.Text
stylizeLogText str pad = do
  let ls = T.lines str
  if null ls
    then str
    else T.intercalate "\n" $ head ls : map (pad <>) (tail ls)

withSGR :: [SGR] -> IO () -> IO ()
withSGR arg f = do
  b <- readIORef shouldColorize
  if b
    then setSGR arg >> f >> setSGR [Reset]
    else f

note :: Hint -> T.Text -> IO ()
note m str =
  outputLog $ logNote (getPosInfo m) str

note' :: T.Text -> IO ()
note' str =
  outputLog $ logNote' str

warn :: PosInfo -> T.Text -> IO ()
warn pos str =
  outputLog $ logWarning pos str

outputPass :: String -> IO ()
outputPass str =
  outputLog (Nothing, LogLevelPass, T.pack str)

outputFail :: String -> IO ()
outputFail str =
  outputLog (Nothing, LogLevelFail, T.pack str)

-- for debug
p :: String -> IO ()
p =
  putStrLn

p' :: (Show a) => a -> IO ()
p' =
  print
