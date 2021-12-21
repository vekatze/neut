{-# LANGUAGE TemplateHaskell #-}

module Data.Global where

import Control.Comonad.Cofree (Cofree (..))
import Control.Monad (when)
import Data.Basic
  ( Hint,
    Ident (..),
    IsReducible,
    PosInfo,
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
    raiseCritical',
  )
import Data.LowComp (LowComp)
import Data.LowType (LowType, voidPtr)
import qualified Data.PQueue.Min as Q
import qualified Data.Set as S
import Data.Term (Term)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import Data.WeakTerm
  ( Constraint,
    SuspendedConstraintQueue,
    WeakTerm,
    WeakTermF (WeakTermAster),
  )
import Path
  ( Abs,
    Dir,
    File,
    Path,
    Rel,
    mkAbsDir,
    mkRelDir,
    parent,
    parseRelDir,
    (</>),
  )
import Path.IO (XdgDirectory (XdgCache), ensureDir, getXdgDir)
import Paths_neut (version)
import System.Console.ANSI
  ( ConsoleIntensity (BoldIntensity),
    SGR (Reset, SetConsoleIntensity),
    setSGR,
  )
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Info as System

data VisitInfo
  = VisitInfoActive
  | VisitInfoFinish

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

{-# NOINLINE mainModuleDirRef #-}
mainModuleDirRef :: IORef (Path Abs Dir)
mainModuleDirRef =
  unsafePerformIO (newIORef $(mkAbsDir "/"))

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

{-# NOINLINE currentFileRef #-}
currentFileRef :: IORef (Maybe (Path Abs File))
currentFileRef =
  unsafePerformIO (newIORef Nothing)

setCurrentFilePath :: Path Abs File -> IO ()
setCurrentFilePath path =
  modifyIORef' currentFileRef $ const $ Just path

getCurrentFilePath :: IO (Path Abs File)
getCurrentFilePath = do
  currentFileOrNothing <- readIORef currentFileRef
  case currentFileOrNothing of
    Just currentFile ->
      return currentFile
    Nothing ->
      raiseCritical' "no current file is set"

-- [("choice", [("left", 0), ("right", 1)]), ...]
{-# NOINLINE enumEnv #-}
enumEnv :: IORef (Map.HashMap T.Text [(T.Text, Int)])
enumEnv =
  unsafePerformIO (newIORef Map.empty)

-- [("left", ("choice", 0)), ("right", ("choice", 1)), ...]
{-# NOINLINE revEnumEnv #-}
revEnumEnv :: IORef (Map.HashMap T.Text (T.Text, Int))
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

initialPrefixEnv :: [T.Text]
initialPrefixEnv =
  ["this"]

{-# NOINLINE prefixEnv #-}
prefixEnv :: IORef [T.Text]
prefixEnv =
  unsafePerformIO (newIORef initialPrefixEnv)

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
topNameEnv :: IORef (S.Set T.Text)
topNameEnv =
  unsafePerformIO (newIORef S.empty)

{-# NOINLINE weakTypeEnv #-}
weakTypeEnv :: IORef (IntMap.IntMap WeakTerm)
weakTypeEnv =
  unsafePerformIO (newIORef IntMap.empty)

{-# NOINLINE topTypeEnv #-}
topTypeEnv :: IORef (Map.HashMap T.Text WeakTerm)
topTypeEnv =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE constTypeEnv #-}
constTypeEnv :: IORef (Map.HashMap T.Text Term)
constTypeEnv =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE holeEnv #-}
holeEnv :: IORef (IntMap.IntMap (WeakTerm, WeakTerm))
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
substEnv :: IORef (IntMap.IntMap WeakTerm)
substEnv =
  unsafePerformIO (newIORef IntMap.empty)

{-# NOINLINE topDefEnv #-}
topDefEnv :: IORef (Map.HashMap T.Text WeakTerm)
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

moduleFileName :: FilePath
moduleFileName =
  "module.ens"

sourceFileExtension :: T.Text
sourceFileExtension =
  "neut"

defaultModulePrefix :: T.Text
defaultModulePrefix =
  "this"

{-# INLINE nsSepChar #-}
nsSepChar :: Char
nsSepChar =
  '.'

{-# INLINE nsSep #-}
nsSep :: T.Text
nsSep =
  T.singleton nsSepChar

{-# INLINE boolTrue #-}
boolTrue :: T.Text
boolTrue =
  "bool" <> nsSep <> "true"

{-# INLINE boolFalse #-}
boolFalse :: T.Text
boolFalse =
  "bool" <> nsSep <> "false"

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
newAster :: Hint -> IO WeakTerm
newAster m = do
  i <- newCount
  return $ m :< WeakTermAster i

{-# INLINE newValueVarLocalWith #-}
newValueVarLocalWith :: T.Text -> IO (Ident, Value)
newValueVarLocalWith name = do
  x <- newIdentFromText name
  return (x, ValueVarLocal x)

--
-- obtain information from the environment
--

getCacheDirPath :: IO (Path Abs Dir)
getCacheDirPath = do
  getXdgDir XdgCache (Just $(mkRelDir "neut")) >>= returnDirectory

getCurrentDirPath :: IO (Path Abs Dir)
getCurrentDirPath =
  parent <$> getCurrentFilePath

getLibraryDirPath :: IO (Path Abs Dir)
getLibraryDirPath = do
  basePath <- getCacheDirPath
  returnDirectory $ basePath </> $(mkRelDir "library")

getTargetDependentDirPath :: String -> IO (Path Abs Dir)
getTargetDependentDirPath directoryName = do
  basePath <- getCacheDirPath
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
