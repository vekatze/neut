{-# LANGUAGE TemplateHaskell #-}

module Data.Global where

import Control.Comonad.Cofree (Cofree (..))
import Control.Monad (when)
import Data.Basic (AliasInfo, BinderF, Hint, Ident (..), Opacity, PosInfo, asText, getPosInfo, showPosInfo)
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
    logError,
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
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
    mkRelDir,
    (</>),
  )
import Path.IO (XdgDirectory (XdgCache), ensureDir, getXdgDir)
import System.Console.ANSI
  ( ConsoleIntensity (BoldIntensity),
    SGR (Reset, SetConsoleIntensity),
    setSGR,
  )
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Info as System

--
-- global variables
--

{-# NOINLINE countRef #-}
countRef :: IORef Int
countRef =
  unsafePerformIO (newIORef 0)

{-# NOINLINE shouldColorizeRef #-}
shouldColorizeRef :: IORef Bool
shouldColorizeRef =
  unsafePerformIO (newIORef True)

{-# NOINLINE shouldCancelAllocRef #-}
shouldCancelAllocRef :: IORef Bool
shouldCancelAllocRef =
  unsafePerformIO (newIORef True)

{-# NOINLINE mainFilePathRef #-}
mainFilePathRef :: IORef (Maybe (Path Abs File))
mainFilePathRef =
  unsafePerformIO (newIORef Nothing)

setMainFilePath :: Path Abs File -> IO ()
setMainFilePath path =
  modifyIORef' mainFilePathRef $ const $ Just path

getMainFilePath :: IO (Path Abs File)
getMainFilePath = do
  mainFilePathOrNothing <- readIORef mainFilePathRef
  case mainFilePathOrNothing of
    Just mainFilePath ->
      return mainFilePath
    Nothing ->
      raiseCritical' "no main file path is set"

{-# NOINLINE endOfEntryRef #-}
endOfEntryRef :: IORef String
endOfEntryRef =
  unsafePerformIO (newIORef "")

{-# NOINLINE targetPlatformRef #-}
targetPlatformRef :: IORef String
targetPlatformRef =
  unsafePerformIO (newIORef $ System.arch <> "-" <> System.os)

{-# NOINLINE targetOSRef #-}
targetOSRef :: IORef String
targetOSRef =
  unsafePerformIO (newIORef System.os)

{-# NOINLINE targetArchRef #-}
targetArchRef :: IORef String
targetArchRef =
  unsafePerformIO (newIORef System.arch)

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

globalEnumEnv :: [(T.Text, [(T.Text, Int)])]
globalEnumEnv =
  [ (constBottom, []),
    (constTop, [(constTopUnit, 0)]),
    (constBool, [(constBoolFalse, 0), (constBoolTrue, 1)])
  ]

-- [("choice", [("left", 0), ("right", 1)]), ...]
{-# NOINLINE enumEnvRef #-}
enumEnvRef :: IORef (Map.HashMap T.Text [(T.Text, Int)])
enumEnvRef =
  unsafePerformIO $ newIORef Map.empty

-- [("left", ("choice", 0)), ("right", ("choice", 1)), ...]
{-# NOINLINE revEnumEnvRef #-}
revEnumEnvRef :: IORef (Map.HashMap T.Text (T.Text, Int))
revEnumEnvRef =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE dataEnvRef #-}
dataEnvRef :: IORef (Map.HashMap T.Text [T.Text])
dataEnvRef =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE constructorEnvRef #-}
constructorEnvRef :: IORef (Map.HashMap T.Text Int)
constructorEnvRef =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE prefixEnvRef #-}
prefixEnvRef :: IORef [T.Text]
prefixEnvRef =
  unsafePerformIO (newIORef [])

{-# NOINLINE aliasEnvRef #-}
aliasEnvRef :: IORef [(T.Text, T.Text)]
aliasEnvRef =
  unsafePerformIO (newIORef [])

{-# NOINLINE currentSectionRef #-}
currentSectionRef :: IORef T.Text
currentSectionRef =
  unsafePerformIO (newIORef "")

{-# NOINLINE topNameSetRef #-}
topNameSetRef :: IORef (S.Set T.Text)
topNameSetRef =
  unsafePerformIO (newIORef S.empty)

{-# NOINLINE weakTypeEnvRef #-}
weakTypeEnvRef :: IORef (IntMap.IntMap WeakTerm)
weakTypeEnvRef =
  unsafePerformIO (newIORef IntMap.empty)

{-# NOINLINE holeEnvRef #-}
holeEnvRef :: IORef (IntMap.IntMap (WeakTerm, WeakTerm))
holeEnvRef =
  unsafePerformIO (newIORef IntMap.empty)

{-# NOINLINE constraintListRef #-}
constraintListRef :: IORef [Constraint]
constraintListRef =
  unsafePerformIO (newIORef [])

{-# NOINLINE sourceAliasMapRef #-}
sourceAliasMapRef :: IORef (Map.HashMap (Path Abs File) [AliasInfo])
sourceAliasMapRef =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE suspendedConstraintQueueRef #-}
suspendedConstraintQueueRef :: IORef SuspendedConstraintQueue
suspendedConstraintQueueRef =
  unsafePerformIO (newIORef Q.empty)

{-# NOINLINE impArgEnvRef #-}
impArgEnvRef :: IORef (Map.HashMap T.Text Int)
impArgEnvRef =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE substRef #-}
substRef :: IORef (IntMap.IntMap WeakTerm)
substRef =
  unsafePerformIO (newIORef IntMap.empty)

{-# NOINLINE termTypeEnvRef #-}
termTypeEnvRef :: IORef (Map.HashMap T.Text WeakTerm)
termTypeEnvRef =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE termDefEnvRef #-}
termDefEnvRef :: IORef (Map.HashMap T.Text (Opacity, [BinderF WeakTerm], WeakTerm))
termDefEnvRef =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE resourceTypeSetRef #-}
resourceTypeSetRef :: IORef (S.Set T.Text)
resourceTypeSetRef =
  unsafePerformIO $ newIORef S.empty

{-# NOINLINE compDefEnvRef #-}
compDefEnvRef :: IORef (Map.HashMap T.Text (Opacity, [Ident], Comp))
compDefEnvRef =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE lowDefEnvRef #-}
lowDefEnvRef :: IORef (Map.HashMap T.Text ([Ident], LowComp))
lowDefEnvRef =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE lowDeclEnvRef #-}
lowDeclEnvRef :: IORef (Map.HashMap T.Text ([LowType], LowType))
lowDeclEnvRef =
  unsafePerformIO $ newIORef initialLowDeclEnv

{-# NOINLINE lowNameSetRef #-}
lowNameSetRef :: IORef (S.Set T.Text)
lowNameSetRef =
  unsafePerformIO (newIORef S.empty)

{-# NOINLINE nopFreeSetRef #-}
nopFreeSetRef :: IORef (S.Set Int)
nopFreeSetRef =
  unsafePerformIO (newIORef S.empty)

{-# NOINLINE modifiedSourceSetRef #-}
modifiedSourceSetRef :: IORef (S.Set (Path Abs File))
modifiedSourceSetRef =
  unsafePerformIO (newIORef S.empty)

sourceFileExtension :: T.Text
sourceFileExtension =
  "neut"

{-# INLINE nsSepChar #-}
nsSepChar :: Char
nsSepChar =
  '.'

{-# INLINE nsSep #-}
nsSep :: T.Text
nsSep =
  T.singleton nsSepChar

constBottom :: T.Text
constBottom =
  "bottom"

constTop :: T.Text
constTop =
  "top"

constTopUnit :: T.Text
constTopUnit =
  "unit"

{-# INLINE constBool #-}
constBool :: T.Text
constBool =
  "bool"

{-# INLINE constBoolTrue #-}
constBoolTrue :: T.Text
constBoolTrue =
  "true"

{-# INLINE constBoolFalse #-}
constBoolFalse :: T.Text
constBoolFalse =
  "false"

unsafePtr :: T.Text
unsafePtr =
  "unsafe-pointer"

initialLowDeclEnv :: Map.HashMap T.Text ([LowType], LowType)
initialLowDeclEnv =
  Map.fromList
    [ ("malloc", ([voidPtr], voidPtr)),
      ("free", ([voidPtr], voidPtr))
    ]

cartImmName :: T.Text
cartImmName =
  "imm"

cartClsName :: T.Text
cartClsName =
  "cls"

--
-- generating new symbols using count
--

{-# INLINE newCount #-}
newCount :: IO Int
newCount =
  atomicModifyIORef' countRef $ \x -> let z = x + 1 in (z, z)

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

getLibraryDirPath :: IO (Path Abs Dir)
getLibraryDirPath = do
  basePath <- getCacheDirPath
  returnDirectory $ basePath </> $(mkRelDir "library")

returnDirectory :: Path Abs Dir -> IO (Path Abs Dir)
returnDirectory path =
  ensureDir path >> return path

--
-- log
--

outputLog :: Log -> IO ()
outputLog (mpos, l, t) = do
  outputLogLocation mpos
  outputLogLevel l
  outputLogText t (logLevelToPad l)
  outputFooter

whenRef :: IORef Bool -> IO () -> IO ()
whenRef ref comp = do
  b <- readIORef ref
  when b comp

outputLogLocation :: Maybe PosInfo -> IO ()
outputLogLocation mpos = do
  case mpos of
    Just (path, loc) ->
      withSGR [SetConsoleIntensity BoldIntensity] $ do
        TIO.putStr $ T.pack (showPosInfo path loc)
        TIO.putStrLn ":"
    _ ->
      return ()

outputFooter :: IO ()
outputFooter = do
  eoe <- readIORef endOfEntryRef
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
  return $ T.replicate (T.length (logLevelToText level) + 2) " "

stylizeLogText :: T.Text -> T.Text -> T.Text
stylizeLogText str pad = do
  let ls = T.lines str
  if null ls
    then str
    else T.intercalate "\n" $ head ls : map (pad <>) (tail ls)

withSGR :: [SGR] -> IO () -> IO ()
withSGR arg f = do
  b <- readIORef shouldColorizeRef
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

outputError :: Hint -> T.Text -> IO a
outputError m text = do
  outputLog $ logError (getPosInfo m) text
  exitWith (ExitFailure 1)

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
