module Main (main) where

import Clarify (clarify)
import Command.Get (get, tidy)
import Command.Init (initialize)
import Control.Concurrent.Async (wait)
import Control.Exception.Safe (try)
import Control.Monad (forM_, void, (>=>))
import Data.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Lazy as L
import Data.Global
  ( endOfEntry,
    outputLog,
    promiseEnv,
    shouldColorize,
  )
import Data.IORef (readIORef, writeIORef)
import Data.Log (Error (Error), raiseError')
import Data.Maybe (fromMaybe)
import Data.Module (Alias)
import Data.Spec (Spec, URL (URL), getEntryPoint, getTargetDir)
import qualified Data.Text as T
import Data.Version (showVersion)
import Elaborate (elaborate)
import Emit (emit)
import GHC.IO.Handle (hClose)
import Lower (lower)
import Options.Applicative
  ( Alternative (many),
    Parser,
    ReadM,
    argument,
    command,
    execParser,
    flag,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    optional,
    progDesc,
    readerError,
    short,
    str,
    strOption,
    subparser,
    value,
  )
import Parse (parse)
import Parse.Section (getSpecForBuildDirectory)
import Path
  ( Abs,
    Dir,
    File,
    Path,
    Rel,
    addExtension,
    dirname,
    filename,
    fromRelDir,
    parent,
    parseRelFile,
    splitExtension,
    stripProperPrefix,
    toFilePath,
    (</>),
  )
import Path.IO (getCurrentDir, resolveDir', resolveFile')
import Paths_neut (version)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.Process
  ( CreateProcess (std_in),
    StdStream (CreatePipe),
    createProcess,
    proc,
    waitForProcess,
    withCreateProcess,
  )
import Text.Read (readMaybe)

type Target =
  String

type InputPath =
  String

type MainInputPath =
  String

type AuxInputPath =
  String

type OutputPath =
  String

type CheckOptEndOfEntry =
  String

type ShouldColorize =
  Bool

type ClangOption =
  String

data OutputKind
  = OutputKindObject
  | OutputKindLLVM
  | OutputKindLibrary
  | OutputKindAsm
  deriving (Show)

instance Read OutputKind where
  readsPrec _ "object" =
    [(OutputKindObject, [])]
  readsPrec _ "llvm" =
    [(OutputKindLLVM, [])]
  readsPrec _ "asm" =
    [(OutputKindAsm, [])]
  readsPrec _ _ =
    []

-- type Alias =
--   String

-- type URL =
--   String

data Command
  = Build
      Target
      OutputKind
      ShouldColorize
      (Maybe ClangOption)
  | Link MainInputPath [AuxInputPath] (Maybe OutputPath) (Maybe ClangOption)
  | Check InputPath ShouldColorize CheckOptEndOfEntry
  | Archive InputPath (Maybe OutputPath)
  | Get Alias URL
  | Tidy
  | Init T.Text
  | Version

main :: IO ()
main =
  execParser (info (helper <*> parseOpt) fullDesc) >>= runCommand

parseOpt :: Parser Command
parseOpt =
  subparser
    ( command
        "build"
        (info (helper <*> parseBuildOpt) (progDesc "build given file"))
        <> command
          "link"
          (info (helper <*> parseLinkOpt) (progDesc "link given files"))
        <> command
          "check"
          (info (helper <*> parseCheckOpt) (progDesc "check specified file"))
        <> command
          "archive"
          ( info
              (helper <*> parseArchiveOpt)
              (progDesc "create archive from given path")
          )
        <> command
          "init"
          ( info
              (helper <*> parseInitOpt)
              (progDesc "create a new module")
          )
        <> command
          "get"
          ( info
              (helper <*> parseGetOpt)
              (progDesc "get a package")
          )
        <> command
          "tidy"
          ( info
              (helper <*> parseTidyOpt)
              (progDesc "tidy the module dependency")
          )
        <> command
          "version"
          ( info
              (helper <*> parseVersionOpt)
              (progDesc "show version info")
          )
    )

parseBuildOpt :: Parser Command
parseBuildOpt =
  Build
    <$> argument
      str
      ( mconcat
          [ metavar "TARGET",
            help "The build target"
          ]
      )
      <*> option
        kindReader
        ( mconcat
            [ long "emit",
              metavar "KIND",
              value OutputKindLibrary,
              help "The type of output file"
            ]
        )
      <*> colorizeOpt
      <*> optional
        ( strOption
            ( mconcat
                [ long "clang-option",
                  metavar "OPT",
                  help "option string to be passed to clang"
                ]
            )
        )

parseLinkOpt :: Parser Command
parseLinkOpt = do
  Link
    <$> argument
      str
      ( mconcat
          [ metavar "INPUT",
            help "The main input file"
          ]
      )
      <*> many (argument str (metavar "AUX_INPUT"))
      <*> optional
        ( strOption $
            mconcat
              [ long "output",
                short 'o',
                metavar "OUTPUT",
                help "The path of output file"
              ]
        )
      <*> optional
        ( strOption
            ( mconcat
                [ long "clang-option",
                  metavar "OPT",
                  help "option string to be passed to clang"
                ]
            )
        )

parseGetOpt :: Parser Command
parseGetOpt =
  Get
    <$> ( T.pack
            <$> argument
              str
              ( mconcat
                  [ metavar "ALIAS",
                    help "The alias of the module"
                  ]
              )
        )
      <*> ( URL . T.pack
              <$> argument
                str
                ( mconcat
                    [ metavar "URL",
                      help "The URL of the archive"
                    ]
                )
          )

parseTidyOpt :: Parser Command
parseTidyOpt =
  pure Tidy

parseInitOpt :: Parser Command
parseInitOpt =
  Init
    <$> ( T.pack
            <$> argument
              str
              ( mconcat
                  [ metavar "MODULE",
                    help "The name of the module"
                  ]
              )
        )

parseVersionOpt :: Parser Command
parseVersionOpt =
  pure Version

kindReader :: ReadM OutputKind
kindReader = do
  s <- str
  case readMaybe s of
    Nothing ->
      readerError $ "unknown mode:" ++ s
    Just m ->
      return m

parseCheckOpt :: Parser Command
parseCheckOpt =
  Check
    <$> argument
      str
      ( mconcat
          [ metavar "INPUT",
            help "The path of input file"
          ]
      )
      <*> colorizeOpt
      <*> strOption
        ( mconcat
            [ long "end-of-entry",
              value "",
              help "String printed after each entry",
              metavar "STRING"
            ]
        )

colorizeOpt :: Parser Bool
colorizeOpt =
  flag
    True
    False
    ( mconcat
        [ long "no-color",
          help "Set this to disable colorization of the output"
        ]
    )

parseArchiveOpt :: Parser Command
parseArchiveOpt =
  Archive
    <$> argument
      str
      ( mconcat [metavar "INPUT", help "The path of input directory"]
      )
    <*> optional
      ( strOption $
          mconcat
            [ long "output",
              short 'o',
              metavar "OUTPUT",
              help "The path of output"
            ]
      )

runCommand :: Command -> IO ()
runCommand cmd =
  case cmd of
    Build target outputKind colorizeFlag mClangOptStr -> do
      writeIORef shouldColorize colorizeFlag
      mainSpec <- runAction getSpecForBuildDirectory
      entryFilePath <- runAction $ resolveTarget target mainSpec
      outputPath <- getOutputPath target mainSpec
      llvmIRBuilder <- runAction (build entryFilePath)
      let llvmIR = toLazyByteString llvmIRBuilder
      case outputKind of
        OutputKindLLVM -> do
          L.writeFile (toFilePath outputPath) llvmIR
          waitAll
        _ -> do
          let additionalClangOptions = words $ fromMaybe "" mClangOptStr
          let clangCmd = proc "clang" $ clangOptWith outputKind outputPath ++ additionalClangOptions ++ ["-c"]
          withCreateProcess clangCmd {std_in = CreatePipe} $ \(Just stdin) _ _ clangProcessHandler -> do
            L.hPut stdin llvmIR
            hClose stdin
            exitCode <- waitForProcess clangProcessHandler
            waitAll
            exitWith exitCode
    Link mainFilePath auxFilePathList mOutputPathStr mClangOptStr -> do
      inputPath <- resolveFile' mainFilePath
      (basename, _) <- splitExtension $ filename inputPath
      mOutputPath <- mapM resolveFile' mOutputPathStr
      outputPath <- constructOutputPath basename mOutputPath OutputKindObject
      let additionalClangOptions = words $ fromMaybe "" mClangOptStr
      let clangCmd = proc "clang" $ clangLibOpt outputPath ++ additionalClangOptions ++ (mainFilePath : auxFilePathList)
      withCreateProcess clangCmd $ \_ _ _ clangProcessHandler -> do
        exitCode <- waitForProcess clangProcessHandler
        waitAll
        exitWith exitCode
    Check inputPathStr colorizeFlag eoe -> do
      writeIORef shouldColorize colorizeFlag
      writeIORef endOfEntry eoe
      inputPath <- resolveFile' inputPathStr
      void $ runAction (runCheck inputPath)
      waitAll
    Archive inputPathStr mOutputPathStr -> do
      libDirPath <- resolveDir' inputPathStr
      let parentDirPath = parent libDirPath
      basename <- stripProperPrefix parentDirPath libDirPath
      mOutputPath <- mapM resolveFile' mOutputPathStr
      outputPath <- toFilePath <$> constructOutputArchivePath libDirPath mOutputPath
      let tarCmd = proc "tar" ["cJf", outputPath, "-C", toFilePath parentDirPath, toFilePath basename]
      (_, _, _, handler) <- createProcess tarCmd
      tarExitCode <- waitForProcess handler
      waitAll
      exitWith tarExitCode
    Init moduleName ->
      runAction $ initialize moduleName
    Get alias url ->
      runAction $ get alias url
    Tidy ->
      runAction tidy
    Version ->
      putStrLn $ showVersion version

constructOutputPath :: Path Rel File -> Maybe (Path Abs File) -> OutputKind -> IO (Path Abs File)
constructOutputPath basename mPath kind =
  case mPath of
    Just path ->
      return path
    Nothing ->
      case kind of
        OutputKindLLVM -> do
          dir <- getCurrentDir
          addExtension ".ll" (dir </> basename)
        OutputKindAsm -> do
          dir <- getCurrentDir
          addExtension ".s" (dir </> basename)
        OutputKindLibrary -> do
          dir <- getCurrentDir
          addExtension ".o" (dir </> basename)
        OutputKindObject -> do
          dir <- getCurrentDir
          return $ dir </> basename

constructOutputArchivePath :: Path Abs Dir -> Maybe (Path Abs File) -> IO (Path Abs File)
constructOutputArchivePath inputPath mPath =
  case mPath of
    Just path ->
      return path
    Nothing ->
      resolveFile' (fromRelDir $ dirname inputPath) >>= addExtension ".tar" >>= addExtension ".gz"

resolveTarget :: Target -> Spec -> IO (Path Abs File)
resolveTarget target mainSpec = do
  -- case M.lookup (T.pack target) (specEntryPoint mainSpec) of
  case getEntryPoint mainSpec (T.pack target) of
    Just path ->
      return path
    Nothing -> do
      l <- raiseError' $ "no such target is defined: `" <> T.pack target <> "`"
      outputLog l
      exitWith (ExitFailure 1)

getOutputPath :: Target -> Spec -> IO (Path Abs File)
getOutputPath target mainSpec = do
  targetFile <- parseRelFile target
  let targetDir = getTargetDir mainSpec
  return $ targetDir </> targetFile

build :: Path Abs File -> IO Builder
build =
  parse >=> elaborate >=> clarify >=> lower >=> emit

runCheck :: Path Abs File -> IO ()
runCheck =
  parse >=> elaborate >=> \_ -> return ()

clangOptWith :: OutputKind -> Path Abs File -> [String]
clangOptWith kind outputPath =
  case kind of
    OutputKindAsm ->
      "-S" : clangBaseOpt outputPath
    _ ->
      clangBaseOpt outputPath

clangBaseOpt :: Path Abs File -> [String]
clangBaseOpt outputPath =
  [ "-xir",
    "-Wno-override-module",
    "-O2",
    "-",
    "-o",
    toFilePath outputPath
  ]

clangLibOpt :: Path Abs File -> [String]
clangLibOpt outputPath =
  [ "-Wno-override-module",
    "-O2",
    "-o",
    toFilePath outputPath
  ]

runAction :: IO a -> IO a
runAction c = do
  resultOrErr <- try c
  case resultOrErr of
    Left (Error err) ->
      foldr ((>>) . outputLog) (exitWith (ExitFailure 1)) err
    Right result ->
      return result

waitAll :: IO ()
waitAll = do
  ps <- readIORef promiseEnv
  forM_ ps $ \promise -> wait promise
