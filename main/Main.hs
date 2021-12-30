module Main (main) where

import Command.Build (build, check, clean)
import Command.Dependency (get, tidy)
import Command.Init (initialize)
import Command.Release (release)
import Control.Exception.Safe (try)
import Control.Monad (void)
import Data.Basic (Alias, URL (..))
import Data.Global
  ( endOfEntryRef,
    outputLog,
    shouldColorizeRef,
  )
import Data.IORef (writeIORef)
import Data.Log (Error (Error))
import qualified Data.Text as T
import Data.Version (showVersion)
import Options.Applicative
  ( Parser,
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
    optional,
    progDesc,
    str,
    strOption,
    subparser,
    value,
  )
import Parse.Module (initializeMainModule)
import Paths_neut (version)
import System.Exit (ExitCode (ExitFailure), exitWith)

type Target =
  String

type InputPath =
  String

type CheckOptEndOfEntry =
  String

type ClangOption =
  String

data Command
  = Build (Maybe Target) (Maybe ClangOption)
  | Clean
  | Check (Maybe InputPath) Bool CheckOptEndOfEntry
  | Release T.Text
  | Get Alias URL
  | Tidy
  | Init T.Text
  | Version

main :: IO ()
main =
  execParser (info (helper <*> parseOpt) fullDesc) >>= runCommand

parseOpt :: Parser Command
parseOpt =
  subparser $
    mconcat
      [ command
          "build"
          ( info
              (helper <*> parseBuildOpt)
              (progDesc "build given file")
          ),
        command
          "clean"
          ( info
              (helper <*> parseCleanOpt)
              (progDesc "remove the resulting files")
          ),
        command
          "check"
          ( info
              (helper <*> parseCheckOpt)
              (progDesc "check specified file")
          ),
        command
          "release"
          ( info
              (helper <*> parseReleaseOpt)
              (progDesc "create a release from a given path")
          ),
        command
          "init"
          ( info
              (helper <*> parseInitOpt)
              (progDesc "create a new module")
          ),
        command
          "get"
          ( info
              (helper <*> parseGetOpt)
              (progDesc "get a module")
          ),
        command
          "tidy"
          ( info
              (helper <*> parseTidyOpt)
              (progDesc "tidy the module dependency")
          ),
        command
          "version"
          ( info
              (helper <*> parseVersionOpt)
              (progDesc "show version info")
          )
      ]

parseBuildOpt :: Parser Command
parseBuildOpt =
  Build
    <$> optional
      ( argument
          str
          ( mconcat
              [ metavar "TARGET",
                help "The build target"
              ]
          )
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

parseCleanOpt :: Parser Command
parseCleanOpt =
  pure Clean

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

parseCheckOpt :: Parser Command
parseCheckOpt =
  Check
    <$> optional
      ( argument
          str
          ( mconcat
              [ metavar "INPUT",
                help "The path of input file"
              ]
          )
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

parseReleaseOpt :: Parser Command
parseReleaseOpt =
  Release
    <$> ( T.pack
            <$> argument
              str
              ( mconcat [metavar "IDENTIFIER", help "The name of the release"]
              )
        )

runCommand :: Command -> IO ()
runCommand cmd = do
  case cmd of
    Build target mClangOptStr -> do
      runAction $ initializeMainModule >> build target mClangOptStr
    Check mInputPathStr colorizeFlag eoe -> do
      writeIORef shouldColorizeRef colorizeFlag
      writeIORef endOfEntryRef eoe
      void $ runAction $ initializeMainModule >> check mInputPathStr
    Clean -> do
      runAction $ initializeMainModule >> clean
    Release identifier -> do
      runAction $ initializeMainModule >> release identifier
    Init moduleName ->
      runAction $ initialize moduleName
    Get alias url -> do
      runAction $ initializeMainModule >> get alias url
    Tidy -> do
      runAction $ initializeMainModule >> tidy
    Version ->
      putStrLn $ showVersion version

runAction :: IO a -> IO a
runAction c = do
  resultOrErr <- try c
  case resultOrErr of
    Left (Error err) ->
      foldr ((>>) . outputLog) (exitWith (ExitFailure 1)) err
    Right result ->
      return result
