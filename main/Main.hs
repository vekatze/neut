module Main (main) where

import Act.Build
import Act.Dependency
import Act.Init
import Act.Release
import Context.App
import qualified Context.App.Main as App
import Data.Function
import qualified Context.Log as Log
import qualified Context.Log.IO as Log
import qualified Context.Throw as Throw
import qualified Context.Throw.IO as Throw
import Control.Monad
import qualified Data.Text as T
import Data.Version
import Entity.Log
import Entity.Module.Reflect
import Entity.ModuleAlias
import Entity.ModuleURL
import Options.Applicative
import Paths_neut
import System.Exit
import Prelude hiding (log)

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
  | Get ModuleAlias ModuleURL
  | Tidy
  | Init T.Text
  | ShowVersion

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
    <$> ( ModuleAlias . T.pack
            <$> argument
              str
              ( mconcat
                  [ metavar "ALIAS",
                    help "The alias of the module"
                  ]
              )
        )
    <*> ( ModuleURL . T.pack
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
  pure ShowVersion

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
  axis <-
    App.new
      Log.Config
        { Log.shouldColorize = True,
          Log.endOfEntry = ""
        }
      Throw.Config
        {
        }
  case cmd of
    Build target mClangOptStr -> do
      runAction axis $ initializeMainModule (axis & throw) >> build axis target mClangOptStr
    Check mInputPathStr colorizeFlag eoe -> do
      checkAxis <-
        App.new
          Log.Config
            { Log.shouldColorize = colorizeFlag,
              Log.endOfEntry = eoe
            }
          Throw.Config
            {
            }
      void $ runAction checkAxis $ initializeMainModule (checkAxis & throw) >> check checkAxis mInputPathStr
    Clean -> do
      runAction axis $ initializeMainModule (axis & throw) >> clean axis
    Release identifier -> do
      runAction axis $ initializeMainModule (axis & throw) >> release axis identifier
    Init moduleName ->
      runAction axis $ initialize axis moduleName
    Get alias url -> do
      runAction axis $ initializeMainModule (axis & throw) >> get axis alias url
    Tidy -> do
      runAction axis $ initializeMainModule (axis & throw) >> tidy axis
    ShowVersion ->
      putStrLn $ showVersion version

runAction :: Axis -> IO a -> IO a
runAction axis c = do
  resultOrErr <- Throw.try (axis & throw) c
  case resultOrErr of
    Left (Error err) ->
      foldr ((>>) . (axis & log & Log.printLog)) (exitWith (ExitFailure 1)) err
    Right result ->
      return result
