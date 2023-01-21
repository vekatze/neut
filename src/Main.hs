module Main (main) where

import Act.Build qualified as Build
import Act.Check qualified as Check
import Act.Clean qualified as Clean
import Act.Get qualified as Get
import Act.Init qualified as Init
import Act.Release qualified as Release
import Act.Run qualified as Run
import Act.Tidy qualified as Tidy
import Act.Version qualified as Version
import Case.Main qualified as Main
import Context.Log qualified as Log
import Data.Text qualified as T
import Entity.ModuleURL
import Entity.Target
import Options.Applicative

data Command
  = Build Build.Config
  | Run Run.Config
  | Check Check.Config
  | Clean Clean.Config
  | Release Release.Config
  | Get Get.Config
  | Tidy Tidy.Config
  | Init Init.Config
  | ShowVersion Version.Config

main :: IO ()
main = do
  c <- execParser (info (helper <*> parseOpt) fullDesc)
  case c of
    Build cfg -> do
      Main.build cfg
    Run cfg -> do
      Main.run cfg
    Check cfg -> do
      Main.check cfg
    Clean cfg ->
      Main.clean cfg
    Release cfg ->
      Main.release cfg
    Init cfg ->
      Main.initialize cfg
    Get cfg ->
      Main.get cfg
    Tidy cfg ->
      Main.tidy cfg
    ShowVersion cfg ->
      Main.version cfg

cmd :: String -> Parser a -> String -> Mod CommandFields a
cmd name parser desc =
  command name (info (helper <*> parser) (progDesc desc))

parseOpt :: Parser Command
parseOpt = do
  subparser $
    mconcat
      [ cmd "build" parseBuildOpt "build given target",
        cmd "run" parseRunOpt "build and run given target",
        cmd "clean" parseCleanOpt "remove the resulting files",
        cmd "check" parseCheckOpt "type-check specified file",
        cmd "release" parseReleaseOpt "create a release tar from a given path",
        cmd "init" parseInitOpt "create a new module",
        cmd "get" parseGetOpt "get a release tar",
        cmd "tidy" parseTidyOpt "tidy the module dependency",
        cmd "version" parseVersionOpt "show version info"
      ]

parseBuildOpt :: Parser Command
parseBuildOpt = do
  mTarget <- optional $ argument str $ mconcat [metavar "TARGET", help "The build target"]
  mClangOpt <- optional $ strOption $ mconcat [long "clang-option", metavar "OPT", help "Options for clang"]
  logCfg <- logConfigOpt
  shouldCancelAlloc <- cancelAllocOpt
  pure $
    Build
      Build.Config
        { Build.mTarget = Target <$> mTarget,
          Build.mClangOptString = mClangOpt,
          Build.logCfg = logCfg,
          Build.shouldCancelAlloc = shouldCancelAlloc
        }

parseRunOpt :: Parser Command
parseRunOpt = do
  target <- argument str $ mconcat [metavar "TARGET", help "The build target"]
  mClangOpt <- optional $ strOption $ mconcat [long "clang-option", metavar "OPT", help "Options for clang"]
  logCfg <- logConfigOpt
  shouldCancelAlloc <- cancelAllocOpt
  pure $
    Run
      Run.Config
        { Run.target = Target target,
          Run.mClangOptString = mClangOpt,
          Run.logCfg = logCfg,
          Run.shouldCancelAlloc = shouldCancelAlloc
        }

parseCleanOpt :: Parser Command
parseCleanOpt = do
  logCfg <- logConfigOpt
  pure $
    Clean
      Clean.Config
        { Clean.logCfg = logCfg
        }

parseGetOpt :: Parser Command
parseGetOpt = do
  moduleAlias <- argument str (mconcat [metavar "ALIAS", help "The alias of the module"])
  moduleURL <- argument str (mconcat [metavar "URL", help "The URL of the archive"])
  logCfg <- logConfigOpt
  pure $
    Get
      Get.Config
        { Get.moduleAliasText = T.pack moduleAlias,
          Get.moduleURL = ModuleURL $ T.pack moduleURL,
          Get.logCfg = logCfg
        }

parseTidyOpt :: Parser Command
parseTidyOpt = do
  logCfg <- logConfigOpt
  pure $
    Tidy
      Tidy.Config
        { Tidy.logCfg = logCfg
        }

parseInitOpt :: Parser Command
parseInitOpt = do
  moduleName <- argument str (mconcat [metavar "MODULE", help "The name of the module"])
  logCfg <- logConfigOpt
  pure $
    Init
      Init.Config
        { Init.moduleName = T.pack moduleName,
          Init.logCfg = logCfg
        }

parseVersionOpt :: Parser Command
parseVersionOpt =
  pure $ ShowVersion Version.Config {}

parseCheckOpt :: Parser Command
parseCheckOpt = do
  inputFilePath <- optional $ argument str (mconcat [metavar "INPUT", help "The path of input file"])
  logCfg <- logConfigOpt
  pure $
    Check
      Check.Config
        { Check.mFilePathString = inputFilePath,
          Check.logCfg = logCfg
        }

logConfigOpt :: Parser Log.Config
logConfigOpt = do
  shouldColorize <- colorizeOpt
  eoe <- T.pack <$> endOfEntryOpt
  pure
    Log.Config
      { Log.shouldColorize = shouldColorize,
        Log.endOfEntry = eoe
      }

endOfEntryOpt :: Parser String
endOfEntryOpt =
  strOption (mconcat [long "end-of-entry", value "", help "String printed after each entry", metavar "STRING"])

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

cancelAllocOpt :: Parser Bool
cancelAllocOpt =
  flag
    True
    False
    ( mconcat
        [ long "no-cancel-alloc",
          help "Set this to disable cancelling malloc/free"
        ]
    )

parseReleaseOpt :: Parser Command
parseReleaseOpt = do
  releaseName <- argument str (mconcat [metavar "NAME", help "The name of the release"])
  logCfg <- logConfigOpt
  pure $
    Release
      Release.Config
        { Release.getReleaseName = releaseName,
          Release.logCfg = logCfg
        }
