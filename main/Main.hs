module Main (main) where

import qualified Act.Build as Build
import qualified Act.Dependency as Dependency
import qualified Act.Init as Init
import qualified Act.Release as Release
import qualified Act.Version as Version
import qualified Context.Enum.Main as Enum
import qualified Context.Gensym.Main as Gensym
import qualified Context.Global.Main as Global
import qualified Context.LLVM.Main as LLVM
import qualified Context.Locator.Main as Locator
import qualified Context.Log as Log
import qualified Context.Log.IO as Log
import qualified Context.Mode as Mode
import qualified Context.Throw as Throw
import qualified Context.Throw.IO as Throw
import Control.Monad
import qualified Data.Text as T
import Entity.ModuleAlias
import Entity.ModuleURL
import Options.Applicative
import Prelude hiding (log)

data Command
  = Build Build.BuildConfig
  | Check Build.CheckConfig
  | Clean Build.CleanConfig
  | Release Release.Config
  | Get Dependency.GetConfig
  | Tidy Dependency.TidyConfig
  | Init Init.Config
  | ShowVersion Version.Config

main :: IO ()
main =
  execParser (info (helper <*> parseOpt) fullDesc) >>= runCommand

runCommand :: Command -> IO ()
runCommand c = do
  case c of
    Build cfg -> do
      Build.build prodMode cfg
    Check cfg -> do
      Build.check prodMode cfg
    Clean cfg ->
      Build.clean prodMode cfg
    Release cfg ->
      Release.release prodMode cfg
    Init cfg ->
      Init.initialize prodMode cfg
    Get cfg ->
      Dependency.get prodMode cfg
    Tidy cfg ->
      Dependency.tidy prodMode cfg
    ShowVersion cfg ->
      Version.showVersion cfg

cmd :: String -> Parser a -> String -> Mod CommandFields a
cmd name parser desc =
  command name (info (helper <*> parser) (progDesc desc))

parseOpt :: Parser Command
parseOpt = do
  subparser $
    mconcat
      [ cmd "build" parseBuildOpt "build given file",
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
  pure $
    Build
      Build.BuildConfig
        { Build.mTarget = mTarget,
          Build.mClangOptString = mClangOpt,
          Build.buildLogCfg = logCfg,
          Build.buildThrowCfg = throwConfig
        }

parseCleanOpt :: Parser Command
parseCleanOpt = do
  logCfg <- logConfigOpt
  pure $
    Clean
      Build.CleanConfig
        { Build.cleanLogCfg = logCfg,
          Build.cleanThrowCfg = throwConfig
        }

parseGetOpt :: Parser Command
parseGetOpt = do
  moduleAlias <- argument str (mconcat [metavar "ALIAS", help "The alias of the module"])
  moduleURL <- argument str (mconcat [metavar "URL", help "The URL of the archive"])
  logCfg <- logConfigOpt
  pure $
    Get
      Dependency.GetConfig
        { Dependency.moduleAlias = ModuleAlias $ T.pack moduleAlias,
          Dependency.moduleURL = ModuleURL $ T.pack moduleURL,
          Dependency.throwCfg = throwConfig,
          Dependency.logCfg = logCfg
        }

parseTidyOpt :: Parser Command
parseTidyOpt = do
  logCfg <- logConfigOpt
  pure $
    Tidy
      Dependency.TidyConfig
        { Dependency.tidyThrowCfg = throwConfig,
          Dependency.tidyLogCfg = logCfg
        }

parseInitOpt :: Parser Command
parseInitOpt = do
  moduleName <- argument str (mconcat [metavar "MODULE", help "The name of the module"])
  logCfg <- logConfigOpt
  pure $
    Init
      Init.Config
        { Init.moduleName = T.pack moduleName,
          Init.throwCfg = throwConfig,
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
      Build.CheckConfig
        { Build.mFilePathString = inputFilePath,
          Build.checkLogCfg = logCfg,
          Build.checkThrowCfg = throwConfig
        }

logConfigOpt :: Parser Log.Config
logConfigOpt = do
  shouldColorize <- colorizeOpt
  eoe <- endOfEntryOpt
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

parseReleaseOpt :: Parser Command
parseReleaseOpt = do
  releaseName <- argument str (mconcat [metavar "NAME", help "The name of the release"])
  logCfg <- logConfigOpt
  pure $
    Release
      Release.Config
        { Release.getReleaseName = releaseName,
          Release.throwCfg = throwConfig,
          Release.logCfg = logCfg
        }

prodMode :: Mode.Mode
prodMode =
  Mode.Mode
    { Mode.logCtx = Log.new,
      Mode.throwCtx = Throw.new,
      Mode.gensymCtx = Gensym.new,
      Mode.llvmCtx = LLVM.new,
      Mode.enumCtx = Enum.new,
      Mode.globalCtx = Global.new,
      Mode.locatorCtx = Locator.new
    }

throwConfig :: Throw.Config
throwConfig =
  Throw.Config {}
