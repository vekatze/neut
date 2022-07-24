module Main (main) where

import qualified Act.Build as Build
import qualified Act.Check as Check
import qualified Act.Clean as Clean
import qualified Act.Get as Get
import qualified Act.Init as Init
import qualified Act.Release as Release
import qualified Act.Tidy as Tidy
import qualified Act.Version as Version
import qualified Context.Alias.Main as Alias
import qualified Context.CompDefinition.Main as CompDefinition
import qualified Context.Definition.Main as Definition
import qualified Context.Gensym.Main as Gensym
import qualified Context.Global.Main as Global
import qualified Context.Implicit.Main as Implicit
import qualified Context.LLVM.Main as LLVM
import qualified Context.Locator.Main as Locator
import qualified Context.Log as Log
import qualified Context.Log.IO as Log
import qualified Context.Mode as Mode
import qualified Context.Module.Main as Module
import qualified Context.Path.Main as Path
import qualified Context.Throw as Throw
import qualified Context.Throw.IO as Throw
import qualified Context.Type.Main as Type
import qualified Data.Text as T
import Entity.ModuleURL
import Entity.Target
import Options.Applicative

data Command
  = Build Build.Config
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
      Build.build prodMode cfg
    Check cfg -> do
      Check.check prodMode cfg
    Clean cfg ->
      Clean.clean prodMode cfg
    Release cfg ->
      Release.release prodMode cfg
    Init cfg ->
      Init.initialize prodMode cfg
    Get cfg ->
      Get.get prodMode cfg
    Tidy cfg ->
      Tidy.tidy prodMode cfg
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
  shouldCancelAlloc <- cancelAllocOpt
  pure $
    Build
      Build.Config
        { Build.mTarget = Target <$> mTarget,
          Build.mClangOptString = mClangOpt,
          Build.logCfg = logCfg,
          Build.throwCfg = throwConfig,
          Build.shouldCancelAlloc = shouldCancelAlloc
        }

parseCleanOpt :: Parser Command
parseCleanOpt = do
  logCfg <- logConfigOpt
  pure $
    Clean
      Clean.Config
        { Clean.logCfg = logCfg,
          Clean.throwCfg = throwConfig
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
          Get.throwCfg = throwConfig,
          Get.logCfg = logCfg
        }

parseTidyOpt :: Parser Command
parseTidyOpt = do
  logCfg <- logConfigOpt
  pure $
    Tidy
      Tidy.Config
        { Tidy.throwCfg = throwConfig,
          Tidy.logCfg = logCfg
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
      Check.Config
        { Check.mFilePathString = inputFilePath,
          Check.logCfg = logCfg,
          Check.throwCfg = throwConfig
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
      Mode.aliasCtx = Alias.new,
      Mode.globalCtx = Global.new,
      Mode.locatorCtx = Locator.new,
      Mode.pathCtx = Path.new,
      Mode.moduleCtx = Module.new,
      Mode.typeCtx = Type.new,
      Mode.implicitCtx = Implicit.new,
      Mode.definitionCtx = Definition.new,
      Mode.compDefinitionCtx = CompDefinition.new
    }

throwConfig :: Throw.Config
throwConfig =
  Throw.Config {}
