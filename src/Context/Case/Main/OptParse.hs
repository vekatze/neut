module Context.Case.Main.OptParse
  ( parseCommand,
  )
where

import Control.Monad.IO.Class
import Data.Text qualified as T
import Entity.Command
import Entity.Config.Build qualified as Build
import Entity.Config.Check qualified as Check
import Entity.Config.Clean qualified as Clean
import Entity.Config.Create qualified as Create
import Entity.Config.Get qualified as Get
import Entity.Config.Log qualified as Log
import Entity.Config.Release qualified as Release
import Entity.Config.Tidy qualified as Tidy
import Entity.Config.Version qualified as Version
import Entity.ModuleURL
import Entity.OutputKind qualified as OK
import Entity.Target
import Options.Applicative

parseCommand :: MonadIO m => m Command
parseCommand =
  liftIO $ execParser (info (helper <*> parseOpt) fullDesc)

parseOpt :: Parser Command
parseOpt = do
  subparser $
    mconcat
      [ cmd "build" parseBuildOpt "build given target",
        cmd "clean" parseCleanOpt "remove the resulting files",
        cmd "check" parseCheckOpt "type-check specified file",
        cmd "release" parseReleaseOpt "create a release tar from a given path",
        cmd "create" parseCreateOpt "create a new module",
        cmd "get" parseGetOpt "get a release tar",
        cmd "tidy" parseTidyOpt "tidy the module dependency",
        cmd "version" parseVersionOpt "show version info"
      ]

cmd :: String -> Parser a -> String -> Mod CommandFields a
cmd name parser desc =
  command name (info (helper <*> parser) (progDesc desc))

parseBuildOpt :: Parser Command
parseBuildOpt = do
  mTarget <- optional $ argument str $ mconcat [metavar "TARGET", help "The build target"]
  mClangOpt <- optional $ strOption $ mconcat [long "clang-option", metavar "OPT", help "Options for clang"]
  logCfg <- logConfigOpt
  shouldCancelAlloc <- cancelAllocOpt
  outputKindList <- outputKindListOpt
  shouldSkipLink <- shouldSkipLinkOpt
  shouldExecute <- shouldExecuteOpt
  pure $
    Build $
      Build.Config
        { Build.mTarget = Target <$> mTarget,
          Build.mClangOptString = mClangOpt,
          Build.logCfg = logCfg,
          Build.shouldCancelAlloc = shouldCancelAlloc,
          Build.outputKindList = outputKindList,
          Build.shouldSkipLink = shouldSkipLink,
          Build.shouldExecute = shouldExecute
        }

parseCleanOpt :: Parser Command
parseCleanOpt = do
  logCfg <- logConfigOpt
  pure $
    Clean $
      Clean.Config
        { Clean.logCfg = logCfg
        }

parseGetOpt :: Parser Command
parseGetOpt = do
  moduleAlias <- argument str (mconcat [metavar "ALIAS", help "The alias of the module"])
  moduleURL <- argument str (mconcat [metavar "URL", help "The URL of the archive"])
  logCfg <- logConfigOpt
  pure $
    Get $
      Get.Config
        { Get.moduleAliasText = T.pack moduleAlias,
          Get.moduleURL = ModuleURL $ T.pack moduleURL,
          Get.logCfg = logCfg
        }

parseTidyOpt :: Parser Command
parseTidyOpt = do
  logCfg <- logConfigOpt
  pure $
    Tidy $
      Tidy.Config
        { Tidy.logCfg = logCfg
        }

parseCreateOpt :: Parser Command
parseCreateOpt = do
  moduleName <- argument str (mconcat [metavar "MODULE", help "The name of the module"])
  logCfg <- logConfigOpt
  pure $
    Create $
      Create.Config
        { Create.moduleName = T.pack moduleName,
          Create.logCfg = logCfg
        }

parseVersionOpt :: Parser Command
parseVersionOpt =
  pure $
    ShowVersion $
      Version.Config {}

parseCheckOpt :: Parser Command
parseCheckOpt = do
  inputFilePath <- optional $ argument str (mconcat [metavar "INPUT", help "The path of input file"])
  logCfg <- logConfigOpt
  pure $
    Check $
      Check.Config
        { Check.mFilePathString = inputFilePath,
          Check.logCfg = logCfg
        }

parseReleaseOpt :: Parser Command
parseReleaseOpt = do
  releaseName <- argument str (mconcat [metavar "NAME", help "The name of the release"])
  logCfg <- logConfigOpt
  pure $
    Release $
      Release.Config
        { Release.getReleaseName = releaseName,
          Release.logCfg = logCfg
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

outputKindListOpt :: Parser [OK.OutputKind]
outputKindListOpt = do
  option outputKindListReader $ mconcat [long "emit", metavar "EMIT", help "llvm, asm, or object", value [OK.Object]]

outputKindListReader :: ReadM [OK.OutputKind]
outputKindListReader =
  eitherReader $ \input ->
    readOutputKinds $ T.splitOn "," $ T.pack input

readOutputKinds :: [T.Text] -> Either String [OK.OutputKind]
readOutputKinds kindStrList =
  case kindStrList of
    [] ->
      return []
    kindStr : rest -> do
      tmp <- readOutputKinds rest
      case kindStr of
        "llvm" ->
          return $ OK.LLVM : tmp
        "asm" ->
          return $ OK.Asm : tmp
        "object" ->
          return $ OK.Object : tmp
        _ ->
          Left $ T.unpack $ "no such output kind exists: " <> kindStr

shouldSkipLinkOpt :: Parser Bool
shouldSkipLinkOpt =
  flag
    False
    True
    ( mconcat
        [ long "skip-link",
          help "Set this to skip linking"
        ]
    )

shouldExecuteOpt :: Parser Bool
shouldExecuteOpt =
  flag
    False
    True
    ( mconcat
        [ long "execute",
          help "Run the executable after compilation"
        ]
    )

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
