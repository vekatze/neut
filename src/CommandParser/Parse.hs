module CommandParser.Parse (run) where

import CommandParser.Command
import CommandParser.Config.Archive qualified as Archive
import CommandParser.Config.Build qualified as Build
import CommandParser.Config.Check qualified as Check
import CommandParser.Config.Clean qualified as Clean
import CommandParser.Config.Create qualified as Create
import CommandParser.Config.Format qualified as Format
import CommandParser.Config.Get qualified as Get
import CommandParser.Config.Remark qualified as Remark
import CommandParser.Config.Version qualified as Version
import CommandParser.Config.Zen qualified as Zen
import Console.FormatMode qualified as FormatMode
import Console.ReportMode qualified as ReportMode
import Data.Text qualified as T
import Options.Applicative

run :: IO Command
run =
  execParser (info (helper <*> parseOpt) fullDesc)

parseOpt :: Parser Command
parseOpt = do
  subparser $
    mconcat
      [ cmd "build" parseBuildOpt "build given target",
        cmd "clean" parseCleanOpt "remove the resulting files",
        cmd "check" parseCheckOpt "type-check all the files in the current module",
        cmd "archive" parseArchiveOpt "package a tarball",
        cmd "create" parseCreateOpt "create a new module",
        cmd "get" parseGetOpt "add or update a dependency",
        cmd "format" parseFormatOpt "format a source file",
        cmd "zen" parseZenOpt "execute `zen` of given file",
        cmd "lsp" parseLSPOpt "start the LSP server",
        cmd "version" parseVersionOpt "show version info"
      ]

cmd :: String -> Parser a -> String -> Mod CommandFields a
cmd name parser desc =
  command name (info (helper <*> parser) (progDesc desc))

parseBuildOpt :: Parser Command
parseBuildOpt = do
  targetName <- argument str $ mconcat [metavar "TARGET", help "The build target"]
  installDir <- optional $ strOption $ mconcat [long "install", metavar "DIRECTORY", help "Install the resulting binary to this directory"]
  remarkCfg <- remarkConfigOpt
  outputKindTextList <- outputKindTextListOpt
  shouldSkipLink <- shouldSkipLinkOpt
  shouldExecute <- shouldExecuteOpt
  rest <- (many . strArgument) (metavar "args")
  pure $
    Internal remarkCfg $
      Build $
        Build.Config
          { Build.targetName = targetName,
            Build.outputKindTextList = outputKindTextList,
            Build.shouldSkipLink = shouldSkipLink,
            Build.shouldExecute = shouldExecute,
            Build.installDir = installDir,
            Build.args = rest
          }

parseCleanOpt :: Parser Command
parseCleanOpt = do
  remarkCfg <- remarkConfigOpt
  pure $
    Internal remarkCfg $
      Clean $
        Clean.Config {}

parseGetOpt :: Parser Command
parseGetOpt = do
  moduleAlias <- argument str (mconcat [metavar "ALIAS", help "The alias of the module"])
  moduleURLText <- argument str (mconcat [metavar "URL", help "The URL of the archive"])
  remarkCfg <- remarkConfigOpt
  pure $
    Internal remarkCfg $
      Get $
        Get.Config
          { Get.moduleAliasText = T.pack moduleAlias,
            Get.moduleURLText = T.pack moduleURLText
          }

parseZenOpt :: Parser Command
parseZenOpt = do
  inputFilePath <- argument str (mconcat [metavar "INPUT", help "The path of input file"])
  remarkCfg <- remarkConfigOpt
  rest <- (many . strArgument) (metavar "args")
  pure $
    Internal remarkCfg $
      CommandParser.Command.Zen $
        Zen.Config
          { Zen.filePathString = inputFilePath,
            Zen.args = rest
          }

parseLSPOpt :: Parser Command
parseLSPOpt = do
  remarkCfg <- remarkConfigOpt
  pure $ External remarkCfg LSP

parseCreateOpt :: Parser Command
parseCreateOpt = do
  moduleName <- argument str (mconcat [metavar "MODULE", help "The name of the module"])
  targetName <-
    optional $
      strOption $
        mconcat
          [ long "target",
            metavar "TARGET_NAME",
            help "The name of the target"
          ]
  remarkCfg <- remarkConfigOpt
  pure $
    External remarkCfg $
      Create $
        Create.Config
          { Create.moduleName = T.pack moduleName,
            Create.targetName = targetName
          }

parseVersionOpt :: Parser Command
parseVersionOpt = do
  remarkCfg <- remarkConfigOpt
  pure $
    External remarkCfg $
      ShowVersion $
        Version.Config {}

parseCheckOpt :: Parser Command
parseCheckOpt = do
  shouldCheckAllDependencies <- flag False True (mconcat [long "full", help "Set this to refresh the caches of all the dependencies"])
  remarkCfg <- remarkConfigOpt
  pure $
    Internal remarkCfg $
      Check $
        Check.Config
          { Check.shouldCheckAllDependencies = shouldCheckAllDependencies
          }

parseArchiveOpt :: Parser Command
parseArchiveOpt = do
  archiveName <- optional $ argument str (mconcat [metavar "NAME", help "The name of the archive"])
  remarkCfg <- remarkConfigOpt
  pure $
    Internal remarkCfg $
      Archive $
        Archive.Config
          { Archive.getArchiveName = archiveName
          }

parseFormatOpt :: Parser Command
parseFormatOpt = do
  inputFilePathList <- many (argument str (mconcat [metavar "INPUT ...", help "Input files or directories"]))
  formatModeValue <- formatModeOpt
  stdinFilePathValue <- stdinFilePathOpt
  shouldMinimizeImports <- flag False True (mconcat [long "minimize-imports", help "Set this to remove unused items in `import {..}`"])
  remarkCfg <- remarkConfigOpt
  pure $
    Internal remarkCfg $
      Format $
        Format.Config
          { Format.filePathStringList = inputFilePathList,
            Format.formatMode = formatModeValue,
            Format.stdinFilePath = stdinFilePathValue,
            Format.shouldMinimizeImports = shouldMinimizeImports
          }

formatModeOpt :: Parser FormatMode.FormatMode
formatModeOpt = do
  option formatModeReader $
    mconcat
      [ short 'm',
        long "mode",
        metavar "MODE",
        help "Set this to specify the behavior of the command (check, stdout, write)",
        value FormatMode.Check
      ]

formatModeReader :: ReadM FormatMode.FormatMode
formatModeReader =
  eitherReader $ \input ->
    case input of
      "check" ->
        return FormatMode.Check
      "stdout" ->
        return FormatMode.Stdout
      "write" ->
        return FormatMode.Write
      _ ->
        Left "MODE must be one of: check, stdout, write"

stdinFilePathOpt :: Parser (Maybe FilePath)
stdinFilePathOpt =
  optional $
    strOption $
      mconcat
        [ long "stdin",
          metavar "FILEPATH",
          help "Set this to read the content of FILEPATH from stdin instead of the actual file, and print the formatted result to stdout"
        ]

remarkConfigOpt :: Parser Remark.Config
remarkConfigOpt = do
  shouldColorize <- colorizeOpt
  reportMode <- reportModeOpt
  pure
    Remark.Config
      { Remark.shouldColorize = shouldColorize,
        Remark.reportMode = reportMode
      }

reportModeOpt :: Parser (Maybe ReportMode.ReportMode)
reportModeOpt = do
  option reportModeReader $
    mconcat
      [ long "report",
        metavar "MODE",
        help "Set report mode (none, plain, fancy, trace=ITEMS)",
        value Nothing
      ]

reportModeReader :: ReadM (Maybe ReportMode.ReportMode)
reportModeReader =
  eitherReader $ \input -> do
  case input of
      "none" ->
        return $ Just ReportMode.NoReport
      "plain" ->
        return $ Just ReportMode.PlainReport
      "fancy" ->
        return $ Just ReportMode.FancyReport
      _ ->
        case T.stripPrefix "trace=" $ T.pack input of
          Just traceText ->
            case ReportMode.parseTraceConfig traceText of
              Left err ->
                Left $ T.unpack err
              Right traceConfig ->
                return $ Just $ ReportMode.TraceReport traceConfig
          Nothing ->
            Left "MODE must be one of: none, plain, fancy, trace=ITEMS"

outputKindTextListOpt :: Parser [T.Text]
outputKindTextListOpt = do
  option outputKindTextListReader $
    mconcat [long "emit", metavar "EMIT", help "EMIT == (llvm || object)", value ["object"]]

outputKindTextListReader :: ReadM [T.Text]
outputKindTextListReader =
  eitherReader $ \input ->
    return $ T.splitOn "," $ T.pack input

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
