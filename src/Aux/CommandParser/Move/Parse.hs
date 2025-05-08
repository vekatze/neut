module Aux.CommandParser.Move.Parse (run) where

import Aux.CommandParser.Rule.Command
import Aux.CommandParser.Rule.Config.Archive qualified as Archive
import Aux.CommandParser.Rule.Config.Build qualified as Build
import Aux.CommandParser.Rule.Config.Check qualified as Check
import Aux.CommandParser.Rule.Config.Clean qualified as Clean
import Aux.CommandParser.Rule.Config.Create qualified as Create
import Aux.CommandParser.Rule.Config.FormatEns qualified as FormatEns
import Aux.CommandParser.Rule.Config.FormatSource qualified as FormatSource
import Aux.CommandParser.Rule.Config.Get qualified as Get
import Aux.CommandParser.Rule.Config.Remark qualified as Remark
import Aux.CommandParser.Rule.Config.Version qualified as Version
import Aux.CommandParser.Rule.Config.Zen qualified as Zen
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
        cmd "format-source" parseFormatSourceOpt "format a source file",
        cmd "format-ens" parseFormatEnsOpt "format an ens file",
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
  buildModeString <- parseBuildModeOpt
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
            Build.buildModeString = buildModeString,
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
  buildModeString <- parseBuildModeOpt
  rest <- (many . strArgument) (metavar "args")
  pure $
    Internal remarkCfg $
      Aux.CommandParser.Rule.Command.Zen $
        Zen.Config
          { Zen.filePathString = inputFilePath,
            Zen.buildModeString = buildModeString,
            Zen.args = rest
          }

parseLSPOpt :: Parser Command
parseLSPOpt = do
  remarkCfg <- remarkConfigOpt
  pure $ Internal remarkCfg LSP

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
  padOpt <- flag True False (mconcat [long "no-padding", help "Set this to disable padding of the output"])
  shouldCheckAllDependencies <- flag False True (mconcat [long "full", help "Set this to refresh the caches of all the dependencies"])
  remarkCfg <- remarkConfigOpt
  pure $
    Internal remarkCfg $
      Check $
        Check.Config
          { Check.shouldInsertPadding = padOpt,
            Check.shouldCheckAllDependencies = shouldCheckAllDependencies
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

parseFormatSourceOpt :: Parser Command
parseFormatSourceOpt = do
  inputFilePath <- argument str (mconcat [metavar "INPUT", help "The path of input file"])
  inPlaceOpt <- flag False True (mconcat [long "in-place", help "Set this to perform in-place update"])
  shouldMinimizeImports <- flag False True (mconcat [long "minimize-imports", help "Set this to remove unused items in `import {..}`"])
  remarkCfg <- remarkConfigOpt
  pure $
    Internal remarkCfg $
      FormatSource $
        FormatSource.Config
          { FormatSource.filePathString = inputFilePath,
            FormatSource.mustUpdateInPlace = inPlaceOpt,
            FormatSource.shouldMinimizeImports = shouldMinimizeImports
          }

parseFormatEnsOpt :: Parser Command
parseFormatEnsOpt = do
  inputFilePath <- argument str (mconcat [metavar "INPUT", help "The path of input file"])
  inPlaceOpt <- flag False True (mconcat [long "in-place", help "Set this to perform in-place update"])
  remarkCfg <- remarkConfigOpt
  pure $
    Internal remarkCfg $
      FormatEns $
        FormatEns.Config
          { FormatEns.filePathString = inputFilePath,
            FormatEns.mustUpdateInPlace = inPlaceOpt
          }

remarkConfigOpt :: Parser Remark.Config
remarkConfigOpt = do
  shouldColorize <- colorizeOpt
  eoe <- T.pack <$> endOfEntryOpt
  enableDebugMode <- flag False True (mconcat [long "enable-debug-output", help "Set this to print debug info"])
  enableSilentMode <- flag False True (mconcat [long "silent", help "Set this to enable silent mode"])
  pure
    Remark.Config
      { Remark.shouldColorize = shouldColorize,
        Remark.enableDebugMode = enableDebugMode,
        Remark.enableSilentMode = enableSilentMode,
        Remark.endOfEntry = eoe
      }

outputKindTextListOpt :: Parser [T.Text]
outputKindTextListOpt = do
  option outputKindTextListReader $
    mconcat [long "emit", metavar "EMIT", help "EMIT == (llvm || object)", value ["object"]]

outputKindTextListReader :: ReadM [T.Text]
outputKindTextListReader =
  eitherReader $ \input ->
    return $ T.splitOn "," $ T.pack input

parseBuildModeOpt :: Parser String
parseBuildModeOpt = do
  strOption $ mconcat [long "mode", metavar "MODE", help "develop, release", value "develop"]

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
