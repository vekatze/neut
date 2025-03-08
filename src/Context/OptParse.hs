module Context.OptParse (parseCommand) where

import Context.App
import Control.Monad.IO.Class
import Data.Text qualified as T
import Entity.BuildMode qualified as BM
import Entity.Command
import Entity.Config.Archive qualified as Archive
import Entity.Config.Build qualified as Build
import Entity.Config.Check qualified as Check
import Entity.Config.Clean qualified as Clean
import Entity.Config.Create qualified as Create
import Entity.Config.Format qualified as Format
import Entity.Config.Get qualified as Get
import Entity.Config.Remark qualified as Remark
import Entity.Config.Version qualified as Version
import Entity.Config.Zen qualified as Zen
import Entity.FileType qualified as FT
import Entity.ModuleURL
import Entity.OutputKind qualified as OK
import Options.Applicative

parseCommand :: App Command
parseCommand =
  liftIO $
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
        cmd "format-source" (parseFormatOpt FT.Source) "format a source file",
        cmd "format-ens" (parseFormatOpt FT.Ens) "format an ens file",
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
  buildMode <- option buildModeReader $ mconcat [long "mode", metavar "MODE", help "develop, release", value BM.Develop]
  remarkCfg <- remarkConfigOpt
  outputKindList <- outputKindListOpt
  shouldSkipLink <- shouldSkipLinkOpt
  shouldExecute <- shouldExecuteOpt
  rest <- (many . strArgument) (metavar "args")
  pure $
    Build $
      Build.Config
        { Build.targetName = targetName,
          Build.remarkCfg = remarkCfg,
          Build.outputKindList = outputKindList,
          Build.shouldSkipLink = shouldSkipLink,
          Build.shouldExecute = shouldExecute,
          Build.installDir = installDir,
          Build.buildMode = buildMode,
          Build.args = rest
        }

parseCleanOpt :: Parser Command
parseCleanOpt = do
  remarkCfg <- remarkConfigOpt
  pure $
    Clean $
      Clean.Config
        { Clean.remarkCfg = remarkCfg
        }

parseGetOpt :: Parser Command
parseGetOpt = do
  moduleAlias <- argument str (mconcat [metavar "ALIAS", help "The alias of the module"])
  moduleURL <- argument str (mconcat [metavar "URL", help "The URL of the archive"])
  remarkCfg <- remarkConfigOpt
  pure $
    Get $
      Get.Config
        { Get.moduleAliasText = T.pack moduleAlias,
          Get.moduleURL = ModuleURL $ T.pack moduleURL,
          Get.remarkCfg = remarkCfg
        }

parseZenOpt :: Parser Command
parseZenOpt = do
  inputFilePath <- argument str (mconcat [metavar "INPUT", help "The path of input file"])
  remarkCfg <- remarkConfigOpt
  buildMode <- option buildModeReader $ mconcat [long "mode", metavar "MODE", help "develop, release", value BM.Develop]
  rest <- (many . strArgument) (metavar "args")
  pure $
    Entity.Command.Zen $
      Zen.Config
        { Zen.filePathString = inputFilePath,
          Zen.remarkCfg = remarkCfg,
          Zen.buildMode = buildMode,
          Zen.args = rest
        }

parseLSPOpt :: Parser Command
parseLSPOpt =
  pure LSP

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
    Create $
      Create.Config
        { Create.moduleName = T.pack moduleName,
          Create.targetName = targetName,
          Create.remarkCfg = remarkCfg
        }

parseVersionOpt :: Parser Command
parseVersionOpt =
  pure $
    ShowVersion $
      Version.Config {}

parseCheckOpt :: Parser Command
parseCheckOpt = do
  padOpt <- flag True False (mconcat [long "no-padding", help "Set this to disable padding of the output"])
  shouldCheckAllDependencies <- flag False True (mconcat [long "full", help "Set this to refresh the caches of all the dependencies"])
  remarkCfg <- remarkConfigOpt
  pure $
    Check $
      Check.Config
        { Check.shouldInsertPadding = padOpt,
          Check.shouldCheckAllDependencies = shouldCheckAllDependencies,
          Check.remarkCfg = remarkCfg
        }

parseArchiveOpt :: Parser Command
parseArchiveOpt = do
  archiveName <- optional $ argument str (mconcat [metavar "NAME", help "The name of the archive"])
  remarkCfg <- remarkConfigOpt
  pure $
    Archive $
      Archive.Config
        { Archive.getArchiveName = archiveName,
          Archive.remarkCfg = remarkCfg
        }

parseFormatOpt :: FT.FileType -> Parser Command
parseFormatOpt fileType = do
  inputFilePath <- argument str (mconcat [metavar "INPUT", help "The path of input file"])
  inPlaceOpt <- flag False True (mconcat [long "in-place", help "Set this to perform in-place update"])
  shouldMinimizeImports <- flag False True (mconcat [long "minimize-imports", help "Set this to remove unused items in `import {..}`"])
  remarkCfg <- remarkConfigOpt
  pure $
    Format $
      Format.Config
        { Format.remarkCfg = remarkCfg,
          Format.filePathString = inputFilePath,
          Format.mustUpdateInPlace = inPlaceOpt,
          Format.shouldMinimizeImports = shouldMinimizeImports,
          Format.inputFileType = fileType
        }

remarkConfigOpt :: Parser Remark.Config
remarkConfigOpt = do
  shouldColorize <- colorizeOpt
  eoe <- T.pack <$> endOfEntryOpt
  enableDebugMode <- flag False True (mconcat [long "enable-debug-output", help "Set this to print debug info"])
  pure
    Remark.Config
      { Remark.shouldColorize = shouldColorize,
        Remark.enableDebugMode = enableDebugMode,
        Remark.endOfEntry = eoe
      }

outputKindListOpt :: Parser [OK.OutputKind]
outputKindListOpt = do
  option outputKindListReader $ mconcat [long "emit", metavar "EMIT", help "EMIT == (llvm || object)", value [OK.Object]]

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
        "object" ->
          return $ OK.Object : tmp
        _ ->
          Left $ T.unpack $ "no such output kind exists: " <> kindStr

buildModeReader :: ReadM BM.BuildMode
buildModeReader =
  eitherReader readBuildMode

readBuildMode :: String -> Either String BM.BuildMode
readBuildMode input = do
  case input of
    "develop" ->
      return BM.Develop
    "release" ->
      return BM.Release
    _ ->
      Left $ T.unpack $ "no such build mode exists: " <> T.pack input

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
