module Main (main) where

import Color.CreateHandle qualified as Color
import Command.Archive.Archive qualified as Archive
import Command.Build.Build qualified as Build
import Command.Check.Check qualified as Check
import Command.Clean.Clean qualified as Clean
import Command.Common.SaveModule qualified as SaveModule
import Command.Create.Create qualified as Create
import Command.FormatEns.FormatEns qualified as FormatEns
import Command.FormatSource.FormatSource qualified as FormatSource
import Command.Get.Get qualified as Get
import Command.LSP.MoveLSP qualified as LSP
import Command.Version.Version qualified as Version
import Command.Zen.Zen qualified as Zen
import CommandParser.Command qualified as C
import CommandParser.Config.Remark qualified as Remark
import CommandParser.Parse qualified as CommandParser
import Control.Monad.IO.Class (MonadIO (liftIO))
import Error.Run (run)
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Platform (ensureExecutables)
import Logger.CreateHandle qualified as Logger
import System.IO

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  userCommand <- liftIO CommandParser.run
  case userCommand of
    C.External loggerConfig cmd -> do
      let shouldColorize = Remark.shouldColorize loggerConfig
      let enableDebugMode = Remark.enableDebugMode loggerConfig
      colorHandle <- Color.createHandle shouldColorize shouldColorize
      loggerHandle <- Logger.createHandle colorHandle enableDebugMode
      run loggerHandle $ do
        case cmd of
          C.Create cfg -> do
            let saveModuleHandle = SaveModule.new loggerHandle
            createHandle <- liftIO $ Create.new loggerConfig loggerHandle saveModuleHandle
            Create.create createHandle cfg
          C.ShowVersion cfg ->
            liftIO $ Version.showVersion cfg
    C.Internal loggerConfig cmd -> do
      h <- Global.new loggerConfig Nothing
      run (Global.loggerHandle h) $ do
        ensureExecutables
        case cmd of
          C.Build cfg -> do
            Build.build (Build.new h) cfg
          C.Check cfg -> do
            Check.check (Check.new h) cfg
          C.Clean _ -> do
            cleanHandle <- liftIO $ Clean.new h
            Clean.clean cleanHandle
          C.Archive cfg -> do
            Archive.archive (Archive.new h) cfg
          C.Get cfg -> do
            getHandle <- liftIO $ Get.new h loggerConfig
            Get.get getHandle cfg
          C.FormatSource cfg -> do
            FormatSource.format (FormatSource.new h) cfg
          C.FormatEns cfg -> do
            FormatEns.format cfg
          C.LSP -> do
            LSP.lsp (LSP.new h)
          C.Zen cfg -> do
            Zen.zen (Zen.new h cfg) cfg
