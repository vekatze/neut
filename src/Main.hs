module Main (main) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Act.Archive qualified as Archive
import Move.Act.Build qualified as Build
import Move.Act.Check qualified as Check
import Move.Act.Clean qualified as Clean
import Move.Act.Create qualified as Create
import Move.Act.Format qualified as Format
import Move.Act.Get qualified as Get
import Move.Act.LSP qualified as LSP
import Move.Act.Version qualified as Version
import Move.Act.Zen qualified as Zen
import Move.Console.EnsureExecutables (ensureExecutables)
import Move.Console.Report qualified as Report
import Move.Context.Color qualified as Color
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (run)
import Move.Scene.Init.Base qualified as Base
import Move.Scene.Module.Save qualified as ModuleSave
import Move.Scene.OptParse qualified as OptParse
import Rule.Command qualified as C
import Rule.Config.Remark qualified as Remark
import System.IO

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  userCommand <- liftIO OptParse.parseCommand
  case userCommand of
    C.External loggerConfig cmd -> do
      colorHandle <- Color.new (Remark.shouldColorize loggerConfig)
      debugHandle <- Debug.new colorHandle (Remark.enableDebugMode loggerConfig)
      let reportHandle = Report.new colorHandle (Remark.endOfEntry loggerConfig)
      run reportHandle $ do
        case cmd of
          C.Create cfg -> do
            let moduleSaveHandle = ModuleSave.new debugHandle
            createHandle <- liftIO $ Create.new loggerConfig reportHandle debugHandle moduleSaveHandle
            Create.create createHandle cfg
          C.ShowVersion cfg ->
            liftIO $ Version.showVersion cfg
    C.Internal loggerConfig cmd -> do
      h <- Base.new loggerConfig Nothing
      run (Base.reportHandle h) $ do
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
          C.Format cfg -> do
            Format.format (Format.new h) cfg
          C.LSP -> do
            LSP.lsp (LSP.new h)
          C.Zen cfg -> do
            Zen.zen (Zen.new h cfg) cfg
