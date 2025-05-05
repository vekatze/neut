module Main (main) where

import Color.Move.CreateHandle qualified as Color
import Control.Monad.IO.Class (MonadIO (liftIO))
import Error.Move.Run (run)
import Logger.Move.CreateHandle qualified as Logger
import Main.Move.Act.Archive qualified as Archive
import Main.Move.Act.Build qualified as Build
import Main.Move.Act.Check qualified as Check
import Main.Move.Act.Clean qualified as Clean
import Main.Move.Act.Create qualified as Create
import Main.Move.Act.Format qualified as Format
import Main.Move.Act.Get qualified as Get
import Main.Move.Act.LSP qualified as LSP
import Main.Move.Act.Version qualified as Version
import Main.Move.Act.Zen qualified as Zen
import Main.Move.Context.Platform (ensureExecutables)
import Main.Move.Scene.Init.Base qualified as Base
import Main.Move.Scene.Module.Save qualified as ModuleSave
import Main.Move.Scene.OptParse qualified as OptParse
import Main.Rule.Command qualified as C
import Main.Rule.Config.Remark qualified as Remark
import System.IO

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  userCommand <- liftIO OptParse.parseCommand
  case userCommand of
    C.External loggerConfig cmd -> do
      let shouldColorize = Remark.shouldColorize loggerConfig
      let enableDebugMode = Remark.enableDebugMode loggerConfig
      colorHandle <- Color.createHandle shouldColorize shouldColorize
      loggerHandle <- Logger.createHandle colorHandle (Remark.endOfEntry loggerConfig) enableDebugMode
      run loggerHandle $ do
        case cmd of
          C.Create cfg -> do
            let moduleSaveHandle = ModuleSave.new loggerHandle
            createHandle <- liftIO $ Create.new loggerConfig loggerHandle moduleSaveHandle
            Create.create createHandle cfg
          C.ShowVersion cfg ->
            liftIO $ Version.showVersion cfg
    C.Internal loggerConfig cmd -> do
      h <- Base.new loggerConfig Nothing
      run (Base.loggerHandle h) $ do
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
